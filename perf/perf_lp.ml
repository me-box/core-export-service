#!/bin/ocaml

#use "topfind";;
#require "lwt";;
#require "cohttp.lwt";;
#require "cohttp.lwt-core";;
#require "macaroons.sodium";;
#require "ezjsonm";;

open Lwt

module Client = Cohttp_lwt_unix.Client
module M      = Sodium_macaroons


let exp_m =
  let m = M.create ~location:"local" ~id:"lp-perf-client" ~key:"perfsecret" in
  let m = M.add_first_party_caveat m "target = databox-export-service" in
  let m = M.add_first_party_caveat m "method = post" in
  let m = M.add_first_party_caveat m "path = /lp/export" in
  let m = M.add_first_party_caveat m "destination = http://127.0.0.1:8088/export" in
  M.serialize m

let dmp_m =
  let m = M.create ~location:"local" ~id:"lp-perf-client" ~key:"perfsecret" in
  let m = M.add_first_party_caveat m "target = databox-export-service" in
  let m = M.add_first_party_caveat m "method = get" in
  let m = M.add_first_party_caveat m "path = /dump" in
  M.serialize m


let gen_body id payload =
  let data =
    if payload = 0 then `O []
    else `O ["v", `String (Bytes.make payload 'v')]
  in
  let obj = `O [
      "id", `String id;
      "uri", `String "http://127.0.0.1:8088/export";
      "data", `String (Ezjsonm.to_string data)] in
  Ezjsonm.to_string obj


let issue_requests cnt payload =
  let uri = Uri.of_string "http://127.0.0.1:8080/lp/export" in
  let fail = ref 0 in
  let request i =
    let rec aux b =
      let body = Cohttp_lwt_body.of_string b in
      let headers = Cohttp.Header.init_with "x-api-key" exp_m in
      Client.post ~body ~headers uri >>= fun (resp, body) ->
      let code =
        let s = Cohttp.Response.status resp in
        Cohttp.Code.code_of_status s
      in
      if not @@ Cohttp.Code.is_success code then return (incr fail)
      else begin
        Cohttp_lwt_body.to_string body >>= fun b ->
        Ezjsonm.(
          from_string b
          |> value
          |> get_dict
          |> fun dic ->
          let id = List.assoc "id" dic |> get_string in
          let state = List.assoc "state" dic |> get_string in
          if state <> "Finished" then aux (gen_body id 0)
          else return_unit)
      end in
    Printf.printf "[client] request %d\n%!" i;
    aux (gen_body "" payload)
  in
  let gen_requests cnt =
    let rec aux i acc =
      if i = cnt then acc
      else aux (i + 1) (request i :: acc)
    in
    aux 0 []
  in
  Lwt.join (gen_requests cnt) >>= fun () ->
  Printf.printf "%d/%d success!\n%!" (cnt - !fail) cnt;
  return_unit


let dump_stats () =
  let uri = Uri.of_string "http://127.0.0.1:8080/dump" in
  let headers = Cohttp.Header.init_with "x-api-key" dmp_m in
  Client.get ~headers uri >>= fun (resp, _) ->
  let c =
    let s = Cohttp.Response.status resp in
    Cohttp.Code.code_of_status s
  in
  let r = if Cohttp.Code.is_success c then "success" else "fail" in
  Printf.printf "dump stats: %s\n%!" r;
  return_unit


let () =
  Printf.printf "argv: %s\n%!" (String.concat " " (Array.to_list Sys.argv));
  let cnt = try Sys.argv.(1) |> int_of_string with _ -> 10 in
  let payload = try Sys.argv.(2) |> int_of_string with _ -> 0 in
  Printf.printf "issuing %d requests, each with %d bytes payload:\n%!" cnt payload;
  (issue_requests cnt payload >>= dump_stats)
  |> Lwt_main.run
