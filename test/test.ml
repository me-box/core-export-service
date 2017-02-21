open Lwt
open Opium.Std

module Macaroon = Sodium_macaroons
module Client   = Cohttp_lwt_unix.Client


let arbiter_endp    = "http://localhost:8888"
let arbiter_token   = "Believe it or not, I'm an arbiter token"
let macaroon_secret = "Am I a secret, or not, or whatever?"
let export_service  = "data_export_service"
let export_port     = "8080"
let local_echo_port = "8000"


let set_environment () =
  Unix.putenv "DATABOX_LOCAL_NAME" export_service;
  Unix.putenv "DATABOX_LOCAL_PORT" export_port;
  Unix.putenv "DATABOX_ARBITER_ENDPOINT" arbiter_endp;
  Unix.putenv "ARBITER_TOKEN" arbiter_token


let local_echo =
  let echo = post "/" begin fun req ->
      let body = Request.body req in
      Cohttp_lwt_body.to_string body >>= fun b ->
      let r = `O ["request", `String b] in
      let s = Ezjsonm.to_string r in
      `String s |> respond' end
  in
  let app =
    App.empty
    |> echo
  in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let arbiter =
  let secret_endp = get "/store/secret" begin fun req ->
      let headers = Request.headers req in
      let api_key = Cohttp.Header.get headers "X-Api-Key" in
      if not (api_key = Some arbiter_token) then
        let code = `Unauthorized in
        `String "Missing/Invalid API key" |> respond' ~code
      else
      let s = B64.(encode ~alphabet:uri_safe_alphabet macaroon_secret) in
      `String s |> respond'
    end in
  let token_endp = post "/token" begin fun req ->
      let location = arbiter_endp in
      let key = macaroon_secret in
      let id = "fake arbiter" in
      let m = Macaroon.create ~location ~key ~id in
      let target = "target = " ^ export_service in
      let routes = Ezjsonm.(
          let l = `A [`String "/export"] in
          let d = `O ["POST", l] in
          let v = to_string d in
          "routes = " ^ v
        ) in
      let m = Macaroon.add_first_party_caveat m target in
      let m = Macaroon.add_first_party_caveat m routes in
      let s = Macaroon.serialize m in
      `String s |> respond'
    end in
  let app =
    App.empty
    |> App.port 8888
    |> secret_endp
    |> token_endp in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let server () =
  set_environment ();
  Lwt.join [arbiter; Export.t; local_echo]


let client () =
  let uri = Uri.of_string arbiter_endp in
  let uri = Uri.with_path uri "/token" in
  Client.get uri >>= fun (_, body)->
  Cohttp_lwt_body.to_string body >>= fun m ->

  let headers = Cohttp.Header.init_with "X-Api-Key" m in
  let uri = Uri.of_string ("http://localhost:" ^ export_port) in

  let make_body id =
    let obj = `O [
        "id",   `String id;
        "dest", `String ("http://localhost:" ^ local_echo_port);
        "data", `O ["key", `String "KEY0"; "value", `A [`String "V0"; `String "V1"]]]
    in
    obj
    |> Ezjsonm.to_string
    |> Cohttp_lwt_body.of_string
  in

  let get_field b field =
    let open Ezjsonm in
    let obj = value @@ from_string b in
    let dic = get_dict obj in
    get_string @@ List.assoc field dic
    |> return
  in

  Client.post ~body:(make_body "") ~headers uri >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun body ->

  let status = Cohttp.Response.status resp in
  let () = assert (status = `OK) in
  get_field body "id"    >>= fun id ->
  get_field body "state" >>= fun state ->
  let () = assert (state = "Pending") in

  let rec aux b = function
  | "Finished" ->
      get_field b "response" >>= fun response ->
      get_field response "request" >>= fun echo ->
      let dic = Ezjsonm.(
          echo |> from_string
          |> value
          |> get_dict)
      in
      let () = assert (List.mem_assoc "key" dic) in
      let () = assert (List.mem_assoc "value" dic) in
      let key = List.assoc "key" dic |> Ezjsonm.get_string in
      let values = List.assoc "value" dic |> Ezjsonm.(get_list get_string) in
      let () = assert (key = "KEY0") in
      let () = assert (values = ["V0"; "V1"]) in
      return_unit
  | _ ->
      Lwt_unix.sleep 0.5 >>= fun () ->
      let body = make_body id in
      Client.post ~body ~headers uri >>= fun (resp, body) ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      get_field body "state"         >>= fun state ->
      aux body state
  in
  aux body state



let () =
  match Unix.fork () with
  | 0 ->
      Unix.sleepf 2.5;
      Lwt_main.run @@ client ()
  | pid ->
      Lwt_main.run @@ server ()
