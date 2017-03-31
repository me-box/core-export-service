open Lwt

module Client = Cohttp_lwt_unix.Client
module C      = Test_common

include C.Constants

let server_port = 8080
let server () =
  C.set_environment ();
  let port = server_port in
  Logs_lwt.info (fun m -> m "[test] ws server running...") >>= fun () ->
  Lwt.join [C.start (); Export.ws ~port ()]


let get_macaroon () =
  let uri =
    let root = Uri.of_string arbiter_endp in
    Uri.with_path root "/ws/token"
  in
  Client.get uri >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun m ->
  let status =
    Cohttp.Response.status resp
    |> Cohttp.Code.code_of_status
  in
  if Cohttp.Code.is_success status then return m
  else return ""


let _macaroon = ref ""
let macaroon () =
  match !_macaroon with
  | "" ->
      get_macaroon () >>= fun ma ->
      return ma
  | _ as ma -> return ma


let slow_uri = "http://127.0.0.1:" ^ local_echo_slow_port
let normal_uri = "http://127.0.0.1:" ^ local_echo_port


let make_frame ?(uri = slow_uri) id =
  let data = `O ["key", `String "KEY0"; "value", `A [`String "V0"; `String "V1"]] in
  let open Ezjsonm in
  let obj = `O [
      "id", `String id;
      "uri", `String uri;
      "data", `String (to_string data)]
  in
  let content = to_string obj in
  Websocket_lwt.Frame.create ~content ()


let client () =
  let ctx = Conduit_lwt_unix.default_ctx in
  let client = `TCP (`IP Ipaddr.(V4 V4.localhost), `Port server_port) in
  let uri =
    let root = Uri.of_string "ws://127.0.0.1" in
    let uri' = Uri.with_port root @@ Some server_port in
    Uri.with_path uri' "/ws/export"
  in

  macaroon () >>= fun m ->
  let extra_headers = Cohttp.Header.of_list ["x-api-key", m] in
  Websocket_lwt.with_connection ~extra_headers ~ctx client uri
  >>= fun (recv, send) ->

  let open Websocket_lwt.Frame in
  (* expects `cnt' finished responses *)
  let react cnt =
    let rec aux cnt =
      recv () >>= fun fr ->
      match fr.opcode with
      | Opcode.Text ->
          Logs_lwt.info (fun m -> m "[test] recv: %s" fr.content) >>= fun () ->
          let open Ezjsonm in
          let resp = from_string fr.content |> value |> get_dict in
          let state = List.assoc "state" resp |> get_string in
          if String.lowercase_ascii state = "finished" then
            if cnt = 1 then return_unit
            else aux @@ pred cnt
          else aux cnt
      | _ ->
          Logs_lwt.warn (fun m -> m "[test] unexpected frame: %a" pp fr)
          >>= fun () -> aux cnt
    in
    aux cnt
  in

  let actions () =
    send @@ make_frame "" >>= fun () ->
    Lwt_unix.sleep 1.5 >>= fun () ->
    send @@ make_frame ""
  in
  Lwt.join [react 2; actions ()] >>= fun () ->

  send @@ create ~opcode:Opcode.Close () >>= fun () ->
  recv () >>= fun fr ->
  Logs_lwt.info (fun m -> m "[test] should be CLOSE frame: %a" pp fr)


let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Debug));
  match Lwt_unix.fork () with
  | 0 -> begin
      Unix.sleepf 2.5;
      try
        let t = Lwt.join [
            client ();
            Lwt_unix.sleep 1.0 >>= client;
            Lwt_unix.sleep 1.5 >>= client]
        in
        Lwt_main.run @@ t;
        Logs.info (fun m -> m "[client] OK!")
      with _ ->
        Logs.err (fun m -> m "[client] ERROR!");
        exit 1 end
  | pid ->
      let wait () =
        Lwt_unix.wait () >>= fun (cpid, status) ->
        assert (pid = cpid);
        if status = Unix.(WEXITED 0) then return_unit
        else Lwt.fail_with "client"
      in
      let wait () =
        wait () >>= fun () -> Lwt_unix.sleep 2.5
      in
      let t = wait () <?> server () in
      try
        Lwt_main.run t;
        Logs.info (fun m -> m "[server] OK!")
      with
      | Failure f when f = "client" ->
          Logs.info (fun m -> m "[server] OK!"); exit 1
      | _ ->
          Logs.err (fun m -> m "[server] ERROR!"); exit 1
