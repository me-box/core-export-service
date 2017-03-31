open Lwt
open Opium.Std

module Macaroon = Sodium_macaroons

module Constants = struct
  let arbiter_endp         = "http://127.0.0.1:8888"
  let arbiter_token        = "Believe it or not, I'm an arbiter token"
  let macaroon_secret      = "Am I a secret, or not, or whatever?"
  let export_service       = "data_export_service"
  let export_port          = "8080"
  let local_echo_port      = "8000"
  let local_echo_slow_port = "8008"
end

open Constants

let set_environment () =
  Unix.putenv "DATABOX_LOCAL_NAME" export_service;
  Unix.putenv "DATABOX_LOCAL_PORT" export_port;
  Unix.putenv "DATABOX_ARBITER_ENDPOINT" arbiter_endp;
  Unix.putenv "ARBITER_TOKEN" arbiter_token


let logging_mw service =
  let filter = fun handler req ->
    let meth =
      Request.meth req
      |> Cohttp.Code.string_of_method
    in
    let uri = Request.uri req in
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun b ->
    Logs_lwt.info (fun m ->
        m "[%s] %s http:%a %s" service meth Uri.pp_hum uri b) >>= fun () ->
    let b = Cohttp_lwt_body.of_string b in
    let req = Request.({req with body = b}) in
    handler req
  in
  Opium_rock.Middleware.create ~filter ~name:"logging request middleware"


let local_echo () =
  let echo = post "/" begin fun req ->
      let body = Request.body req in
      Cohttp_lwt_body.to_string body >>= fun b ->
      let v = Ezjsonm.(from_string b |> value) in
      let r = `O ["request", v] in
      let s = Ezjsonm.to_string r in
      `String s |> respond' end
  in
  let app =
    App.empty
    |> App.port (int_of_string local_echo_port)
    |> App.middleware (logging_mw "local_echo")
    |> echo
  in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let local_echo_slow () =
  let echo = post "/" begin fun req ->
      let body = Request.body req in
      Cohttp_lwt_body.to_string body >>= fun b ->
      let v = Ezjsonm.(from_string b |> value) in
      let r = `O ["request", v] in
      let s = Ezjsonm.to_string r in
      return @@`String s >>= fun resp ->
      Lwt_unix.sleep 3.0 >>= fun () ->
      Logs_lwt.info (fun m ->
          m "[echo_slow] responding with %s" s) >>= fun () ->
      respond' resp end
  in
  let app =
    App.empty
    |> App.port (int_of_string local_echo_slow_port)
    |> App.middleware (logging_mw "local_echo_slow")
    |> echo
  in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let mint_macaroon ?(id = "arbiter") ?(location = arbiter_endp) ?(key = macaroon_secret)
    ?(target = "target = " ^ export_service) ?(meth = "method = " ^ "POST") ~path () =
  let m = Macaroon.create ~id ~location ~key in
  let m = Macaroon.add_first_party_caveat m target in
  let m = Macaroon.add_first_party_caveat m path in
  let m = Macaroon.add_first_party_caveat m meth in
  Macaroon.serialize m


let arbiter () =
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
  let token_endp = get "/token" begin fun req ->
      let path = "path = /export" in
      let token = mint_macaroon ~path () in
      `String token |> respond'
    end in
  let invalid_routes_token_endp = get "/routes-token" begin fun req ->
      let invalid_path = "path = /exportttt" in
      let token = mint_macaroon ~path:invalid_path () in
      `String token |> respond'
    end in
  let lp_token_endp = get "/lp/token" begin fun req ->
      let path = "path = /lp/export" in
      let token = mint_macaroon ~path () in
      `String token |> respond'
    end in
  let ws_token_endp = get "/ws/token" begin fun req ->
      let path = "path = /ws/export" in
      let meth = "method = GET" in
      let token = mint_macaroon ~path ~meth () in
      `String token |> respond'
    end in
  let app =
    App.empty
    |> App.middleware (logging_mw "arbiter")
    |> App.port 8888
    |> secret_endp
    |> ws_token_endp
    |> lp_token_endp
    |> token_endp
    |> invalid_routes_token_endp in
  match App.run_command' app with
  | `Ok t -> t
  | _ -> assert false


let started = ref false

let start () =
  if !started then return_unit else begin
    started := true;
    Lwt.join [arbiter (); local_echo (); local_echo_slow ()]
  end
