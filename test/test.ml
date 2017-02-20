open Opium.Std

module Macaroon = Sodium_macaroons

let arbiter_endp    = "http://localhost:8888"
let arbiter_token   = "Believe it or not, I'm an arbiter token"
let macaroon_secret = "Am I a secret, or not, or whatever?"
let export_service  = "data_export_service"

let set_environment () =
  Unix.putenv "DATABOX_LOCAL_NAME" export_service;
  Unix.putenv "DATABOX_LOCAL_PORT" "8080";
  Unix.putenv "DATABOX_ARBITER_ENDPOINT" arbiter_endp;
  Unix.putenv "ARBITER_TOKEN" arbiter_token


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
  Lwt.join [arbiter; Export.t]
  |> Lwt_main.run


let client () = ()


let () =
  match Unix.fork () with
  | 0 -> client ()
  | pid -> server ()
