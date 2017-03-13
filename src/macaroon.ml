open Lwt
open Opium.Std

module Client   = Cohttp_lwt_unix.Client
module Macaroon = Sodium_macaroons
module R        = Rresult.R

let s = ref None

let get_secret () =
  let endp = Uri.of_string (Export_env.arbiter_endp ()) in
  let url = Uri.with_path endp "/store/secret" in

  let h = ["X-Api-Key", Export_env.arbiter_token ()] in
  let headers = Cohttp.Header.of_list h in

  Client.get ~headers url >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in

  if not (code = 200) then return_unit
  else
    Cohttp_lwt_body.to_string body >>= fun body ->
    let alphabet = B64.uri_safe_alphabet in
    s := Some (B64.decode ~alphabet body);
    return_unit

let init = get_secret

let secret () =
  match !s with
  | None ->  R.error_msg "Can't get macaroon secret"
  | Some s -> R.ok s


let verify_target caveat_str =
  let expected = "target = " ^ Export_env.local_name () in
  expected = caveat_str


let verify_method  meth caveat_str =
  let meth =
    meth
    |> Cohttp.Code.string_of_method
    |> String.lowercase_ascii
  in
  let expected = "method = " ^ meth in
  let caveat_str' = String.lowercase_ascii caveat_str in
  expected = caveat_str'

(* take advantage of export-service's simple API *)
(* pretty much the same as satisfyExact *)
let verify_path url caveat_str =
  let path = Uri.path url in
  let expected = "path = " ^ path in
  expected = caveat_str


(* let verify_path url meth caveat_str =
  try
    let prefix_len = String.length "routes = " in
    let prefix = String.sub caveat_str 0 prefix_len in

    if not (prefix = "routes = ") then false
    else
    let l = String.length caveat_str in
    let r = String.sub caveat_str prefix_len (l - prefix_len) in

    let wdic = Ezjsonm.(from_string r |> value |> get_dict) in
    let m = Cohttp.Code.string_of_method meth in
    let wl = List.assoc m wdic in
    let wl = Ezjsonm.(get_list get_string wl) in
    let path = Uri.path url in
    List.mem path wl
  with _ ->
    (* String.sub, Ezjsonm.from_string, List.assoc, Ezjsonm.get_list *)
    (* throw all kinds of exceptions, TODO: logging them? *)
    false *)


(* assume all destinations are valid for now *)
let verify_destination dest caveat_str =
  try
    let prefix_len = String.length "destination = " in
    let prefix = String.sub caveat_str 0 prefix_len in
    prefix = "destination = "
  with _ -> false


let extract_destination body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let open Ezjsonm in
  let dic = from_string body |> value |> get_dict in
  let dest = List.assoc "dest" dic in
  return @@ get_string dest


let verify macaroon key uri meth =
  let open R in
  if not @@ is_ok macaroon then error @@ get_error macaroon
  else if not @@ is_ok key then error @@ get_error key
  else begin
    let macaroon = get_ok macaroon
    and key      = get_ok key in

    let check str =
      let f verifier = verifier str in
      let l = [
        verify_target;
        verify_method meth;
        verify_path uri;
        verify_destination "";] in
      List.exists f l
    in

    return @@ Macaroon.verify macaroon ~key ~check []
  end


let extract_macaroon headers =
  let open R in
  (match Cohttp.Header.get headers "x-api-key" with
  | Some m -> ok m
  | None -> begin
      match Cohttp.Header.get_authorization headers with
      | Some (`Basic (name, _)) -> ok name
      | _ -> error_msg "Missing API key/token" end)
  >>= fun t ->
  match Macaroon.deserialize t with
  | `Ok m -> ok m
  | `Error (_, _) -> error_msg "Invalid API key/token"


let macaroon_verifier_mw =
  let filter = fun handler req ->
    let uri = Request.uri req in
    let meth = Request.meth req in
    let headers = Request.headers req in

    let macaroon = extract_macaroon headers
    and key = secret () in

    let r = verify macaroon key uri meth in

    if R.is_error r then
      let msg =
        let e = R.get_error r in
        let _ = Format.flush_str_formatter () in
        let () = R.pp_msg Format.str_formatter e in
        Format.flush_str_formatter ()
      in
      let info = Printf.sprintf "macaroon verification fails: %s" msg in
      Logs_lwt.info (fun m -> m "%s" info) >>= fun () ->

      let body = Cohttp_lwt_body.of_string info in
      let code = `Unauthorized in
      return @@ Response.create ~body ~code ()

    else match R.get_ok r with
    | true ->
        Logs_lwt.info (fun m -> m "macaroon verification passes") >>= fun () ->
        handler req
    | false ->
        let info = "Invalid API key/token" in
        Logs_lwt.info (fun m -> m "%s" info) >>= fun () ->

        let body = Cohttp_lwt_body.of_string info in
        let code = `Unauthorized in
        return @@ Response.create ~body ~code ()
  in
  Opium_rock.Middleware.create ~filter ~name:"Macaroon Verifier"
