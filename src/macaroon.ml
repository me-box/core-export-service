open Lwt
open Opium.Std

module Client = Cohttp_lwt_unix.Client
module Macaroon   = Sodium_macaroons


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


let rec secret () = match !s with
  | None -> get_secret () >>= secret
  | Some s -> return s


let verify_target caveat_str =
  let expected = "target = " ^ Export_env.local_name () in
  expected = caveat_str


let verify_path url meth caveat_str =
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


let verify_destination dest caveat_str =
  let expected = "destination = " ^ dest in
  caveat_str = expected


let extract_destination body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let open Ezjsonm in
  let dic = from_string body |> value |> get_dict in
  let dest = List.assoc "url" dic in
  return @@ get_string dest


(* TODO: lots of exception processing *)
let macaroon_verifier_mw =
  let filter = fun handler req ->
    let headers = Request.headers req in
    let token =
      match Cohttp.Header.get headers "X-Api-Key" with
      | None -> raise Not_found
      | Some m -> m
    in
    let macaroon =
      match Macaroon.deserialize token with
      | `Ok m -> m
      | `Error (_, _) -> raise (Invalid_argument token)
    in
    secret () >>= fun key ->

    let body = Request.body req in
    extract_destination body >>= fun dest ->

    let url = Request.uri req in
    let meth = Request.meth req in

    let check str =
      let f verifier = verifier str in
      let l = [
          verify_target;
          verify_path url meth;
          verify_destination dest;] in
      List.exists f l
    in

    let r = Macaroon.verify macaroon ~key ~check [] in
    if r then handler req
    else
      let body = Cohttp_lwt_body.of_string "Invalid API key/token" in
      let code = `Unauthorized in
      return @@ Response.create ~body ~code ()
  in
  Opium_rock.Middleware.create ~filter ~name:"Macaroon Verifier"
