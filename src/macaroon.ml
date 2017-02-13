open Lwt
open Opium.Std

module Client = Cohttp_lwt_unix.Client


let s = ref None

let get_secret () =
  let endp = Uri.of_string (Env.arbiter_endp ()) in
  let url = Uri.with_path endp "/store/secret" in

  let h = ["X-Api-Key", Env.arbiter_token ()] in
  let headers = Cohttp.Header.of_list h in

  Client.get ~headers url >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  let code = Cohttp.Code.code_of_status status in

  if not (code = 200) then return_unit
  (* TODO: log unexpected status code *)
  else
    Cohttp_lwt_body.to_string body >>= fun body ->
    s := Some (B64.decode body);
    return_unit


let rec secret () = match !s with
  | None -> get_secret () >>= secret
  | Some s -> return s
