open Lwt

module Client = Cohttp_lwt_unix.Client
module R      = Rresult.R


type test_step = {
  meth      : Cohttp.Code.meth;
  uri       : Uri.t;
  before_ts : unit -> (Cohttp.Header.t * Cohttp_lwt_body.t) Lwt.t;
  after_ts  : Cohttp.Response.t -> string -> (unit, string) Rresult.result Lwt.t;
  next_ts   : unit -> test_step option Lwt.t
}


let no_next_step = fun () -> return_none

let print_after_ts (resp, body) =
  let status =
    resp
    |> Cohttp.Response.status
    |> Cohttp.Code.string_of_status
  in
  Logs_lwt.info (fun m -> m "[test] status: %s  body: %s" status body)
  >>= return_ok

let empty_before_ts () =
  let h = Cohttp.Header.init () in
  let b = Cohttp_lwt_body.empty in
  return (h, b)


let env : (string, string) Hashtbl.t = Hashtbl.create 13

let put_env k v = Hashtbl.replace env k v

let get_env k =
  match Hashtbl.mem env k with
  | false -> None
  | true  -> Some (Hashtbl.find env k)

let rm_env k = Hashtbl.remove env k

let clear_env () = Hashtbl.clear env

let pp_status ppf s =
  Format.fprintf ppf "%s" (Cohttp.Code.string_of_status s)

let assert_equal ~exp  v pp =
  if exp = v then return_ok () else
  Logs_lwt.err (fun m ->
      m "[test] assertion failed: %a <> %a [expectation]" pp v pp exp)
  >>= fun () ->
  return_error "assertion failed"


let rec process_step {meth; uri; before_ts; after_ts; next_ts} =
  before_ts () >>= fun (headers, body) ->
  (match meth with
  | `GET  -> Client.get ~headers uri
  | `POST -> Client.post ~body ~headers uri
  | _ -> Lwt.fail_with "not implemented method")

  >>= fun (resp, b) -> Cohttp_lwt_body.to_string b
  >>= fun body -> after_ts resp body
  >>= function
  | Rresult.Ok () -> begin
      next_ts () >>= function
      | None -> return_ok ()
      | Some s -> process_step s end
  | Rresult.Error msg ->
      return_error msg


let process_case (name, steps) =
  let rec aux = function
  | Rresult.Ok (), s :: tl ->
      process_step s >>= fun r ->
      aux (r, tl)
  | Rresult.Error m, _ ->
      return_error "test case failed"
  | r, _ -> return r
  in
  aux (Rresult.Ok (), steps)


let process_suit cases =
  Lwt_list.mapi_s (fun i (name, steps) ->
      Logs_lwt.info (fun m -> m "[test] case%d <%s> starts..." i name)
      >>= fun () -> process_case (name, steps)
      >>= fun r ->
      (if R.is_ok r then
         Logs_lwt.info (fun m -> m "[test] case%d <%s> passed" i name)
       else
       Logs_lwt.err (fun m -> m "[test] case%d <%s> failed" i name))
      >>= fun () ->
      clear_env ();
      return r) cases

  >>= fun rs -> Lwt_list.for_all_p (fun r -> return @@ R.is_ok r) rs
  >>= function
  | false -> fail_with "some test case failed"
  | true -> return_unit
