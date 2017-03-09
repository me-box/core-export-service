open Lwt
open Opium.Std

module Client = Cohttp_lwt_unix.Client
module R      = Rresult.R


type request = {
  id   : Uuidm.t;
  uri  : Uri.t;
  data : Ezjsonm.t; }


type response = {
  req_id       : Uuidm.t;
  state        : state;
  ext_response : ext_response option; }

and state = [`Pending | `Processing | `Finished of ext_response]

and ext_response = {
  status : Cohttp.Code.status_code;
  body   : string; }


type queue = {
    t    : request Lwt_stream.t;
    stbl : (Uuidm.t, state) Hashtbl.t;
    etbl : (Uuidm.t, unit Lwt_condition.t) Hashtbl.t;
    push : request option -> unit; }


let depyt_request : request Depyt.t =
  let open Depyt in
  let id =
    let to_t str =
      if str = "" then Uuidm.nil
      else match Uuidm.of_string str with
      | None -> raise (Invalid_argument str)
      | Some id -> id
    in
    like string to_t Uuidm.to_string
  in
  let uri = like string Uri.of_string Uri.to_string in
  let data = like string Ezjsonm.from_string Ezjsonm.to_string in
  record "request" (fun id uri data -> {id; uri; data})
  |+ field "id"   id   (fun r -> r.id)
  |+ field "uri"  uri  (fun r -> r.uri)
  |+ field "data" data (fun r -> r.data)
  |> sealr


let string_of_state = function
  | `Pending -> "Pending"
  | `Processing -> "Processing"
  | `Finished _ -> "Finished"


let json_of_response {req_id; state; ext_response} =
  let open Ezjsonm in
  let id = req_id |> Uuidm.to_string |> string in
  let state = state |> string_of_state |> string in
  let ext_response = match ext_response with
  | None -> `O []
  | Some {status; body} ->
      let status = status |> Cohttp.Code.string_of_status |> string in
      let body   = body |> string in
      `O ["status", status;
          "body", body]
  in
  `O ["id", id;
      "state", state;
      "ext_response", ext_response]


let decode_request body =
  let decoder = Jsonm.decoder (`String body) in
  Depyt.decode_json depyt_request decoder


let new_request r q =
  let id = Uuidm.create `V4 in
  let request = {r with id} in
  let state = `Pending in

  let ext_response = None in
  let response = {req_id = id; state; ext_response} in

  let () = Hashtbl.add q.stbl id state in
  let () = Hashtbl.add q.etbl id (Lwt_condition.create ()) in
  let () = q.push (Some request) in
  response


let export q =
  let handler q req =
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    Logs_lwt.info (fun m -> m "body: %s" body) >>= fun () ->

    let request = decode_request body in
    let bind_f r =
      if r.id = Uuidm.nil then R.ok @@ new_request r q

      else if not @@ Hashtbl.mem q.stbl r.id then
        let id = Uuidm.to_string r.id in
        R.error @@ "Can't find request with the id of " ^ id

      else match Hashtbl.find q.stbl r.id with
      | `Finished ext_resp as state ->
          let ext_response = Some ext_resp in
          let response = {req_id = r.id; state; ext_response} in
          let () = Hashtbl.remove q.stbl r.id in
          R.ok response
      | #state as state ->
          let response = {req_id = r.id; state; ext_response = None} in
          R.ok response
    in

    match R.bind request bind_f with
    | Ok r ->
        let obj = json_of_response r in
        `Json obj |> respond'
    | Error m ->
        let code = `Not_found in
        `String m |> respond' ~code
  in
  post "/export" @@ handler q


let get_timeout_param req =
  let default = 5.0 in
  let uri = Request.uri req in
  let param = Uri.get_query_param uri "timeout" in
  match param with
  | None -> default
  | Some t -> try float_of_string t with _ -> default


let export_lp q =
  let handler q req =
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    Logs_lwt.info (fun m -> m "body: %s" body) >>= fun () ->

    let timeout = get_timeout_param req in
    let request = decode_request body in

    let rec process r =
      if r.id = Uuidm.nil then Lwt.return_ok @@ new_request r q

      else if not @@ Hashtbl.mem q.stbl r.id then
        let id = Uuidm.to_string r.id in
        Lwt.return_error @@ "Can't find request with the id of " ^ id

      else match Hashtbl.find q.stbl r.id with
      | `Finished ext_resp as state ->
          let ext_response = Some ext_resp in
          let response = {req_id = r.id; state; ext_response} in
          let () = Hashtbl.remove q.stbl r.id in
          let () = Hashtbl.remove q.etbl r.id in
          Lwt.return_ok response
      | #state ->
          let con = Hashtbl.find q.etbl r.id in
          Lwt.pick [Lwt_unix.sleep timeout; Lwt_condition.wait con] >>= fun () ->

          match Hashtbl.find q.stbl r.id with
          | `Finished _ -> process r
          | #state as state' ->
            let response = {req_id = r.id; state = state'; ext_response = None} in
            Lwt.return_ok response
    in

    match request with
    | Ok req ->
        process req >>= (function
        | Ok resp -> `Json (json_of_response resp) |> respond'
        | Error msg -> `String msg |> respond' ~code:`Not_found)
    | Error msg -> `String msg |> respond' ~code:`Not_found
  in
  post "/lp/export" @@ handler q


let worker_t q =
  let process {id; uri; data} =
    let () = Hashtbl.replace q.stbl id `Processing in
    let () =
      let con = Hashtbl.find q.etbl id in
      Lwt_condition.signal con ()
    in
    Logs_lwt.debug (fun m -> m "%a state -> Processing" Uuidm.pp id)
    >>= fun () ->

    let body =
      data |> Ezjsonm.to_string
      |> Cohttp_lwt_body.of_string
    in
    Client.post ~body uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    let status = Cohttp.Response.status resp in
    let ext_resp = {status; body} in
    let state = `Finished ext_resp in

    let () = Hashtbl.replace q.stbl id state in
    let () =
      let con = Hashtbl.find q.etbl id in
      Lwt_condition.signal con ()
    in
    Logs_lwt.debug (fun m -> m "%a state -> Finished" Uuidm.pp id)
    >>= return
  in
  let rec aux () =
    Lwt_stream.get q.t >>= function
    | None -> Logs_lwt.warn (fun m -> m "Got None...(stream closed?)")
    | Some r ->
        process r >>= fun () ->
        Logs_lwt.info (fun m -> m "Finished processing of a request!") >>=
        aux
  in
  aux ()


let base_app () =
  let p = Export_env.local_port () |> int_of_string in
  let () = Logs.info (fun m -> m "serving on port %d" p) in

  let env_cert = Export_env.cert_path ()
  and env_key  = Export_env.key_path () in

  let app = App.empty |> App.port p in

  if env_cert = "" || env_key = "" then app else
  let open Bos.OS in
  let open Rresult.R.Infix in
  (Dir.user () >>= fun user ->
  let cert_dir =
    Fpath.add_seg user "certs"
    |> Fpath.to_dir_path
  in
  Dir.create cert_dir >>= fun _ ->

  let file_cert = Fpath.add_seg cert_dir "public.cert"
  and file_key  = Fpath.add_seg cert_dir "private.key" in
  File.delete file_cert >>= fun () ->
  File.delete file_key  >>= fun () ->
  File.write file_cert env_cert >>= fun () ->
  File.write file_key env_key   >>= fun () ->
  let cert = Fpath.to_string file_cert in
  let key  = Fpath.to_string file_key in
  Ok (cert, key))
  |> function
  | Error msg ->
      Logs.err(fun m ->
          m "while installing https certs: %a" Rresult.R.pp_msg msg);
      app
  | Ok (cert, key) ->
      app |> App.ssl ~cert ~key


let t () =
  let t, push = Lwt_stream.create () in
  let stbl = Hashtbl.create 13 in
  let etbl = Hashtbl.create 13 in
  let queue = {t; stbl; etbl; push} in

  let app =
    base_app ()
    |> middleware Macaroon.macaroon_verifier_mw
    |> export queue
    |> export_lp queue
  in

  let export_queue =
    match App.run_command' app with
    | `Ok t -> t
    | _ -> assert false
  in

  Lwt.join [
    export_queue;
    worker_t queue; ]
