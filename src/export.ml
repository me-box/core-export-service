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


let export q =
  let handler q req =
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    Logs_lwt.info (fun m -> m "body: %s\n" body) >>= fun () ->

    let request = decode_request body in
    let bind_f r =
      if r.id = Uuidm.nil then
        let id = Uuidm.create `V4 in
        let request = {r with id} in
        let state = `Pending in

        let ext_response = None in
        let response = {req_id = id; state; ext_response} in

        let () = Hashtbl.add q.stbl id state in
        let () = q.push (Some request) in
        R.ok response

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


let worker_t q =
  let process {id; uri; data} =
    let () = Hashtbl.replace q.stbl id `Processing in
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
    return_unit
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


let t () =
  let t, push = Lwt_stream.create () in
  let stbl = Hashtbl.create 13 in
  let queue = {t; stbl; push} in

  let p = Export_env.local_port () |> int_of_string in
  let () = Logs.info (fun m -> m "serving on port %d" p) in

  let cert = Export_env.cert_path ()
  and key  = Export_env.key_path () in

  let base_app =
    App.empty
    |> App.port p
    |> (if cert != "" && key != "" then App.ssl ~cert ~key
        else fun b -> b)
  in
  let app =
    base_app
    |> middleware Macaroon.macaroon_verifier_mw
    |> export queue in

  let export_queue =
    match App.run_command' app with
    | `Ok t -> t
    | _ -> assert false
  in

  Lwt.join [
    export_queue;
    worker_t queue; ]
