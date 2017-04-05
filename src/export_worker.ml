open Export_typ
open Lwt.Infix

module Client = Cohttp_lwt_unix.Client


type queue = {
    t    : request Lwt_stream.t;
    stbl : (id, state) Hashtbl.t;
    etbl : (id, unit Lwt_condition.t) Hashtbl.t;
    push : request option -> unit; }
and id = Uuidm.t


let _queue = ref None
let get_queue () =
  match !_queue with
  | Some q -> q
  | None ->
      let t, push = Lwt_stream.create () in
      let stbl = Hashtbl.create 13 in
      let etbl = Hashtbl.create 13 in
      let queue = {t; stbl; etbl; push} in
      let () = _queue := Some queue in
      queue


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


let request_finished q id =
  Hashtbl.remove q.stbl id;
  Hashtbl.remove q.etbl id


let mem q id =
  Hashtbl.mem q.stbl id


let get_state q id =
  Hashtbl.find q.stbl id


let get_con q id =
  Hashtbl.find q.etbl id


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
    Logs_lwt.debug (fun m ->
        m "POSTing with %s" (Ezjsonm.to_string data)) >>= fun () ->
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body uri >>= fun (resp, body) ->
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
    >>= Lwt.return
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
