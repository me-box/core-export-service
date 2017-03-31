open Lwt
open Opium.Std
open Export_typ


module Client = Cohttp_lwt_unix.Client
module R      = Rresult.R


type queue = {
    t    : request Lwt_stream.t;
    stbl : (Uuidm.t, state) Hashtbl.t;
    etbl : (Uuidm.t, unit Lwt_condition.t) Hashtbl.t;
    push : request option -> unit; }


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


let request_finished r q =
  Hashtbl.remove q.stbl r.id;
  Hashtbl.remove q.etbl r.id


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
          let () = request_finished r q in
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
          let () = request_finished r q in
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

  let cert, key =
    match Export_env.init_certs () with
    | Ok (cp, kp) -> Fpath.to_string cp, Fpath.to_string kp
    | Error msg ->
        Logs.debug(fun m ->
            m "while installing https certs: %a" Rresult.R.pp_msg msg);
        "", ""
  in

  let app = App.empty |> App.port p in

  if cert = "" || key = "" then app else
  app |> App.ssl ~cert ~key


let polling () =
  let queue = get_queue () in
  let app =
    base_app ()
    |> middleware Macaroon.macaroon_verifier_mw
    |> export queue
    |> export_lp queue
  in

  let export_queue () =
    match App.run_command' app with
    | `Ok t -> t
    | _ -> assert false
  in

  Macaroon.init () >>= fun () ->
  Lwt.join [export_queue (); worker_t queue; ]



let client_fr_handler push fr =
  let open Websocket_cohttp_lwt.Frame in
  let () = push @@ Some fr in
  if fr.opcode = Opcode.Close then push None


(* add tests about this and insert logging functions *)
let ws_processor q fr_str push_fr () =
  let client_con = Lwt_condition.create () in
  let open Websocket_cohttp_lwt.Frame in
  let rec listen_to_client () =
    Lwt_stream.get fr_str >>= function
    | Some fr when fr.opcode = Opcode.Text ->
        let req = decode_request fr.content in
        R.bind req (fun r ->
            if r.id = Uuidm.nil then
              let resp = new_request r q in
              let v = `New resp.req_id in
              R.ok @@ Lwt_condition.signal client_con @@ `Client v
            else if not (Hashtbl.mem q.etbl r.id) then
              let content =
                Format.asprintf "not recognized id: %a" Uuidm.pp r.id in
              let fr = create ~content () in
              R.ok @@ push_fr @@ Some fr
            else
            let v = `Old r.id in
            R.ok @@ Lwt_condition.signal client_con @@ `Client v)
        |> (function
          | Ok () -> listen_to_client ()
          | Error msg ->
              let fr = create ~content:msg () in
              let () = push_fr @@ Some fr in
              listen_to_client ())
    | Some fr when fr.opcode = Opcode.Ping ->
        let pong = create ~opcode:Opcode.Pong () in
        let () = push_fr @@ Some pong in
        listen_to_client ()
    | Some fr when fr.opcode = Opcode.Close ->
        let close =
          if String.length fr.content >= 2 then
            let content = String.sub fr.content 0 2 in
            create ~opcode:Opcode.Close ~content ()
          else close 1000
        in
        let () = Lwt_condition.signal client_con @@ `Client `Close in
        let () = push_fr @@ Some close in
        listen_to_client ()
    | Some fr ->
        Logs_lwt.warn (fun m -> m "unparsable frame: %a" pp fr)
        >>= listen_to_client
    | None ->
        Logs_lwt.err (fun m -> m "client frame stream closed!")
  in
  let push_to_client () =
    let push_frame resp =
      json_of_response resp
      |> Ezjsonm.to_string
      |> fun content -> push_fr @@ Some (create ~content ())
    in
    let wait_on_id id =
      let con = Hashtbl.find q.etbl id in
      Lwt_condition.wait con >>= fun () ->
      return @@ `Worker id
    in
    let clean_table id =
      Hashtbl.remove q.stbl id;
      Hashtbl.remove q.etbl id
    in
    let push_response id =
      let state = Hashtbl.find q.stbl id in
      match state  with
      | `Finished ext_resp ->
          let ext_response = Some ext_resp in
          let response = {req_id = id; state; ext_response} in
          push_frame response
      | #state ->
          let ext_response = None in
          let response = {req_id = id; state; ext_response} in
          push_frame response
    in
    let clean_up lt =
      let rec aux acc =
        Lwt.nchoose_split acc >>= fun (rts, nrts) ->
        List.iter (function
          | `Worker id -> clean_table id
          (* shouldn't see others here*)
          | _ -> ()) rts;
        if List.length nrts <> 0 then aux nrts
        else return_unit
      in
      aux lt
    in
    let is_finished id =
      match Hashtbl.find q.stbl id with
      | `Finished _ -> true
      | _ -> false
    in
    let rec loop acc =
      Lwt.nchoose_split acc >>= fun (rts, nrts) ->
      let closed = ref false in
      List.fold_left (fun acc rt -> match rt with
        | `Client (`New id) ->
            push_response id;
            Lwt_condition.wait client_con
            :: wait_on_id id
            :: acc
        | `Client (`Old id) ->
            push_response id;
            Lwt_condition.wait client_con :: acc
        | `Client `Close ->
            closed := true; acc
        | `Worker id ->
            if not !closed then push_response id;
            if is_finished id then begin
              clean_table id; acc end
            else wait_on_id id :: acc) nrts rts
      |> fun nrts -> if !closed then return nrts else loop nrts
    in
    loop [Lwt_condition.wait client_con]
    >>= clean_up
  in
  listen_to_client () <&> push_to_client ()


let handler q (flow, conn) req body =
  Logs_lwt.info (fun m -> m "[ws] connection %s opened%!"
      (Cohttp.Connection.to_string conn)) >>= fun () ->
  Cohttp_lwt_body.to_string body >>= fun body ->
  Macaroon.macaroon_request_checker req ~body >>= function
  | true ->
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    (match path with
    | "/ws/export" ->
        let fr_str, push = Lwt_stream.create () in
        let handler = client_fr_handler push in
        Websocket_cohttp_lwt.upgrade_connection req flow handler
        >>= fun (resp, body, push_frame) ->
        Lwt.async @@ ws_processor q fr_str push_frame;
        return (resp, body)
    | _ ->
        let resp = Cohttp.Response.make ~status:`Not_found () in
        let body = Cohttp_lwt_body.empty in
        return (resp, body))
  | false ->
  let resp = Cohttp.Response.make ~status:`Unauthorized () in
  let body = Cohttp_lwt_body.of_string "Missing/Invalide API key/token" in
  return (resp, body)


let mode p =
  match Export_env.init_certs () with
  | Ok (cp, kp) ->
      let config = `Crt_file_path (Fpath.to_string cp),
                   `Key_file_path (Fpath.to_string kp),
                   `No_password, `Port p in
      `TLS config
  | Error msg ->
      Logs.debug(fun m ->
          m "while installing https certs: %a" Rresult.R.pp_msg msg);
      `TCP (`Port p)


let ws ?(port = 8081) () =
  let q = get_queue () in
  let mode = mode port in
  let conn_closed (_, conn) =
    Logs.info (fun m -> m "[ws] connection %s closed%!"
      (Cohttp.Connection.to_string conn))
  in
  let server = Cohttp_lwt_unix.Server.make ~conn_closed ~callback:(handler q) () in

  Macaroon.init () >>= fun () ->
  Lwt.join [
    worker_t q;
    Cohttp_lwt_unix.Server.create ~mode server]
