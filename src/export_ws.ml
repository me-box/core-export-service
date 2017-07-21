open Lwt
open Export_typ

module R = Rresult.R
module W = Export_worker


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
              let resp = W.new_request r q in
              let v = `New resp.req_id in
              R.ok @@ Lwt_condition.signal client_con @@ `Client v
            else if not @@ W.mem q r.id then
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
      let con = W.get_con q id in
      Lwt_condition.wait con >>= fun () ->
      return @@ `Worker id
    in
    let push_response id =
      let state = W.get_state q id in
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
          | `Worker id -> W.request_finished q id
          (* shouldn't see others here*)
          | _ -> ()) rts;
        if List.length nrts <> 0 then aux nrts
        else return_unit
      in
      aux lt
    in
    let is_finished id =
      match W.get_state q id with
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
              W.request_finished q id; acc end
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
  match Export_env.init_https () with
  | Ok (cp, kp) ->
      let config = `Crt_file_path (Fpath.to_string cp),
                   `Key_file_path (Fpath.to_string kp),
                   `No_password, `Port p in
      `TLS config
  | Error msg ->
      Logs.debug(fun m ->
          m "while installing https certs: %a" Rresult.R.pp_msg msg);
      `TCP (`Port p)


let ws ?secret ?port () =
  let q = W.create_queue ~owner:"ws" in
  let port =
    match port with
    | None -> Export_env.local_port () |> int_of_string
    | Some p -> p
  in
  let mode = mode port in
  let conn_closed (_, conn) =
    Logs.info (fun m -> m "[ws] connection %s closed%!"
      (Cohttp.Connection.to_string conn))
  in
  let server = Cohttp_lwt_unix.Server.make ~conn_closed ~callback:(handler q) () in

  Macaroon.init ?secret () >>= fun () ->
  Lwt.join [
    W.worker_t q;
    Cohttp_lwt_unix.Server.create ~mode server]
