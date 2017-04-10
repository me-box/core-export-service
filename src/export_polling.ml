open Export_typ
open Opium.Std
open Lwt.Infix

module R = Rresult.R
module W = Export_worker

let export q =
  let handler q req =
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun body ->
    Logs_lwt.info (fun m -> m "body: %s" body) >>= fun () ->

    let request = decode_request body in
    let bind_f r =
      if r.id = Uuidm.nil then R.ok @@ W.new_request r q

      else if not @@ W.mem q r.id then
        let id = Uuidm.to_string r.id in
        R.error @@ "Can't find request with the id of " ^ id

      else match W.get_state q r.id with
      | `Finished ext_resp as state ->
          let ext_response = Some ext_resp in
          let response = {req_id = r.id; state; ext_response} in
          let () = W.request_finished q r.id in
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
      if r.id = Uuidm.nil then Lwt.return_ok @@ W.new_request r q

      else if not @@ W.mem q r.id then
        let id = Uuidm.to_string r.id in
        Lwt.return_error @@ "Can't find request with the id of " ^ id

      else match W.get_state q r.id with
      | `Finished ext_resp as state ->
          let ext_response = Some ext_resp in
          let response = {req_id = r.id; state; ext_response} in
          let () = W.request_finished q r.id in
          Lwt.return_ok response
      | #state ->
          let con = W.get_con q r.id in
          Lwt.pick [Lwt_unix.sleep timeout; Lwt_condition.wait con] >>= fun () ->

          match W.get_state q r.id with
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


let dump =
  let handler _ =
    Logs_lwt.info (fun m -> m "starting dumping stats ...") >>= fun () ->
    let r = Export_stats.dump () in
    Logs_lwt.info (fun m -> m "finished dumping.") >>= fun () ->
    if R.is_ok r then respond' @@ `String ""
    else respond' @@ `String (R.get_error r)
  in
  get "/dump" handler


let base_app ?port () =
  let p =
    match port with
    | None -> Export_env.local_port () |> int_of_string
    | Some p -> p in
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


let polling ?(lp = false) ?secret ?port () =
  let queue = W.get_queue () in
  let app =
    base_app ?port ()
    |> middleware Macaroon.macaroon_verifier_mw
    |> dump
    |> if lp then export_lp queue else export queue
  in

  let argv = Array.of_list ["opium"] in
  let export_queue () =
    match App.run_command' ~argv app with
    | `Ok t -> t
    | _ -> assert false
  in

  Macaroon.init ?secret () >>= fun () ->
  Lwt.join [export_queue (); W.worker_t queue; ]
