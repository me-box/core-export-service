open Export_typ
open Lwt.Infix

module W = Export_worker

type factory = {
  w_tbl : (string, W.queue) Hashtbl.t;
  push  : W.queue option -> unit;
}


let get_queue {w_tbl; push} ~id =
  if Hashtbl.mem w_tbl id then Hashtbl.find w_tbl id
  else begin
    Logs.info (fun m -> m "new worker needed for client: %s" id);
    let q = W.create_queue ~owner:id in
    Hashtbl.replace w_tbl id q;
    push (Some q);
    q
  end


let init () =
  let f, push = Lwt_stream.create () in
  let s, _ = Lwt.wait () in
  let rec factory workers () =
    Lwt.choose [
      Lwt_stream.get f;
      Lwt.join workers >>= fun () -> Lwt.return_none] >>= function
    | None ->
        if Lwt_stream.is_closed f then
          Logs_lwt.err (fun m -> m "new worker stream closed")
        else begin
          Logs_lwt.warn (fun m -> m "some worker failes") >>= fun () -> factory workers ()
        end
    | Some q -> factory (W.worker_t q :: workers) ()
  in
  factory [s], {w_tbl = Hashtbl.create 13; push}
