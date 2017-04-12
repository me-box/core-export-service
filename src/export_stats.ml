let t = Hashtbl.create 2017

let now = Mtime.elapsed

let new_request id = Hashtbl.add t (now ()) (`Created id)

let request_processing id = Hashtbl.add t (now ()) (`Processing id)

let request_finished id = Hashtbl.add t (now ()) (`Finished id)

let exported len = Hashtbl.add t (now ()) (`Export len)


let to_list () =
  let min = ref 0. in
  let lt = Hashtbl.fold (fun tm ev acc ->
      let tm = Mtime.to_us tm in
      if compare tm !min < 0 then min := tm;
      (tm, ev) :: acc) t [] in
  let sorted = List.sort (fun e1 e2 -> compare (fst e1) (fst e2)) lt in
  List.map (fun (tm, ev) -> tm -. !min, ev) sorted


let dump () =
  let ts = to_list () in
  let cnt_buf = Buffer.create 997
  and bw_buf = Buffer.create 997 in
  let cnt = ref 0
  and bw = ref 0
  and lc = Hashtbl.create 997 in

  List.iter (fun (tm, ev) -> match ev with
    | `Created id ->
        assert (not @@ Hashtbl.mem lc id);
        let arr = Array.create_float 3 in
        arr.(0) <- tm;
        Hashtbl.add lc id arr;
        incr cnt;
        let entry = Printf.sprintf "%f %d\n" tm !cnt in
        Buffer.add_string cnt_buf entry
    | `Processing id ->
        let arr = Hashtbl.find lc id in
        arr.(1) <- tm;
    | `Finished id ->
        let arr = Hashtbl.find lc id in
        arr.(2) <- tm;
        decr cnt;
        let entry = Printf.sprintf "%f %d\n" tm !cnt in
        Buffer.add_string cnt_buf entry
    | `Export len ->
        bw := !bw + len;
        let entry = Printf.sprintf "%f %d\n" tm !bw in
        Buffer.add_string bw_buf entry) ts;

  let lc_buf = Buffer.create 997 in
  Hashtbl.iter (fun id arr ->
      let entry = Printf.sprintf "%s %f %f %f\n"
          (String.sub (Uuidm.to_string id) 0 8) arr.(0) arr.(1) arr.(2) in
      Buffer.add_string lc_buf entry) lc;

  Hashtbl.clear t;

  let open Bos.OS in
  let open Rresult.R in
  (Dir.current () >>= fun dir ->
  let cnt = Fpath.add_seg dir "queue_count"
  and bw  = Fpath.add_seg dir "throughput"
  and delay = Fpath.add_seg dir "request_delay" in
  File.write cnt (Buffer.contents cnt_buf) >>= fun () ->
  File.write bw  (Buffer.contents bw_buf) >>= fun () ->
  File.write delay (Buffer.contents lc_buf))
  |> reword_error (fun msg -> Format.(
      fprintf str_formatter "dump: %a" pp_msg msg;
      flush_str_formatter ()))
