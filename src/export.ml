open Lwt
open Opium.Std

type request = {
    id   : Uuidm.t;
    url  : Uri.t;
    body : Cohttp_lwt_body.t; }

type response = {
    status : Cohttp.Code.status_code;
    body   : Cohttp_lwt_body.t; }


type state = [`Pending | `Processing | `Finished of response]


type queue = {
    t    : request Lwt_stream.t;
    stbl : (Uuidm.t, state) Hashtbl.t;
    push : request option -> unit; }


let string_of_state = function
  | `Pending -> "Pending"
  | `Processing -> "Processing"
  | `Finished _ -> "Finished"


let export q =
  let handler q req =
    let body = Request.body req in
    Cohttp_lwt_body.to_string body >>= fun body ->

    let open Ezjsonm in
    let obj =
      from_string body
      |> value
      |> get_dict
    in
    let id =
      List.assoc "id" obj
      |> get_string
    in

    if id = "" then
      let url =
        List.assoc "dest" obj
        |> get_string
      in
      let body =
        List.assoc "data" obj
        |> get_string
      in
      let id = Uuidm.create `V4 in
      let request = {
          id;
          url = Uri.of_string url;
          body = Cohttp_lwt_body.of_string body}
      in
      let state = `Pending in
      let r = dict [
          "id", Uuidm.to_string id |> string;
          "state", string_of_state state |> string;
          "response", "" |> string;]
      in
      let () = Hashtbl.add q.stbl id state in
      let () = q.push (Some request) in
      `Json r |> respond'
    else
      let id = match Uuidm.of_string id with
        | None -> raise @@ Invalid_argument id
        | Some uuid -> uuid
      in
      match Hashtbl.find q.stbl id with
      | `Finished {status; body} as s -> begin
         Cohttp_lwt_body.to_string body >>= fun body ->
         let r = dict [
           "status", Cohttp.Code.string_of_status status |> string;
           "body", body |> string; ]
           |> value
         in
         let resp = dict [
           "id", Uuidm.to_string id |> string;
           "state", string_of_state s |> string;
           "respons", r;]
         in
         let () = Hashtbl.remove q.stbl id in
         `Json resp |> respond' end
      | #state as state ->
         let resp = dict [
           "id", Uuidm.to_string id |> string;
           "state", string_of_state state |> string;
           "respons", "" |> string;]
         in
         `Json resp |> respond'
  in
  post "/export" @@ handler q





