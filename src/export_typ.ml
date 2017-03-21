type request = {
  id   : Uuidm.t;
  uri  : Uri.t;
  data : Ezjsonm.t; }


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


let decode_request body =
  let decoder = Jsonm.decoder (`String body) in
  Depyt.decode_json depyt_request decoder



type response = {
  req_id       : Uuidm.t;
  state        : state;
  ext_response : ext_response option; }

and state = [`Pending | `Processing | `Finished of ext_response]

and ext_response = {
  status : Cohttp.Code.status_code;
  body   : string; }


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
