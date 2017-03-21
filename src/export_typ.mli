type request = { id : Uuidm.t; uri : Uri.t; data : Ezjsonm.t; }

val decode_request : string -> (request, string) Result.result

type response = {
  req_id : Uuidm.t;
  state : state;
  ext_response : ext_response option;
}
and state = [ `Finished of ext_response | `Pending | `Processing ]
and ext_response = { status : Cohttp.Code.status_code; body : string; }

val string_of_state : [< `Finished of 'a | `Pending | `Processing ] -> string

val json_of_response : response -> [> `O of (string * Ezjsonm.value) list ]
