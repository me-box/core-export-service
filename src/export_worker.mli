type queue

type id = Uuidm.t

val get_queue : unit -> queue

val new_request : Export_typ.request -> queue -> Export_typ.response

val request_finished : queue -> id -> unit

val mem : queue -> id ->  bool

val get_state : queue -> id -> Export_typ.state

val get_con : queue -> id -> unit Lwt_condition.t

val worker_t : queue -> unit Lwt.t
