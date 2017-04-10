val new_request : Uuidm.t -> unit

val request_processing : Uuidm.t -> unit

val request_finished : Uuidm.t -> unit

val exported : int -> unit

val dump: unit -> (unit, string) result
