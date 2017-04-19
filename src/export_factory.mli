type factory

val get_queue : factory -> id:string -> Export_worker.queue

val init : unit -> (unit -> unit Lwt.t) * factory
