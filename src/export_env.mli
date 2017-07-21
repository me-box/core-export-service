val local_name : unit -> string

val local_port : unit -> string

val arbiter_endp : unit -> string

val arbiter_token : unit -> string

val init_https : unit -> (Fpath.t * Fpath.t, Rresult.R.msg) result
