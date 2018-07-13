val local_name : unit -> string

val local_port : unit -> string

val arbiter_token : unit -> string

val arbiter_public_key : unit -> string

val arbiter_uri : unit -> string

val init_https : unit -> (Fpath.t * Fpath.t, Rresult.R.msg) result
