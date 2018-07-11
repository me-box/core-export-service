val init : ?secret:string -> unit -> unit Lwt.t

val macaroon_verifier_mw : Opium_kernel.Rock.Middleware.t

val macaroon_request_checker : Cohttp.Request.t -> body:string -> bool Lwt.t
