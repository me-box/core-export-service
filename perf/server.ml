#!/bin/ocaml

#use "topfind";;
#require "lwt.unix";;
#require "cohttp.lwt";;
#require "cohttp.lwt-core";;

open Lwt

module Server = Cohttp_lwt_unix.Server


let callback (_, conn) req body =
  Printf.printf "[perf] conn %s opened.\n%!" (Cohttp.Connection.to_string conn);
  let meth = Cohttp.Request.meth req in
  let path = Cohttp.Request.uri req |> Uri.path in
  if meth = `POST && path = "/export" then
    Lwt_unix.sleep 0.1 >>= fun () ->
    Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty ()
  else Server.respond_not_found ()


let t =
  let conn_closed (_, conn) =
    Printf.printf "[perf] conn %s closed.\n%!" (Cohttp.Connection.to_string conn)
  in
  let spec = Server.make ~conn_closed ~callback () in
  let mode = `TCP (`Port 8088) in
  Server.create ~mode spec


let () = Lwt_main.run t
