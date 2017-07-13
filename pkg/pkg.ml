#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "export-service" @@ fun c ->
  Ok [
    Pkg.mllib "src/export-service.mllib";
    Pkg.bin "bin/service" ~dst:"export-service";
    Pkg.test "test/test_polling";
    Pkg.test "test/test_ws"
  ]
