#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "databox-bridge" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Export"] "src/databox-bridge.mllib";
       Pkg.bin "src/service" ~dst:"databox-export-service";
       Pkg.test "test/test"; ]
