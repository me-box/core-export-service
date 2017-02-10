#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "databox-bridge" @@ fun c ->
  Ok [ Pkg.mllib "src/databox-bridge.mllib";
       Pkg.test "test/test"; ]
