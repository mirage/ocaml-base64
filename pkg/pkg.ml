#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "base64" @@ fun c ->
  Ok [ Pkg.mllib "src/b64.mllib" ]
