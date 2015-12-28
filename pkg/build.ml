#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let jsoo = Env.bool "jsoo"
let cmdliner = Env.bool "cmdliner"
let fmt = Env.bool "fmt"
let lwt = Env.bool "lwt"

let () =
  Pkg.describe "logs" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/logs";
    Pkg.lib ~cond:fmt ~exts:Exts.module_library "src/logs_stdo";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library "src/logs_browser";
    Pkg.lib ~cond:cmdliner ~exts:Exts.module_library "src/logs_cli";
    Pkg.lib ~cond:lwt ~exts:Exts.module_library "src/logs_lwt";
    Pkg.lib ~cond:fmt ~exts:Exts.library "src/logs_top";
    Pkg.lib "src/logs_top_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/tool.ml";
    Pkg.doc "test/tags.ml"; ]
