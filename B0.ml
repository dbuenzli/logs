open B0_kit.V000

(* OCaml library names *)

let mtime = B0_ocaml.libname "mtime"
let mtime_clock_os = B0_ocaml.libname "mtime.clock.os"
let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"
let fmt = B0_ocaml.libname "fmt"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let fmt_cli = B0_ocaml.libname "fmt.cli"
let lwt = B0_ocaml.libname "lwt"
let lwt_unix = B0_ocaml.libname "lwt.unix"
let js_of_ocaml_compiler_runtime =
  B0_ocaml.libname "js_of_ocaml-compiler.runtime"

let logs = B0_ocaml.libname "logs"
let logs_fmt = B0_ocaml.libname "logs.fmt"
let logs_browser = B0_ocaml.libname "logs.browser"
let logs_cli = B0_ocaml.libname "logs.cli"
let logs_lwt = B0_ocaml.libname "logs.lwt"
let logs_threaded = B0_ocaml.libname "logs.threaded"

(* Libraries *)

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let logs_lib =
  let srcs = mod_srcs "logs" in
  B0_ocaml.lib logs ~doc:"The logs library" ~srcs ~requires:[]

let logs_fmt_lib =
  let srcs = mod_srcs "logs_fmt" in
  let requires = [logs; fmt] in
  B0_ocaml.lib logs_fmt ~doc:"The logs.fmt library" ~srcs ~requires

let logs_browser_lib =
  let srcs = mod_srcs "logs_browser" in
  let requires = [logs; js_of_ocaml_compiler_runtime] in
  B0_ocaml.lib logs_browser ~doc:"The logs.browser library" ~srcs ~requires

let logs_threaded_lib =
  let srcs = mod_srcs "logs_threaded" in
  let requires = [logs; threads] in
  B0_ocaml.lib logs_threaded ~doc:"The logs.threaded library" ~srcs ~requires

let logs_cli_lib =
  let srcs = mod_srcs "logs_cli" in
  let requires = [logs; cmdliner] in
  B0_ocaml.lib logs_cli ~doc:"The logs.cli library" ~srcs ~requires

let logs_lwt_lib =
  let srcs = mod_srcs "logs_lwt" in
  let requires = [logs; lwt] in
  B0_ocaml.lib logs_lwt ~doc:"The logs.lwt library" ~srcs ~requires

(* Tools *)

(* Tests *)

let test ?doc base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "test/%s.ml" base))] in
  B0_ocaml.exe base ?doc ~srcs ~requires

let test_fmt = test "test_fmt" ~requires:[logs; logs_fmt; fmt_tty]
let test_tool =
  let requires = [logs; logs_fmt; logs_cli; fmt_cli; fmt_tty; cmdliner] in
  test "tool" ~requires

let test_tags = test "tags" ~requires:[logs; mtime; mtime_clock_os]
let test_formatter = test "test_formatter" ~requires:[logs]
let test_multi = test "test_multi" ~requires:[logs; logs_fmt; fmt_tty]
let test_threaded =
  let requires = [logs; logs_fmt; logs_threaded; threads] in
  test "test_threaded" ~requires

let test_lwt =
  let requires = [logs; logs_fmt; logs_lwt; fmt; fmt_tty; lwt; lwt_unix] in
  test "test_lwt" ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The logs programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/logs"
    |> add online_doc "https://erratique.ch/software/logs/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/logs.git"
    |> add issues "https://github.com/dbuenzli/logs/issues"
    |> add description_tags ["log"; "system"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-js_of_ocaml" "%{js_of_ocaml:installed}%"
          "--with-fmt" "%{fmt:installed}%"
          "--with-cmdliner" "%{cmdliner:installed}%"
          "--with-lwt" "%{lwt:installed}%"
          "--with-base-threads" "%{base-threads:installed}%"]]|}
    |> add B0_opam.Meta.depopts ["cmdliner", "";
                                 "js_of_ocaml", "";
                                 "fmt", "";
                                 "lwt", "";
                                 "base-threads", ""]
    |> add B0_opam.Meta.conflicts [
      "cmdliner", {|< "1.1.0"|};
      "js_of_ocaml", {|< "4.0.0"|};
      "fmt", {|< "0.9.0"|}; ]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "mtime", {|with-test|};]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"logs package" ~meta ~locked:true @@
  B0_unit.list ()
