[@@@ocaml.warning "-unused-var-strict"]
[@@@ocaml.warning "-unused-field"]

open Cmdliner

(* https://erratique.ch/software/logs/doc/Logs_lwt/index.html#report_ex *)
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let report (src : Logs.Src.t) level ~over k msgf =
    let k _ =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k () (* this is 'k' from 'report' not 'k ()' *)
    in
    (* Adapted from https://erratique.ch/software/logs/doc/Logs/index.html#ex1 *)
    let with_source h k ppf fmt =
      Format.kfprintf k ppf
        ("%a [%s] @[" ^^ fmt ^^ "@]@.")
        Logs_fmt.pp_header (level, h) (Logs.Src.name src)
    in
    match level with
    | Logs.App -> msgf @@ fun ?header ?tags fmt -> with_source header k app fmt
    | _ -> msgf @@ fun ?header ?tags fmt -> with_source header k dst fmt
  in
  { Logs.report }

(* Inspired by
   https://github.com/mjambon/cmdliner-cheatsheet/blob/main/src/Demo_arg_main.ml *)

type conf = {
  name : string;
  log : unit;
}

let name_term =
  let default = "John Doe" in
  let info =
    Arg.info
      [ "N"; "name" ] (* '-N' and '--name' will be synonyms *)
      ~docv:"NAME" ~doc:"Name to print"
  in
  Arg.value (Arg.opt Arg.string default info)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (lwt_reporter ());
  ()

let setup_log_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let conf_term run =
  let combine name log =
    let conf = { name; log } in
    run conf
  in
  Term.(const combine $ name_term $ setup_log_term)

let parse_command_line_and_run run =
  let info =
    Cmd.info "lwt-reporter-example"
    (* program name as it will appear in --help *)
  in
  Cmd.v info (conf_term run) |> Cmd.eval |> Stdlib.exit

let main conf =
  Logs.info (fun m -> m "My name is: %s" conf.name);
  Lwt_reporter_example.A_module.log_name conf.name

let () = parse_command_line_and_run main
