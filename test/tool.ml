(* This code is in the public domain. *)

(* Example setup for a simple command line tool with colorful output. *)

let hello () = Logs.app "Hello horrible world!" Logs.unit

(* Command line interface *)

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_stdo.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main () =
  match Term.(eval (const hello $ setup_log, Term.info "tool")) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)

let () = main ()
