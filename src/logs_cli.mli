(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} support for {!Logs}.

    See a full {{!ex}example}.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Options for setting the report level} *)

val level : ?env:Cmdliner.Arg.env -> ?docs:string -> unit ->
    Logs.level option Cmdliner.Term.t
(** [level ?env ?docs ()] is a term for two {!Cmdliner} options that
    can be used with {!Logs.set_level}.  The options are documented
    under [docs] (defaults to the default of {!Cmdliner.Arg.info}).

    The options work as follows:
    {ul
    {- [-v [LEVEL]] or [--verbose=[LEVEL]], the value of the term
       is [Some l] where [l] depends on [LEVEL]. If [LEVEL] is
       unspecified the value of the term is [Some Logs.Info].}
    {- [-q] or [--quiet], the value of the term is [None]. Takes
       over the [--verbose] option if both are present.}
    {- If both options are absent the default value is
       [Some Logs.warning]}}

    If [env] is provided, the default value in case both options are
    absent can be overridden by the corresponding environment
    variable. *)

(** {1:ex Example}

    The following example shows how to setup {!Logs} and {!Fmt} so
    that logging is performed on standard outputs with ANSI coloring
    if these are [tty]s. The command line interface provides options
    to control the use of colors and the log reporting level.
{[
let hello () = Logs.app (fun m -> m "Hello horrible world!")

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_stdo.reporter ());
  ()

(* Command line interface *)

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main () =
  match Term.(eval (const hello $ setup_log, Term.info "tool")) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)

let () = main ()
]}

*)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
