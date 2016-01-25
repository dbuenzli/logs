(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Format} colorful reporter for {!Logs}.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Reporter} *)

val reporter :
  ?pp_header:(Logs.level * string option) Fmt.t ->
  ?app:Format.formatter ->
  ?dst:Format.formatter -> unit -> Logs.reporter
(** [reporter] is like {!Logs.format_reporter} except ANSI colors may be
    used in message header rendering if the formatters are configured to do so;
    see {!Fmt.set_style_renderer} and {!Fmt_tty}.

    Consult a full command line {{!Logs_cli.ex}setup example}. *)

(** {1:cheader Colored message headers} *)

val err_style : Fmt.style
(** [err_style] is the style used to render headers at error level. *)

val warn_style : Fmt.style
(** [warn_style] is the style used to render headers at warning level. *)

val info_style : Fmt.style
(** [info_style] is the style used to render headers at info level. *)

val debug_style : Fmt.style
(** [debug_style] is the style used to render headers at debug level. *)

val pp_header : (Logs.level * string option) Fmt.t
(** [pp_header] is like {!Logs.pp_header} but may use ANSI colors if the
    formatter is configured to do so, see {!Fmt.set_style_renderer} and
    {!Fmt_tty}. *)

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
