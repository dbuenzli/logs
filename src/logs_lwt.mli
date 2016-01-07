(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Lwt} support.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)


(** {1 Log functions} *)

open Result

type 'a log = ('a, unit Lwt.t) Logs.msgf -> unit Lwt.t
(** The type for Lwt log functions. See {!Logs.log}.

    The returned thread only proceeds once the log operation is over
    (see {!Logs.kmsg}). *)

val msg : ?src:Logs.src -> Logs.level -> 'a log
(** See {!Logs.msg}. *)

val app : ?src:Logs.src -> 'a log
(** See {!Logs.app}. *)

val err : ?src:Logs.src -> 'a log
(** See {!Logs.err}. *)

val warn : ?src:Logs.src -> 'a log
(** See {!Logs.warn}. *)

val info : ?src:Logs.src -> 'a log
(** See {!Logs.info}. *)

val debug : ?src:Logs.src -> 'a log
(** See {!Logs.debug}. *)

val kmsg : (unit -> 'b Lwt.t) -> ?src:Logs.src ->
  Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t
(** See {!Logs.kmsg}. *)

(** {2 Logging {!result} value [Error]s} *)

val on_error : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
  use:('b -> 'a Lwt.t) -> ('a, 'b) result Lwt.t -> 'a Lwt.t
(** See {!Logs.on_error}. *)

val on_error_msg : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) ->
  ('a, [`Msg of string]) result Lwt.t -> 'a Lwt.t
(** See {!Logs.on_error_msg}. *)

(** {1 Source specific log functions} *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  (** See {!Logs.msg}. *)

  val app : 'a log
  (** See {!Logs.app}. *)

  val err : 'a log
  (** See {!Logs.err}. *)

  val warn : 'a log
  (** See {!Logs.warn}. *)

  val info : 'a log
  (** See {!Logs.info}. *)

  val debug : 'a log
  (** See {!Logs.debug}. *)

  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Lwt.t) ->
    Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t
  (** See {!Logs.kmsg}. *)

  (** {2 Logging {!result} value [Error]s} *)

  val on_error : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
    use:('b -> 'a Lwt.t) -> ('a, 'b) result Lwt.t -> 'a Lwt.t
  (** See {!Logs.on_error}. *)

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) -> ('a, [`Msg of
    string]) result Lwt.t -> 'a Lwt.t
  (** See {!Logs.on_error_msg}. *)
end

val src_log : Logs.src -> (module LOG)


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
