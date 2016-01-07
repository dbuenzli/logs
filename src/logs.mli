(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Logging.

    [Logs] provides a basic logging infrastructure. {{!func}Logging}
    is performed on {{!srcs}sources} whose reporting
    {{!type:level}level} can be set independently. Log message
    report is decoupled from logging and handled by a
    {{!reporters}reporter}.

    See the {{!basics}basics}, a few {{!usage}usage conventions} to
    respect and a note on {{!sync}synchronous logging}.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

open Result

(** {1:levels Reporting levels} *)

(** The type for reporting levels. For level semantics see the
    {{!usage}usage conventions}.

    Log {{!srcs}sources} have an optional {{!Src.level}reporting level}. If
    the level is [Some l] then any message whose level is smaller or
    equal to [l] is reported. If the level is [None] no message is
    ever reported. *)
type level = App | Error | Warning | Info | Debug

val level : unit -> level option
(** [level ()] is the reporting level given to {{!Src.create}new sources}. *)

val set_level : ?all:bool -> level option -> unit
(** [set_level ?all l] sets the reporting level given to
    {{!Src.create}new sources}. If [all] is [true] (default), also
    sets the reporting level of all {{!Src.list}existing sources}. Use
    {!Src.set_level} to only affect a specific source. Only applications
    should use this function directly see {{!usage}usage conventions}. *)

val pp_level : Format.formatter -> level -> unit
(** [pp_level ppf l] prints an unspecified representation of [l] on
    [ppf]. *)

(** {1:srcs Log sources} *)

type src
(** The type for log sources. A source defines a named unit of logging
    whose reporting level can be set independently. *)

val default : src
(** [default] is a logging source that is reserved for use by
    applications. See {{!usage}usage conventions}. *)

(** Sources. *)
module Src : sig

  (** {1 Sources} *)

  type t = src
  (** The type for log sources. *)

  val create : ?doc:string -> string -> src
  (** [create ?doc name] is a new log source. [name] is the name of
      the source; it doesn't need to be unique but it is good practice
      to prefix the name with the name of your package or library
      (e.g. ["mypkg.network"]). [doc] is a documentation string
      describing the source, defaults to ["undocumented"]. The initial
      reporting level of the source is defined by {!Logs.level}. *)

  val name : src -> string
  (** [name] is [src]'s name. *)

  val doc : src -> string
  (** [doc src] is [src]'s documentation string. *)

  val level : src -> level option
  (** [level src] is the report level of [src] (if any).  *)

  val set_level : src -> level option -> unit
  (** [set_level src l] sets the report level of [src] to [l]. Only
      applications should use this function directly, see {{!usage}usage
      conventions}. *)

  val equal : src -> src -> bool
  (** [equal src src'] is [true] iff [src] and [src'] are the same source. *)

  val compare : src -> src -> int
  (** [compare src src'] is a total order on sources. *)

  val pp : Format.formatter -> src -> unit
  (** [pp ppf src] prints an unspecified representation of [src] on
      [ppf]. *)

  val list : unit -> src list
  (** [list ()] is the current exisiting source list. *)
end

(** {1:func Log functions} *)

(** Message tags.

    Message tags are arbitrary named and typed values that can be
    associated to log messages. See an {{!ex1}example}. *)
module Tag : sig

  (** {1 Tag definitions} *)

  type 'a def
  (** The type for tag definitions. The type ['a] is the type of the
      tag. The definition specifies a name for the tag, a pretty-printer
      for the type of the tag and a documentation string. See {!val:def}. *)

  (** The type for existential tag definitions. *)
  type def_e = Def : 'a def -> def_e

  val def : ?doc:string -> string -> (Format.formatter -> 'a -> unit) -> 'a def
  (** [def ~doc name pp] is a tag definition. [name] is the name of
      the tag, it doesn't need to be unique. [pp] is a printer for the
      type of the tag. [doc] is a documentation string describing
      the tag (defaults to ["undocumented"]). *)

  val name : 'a def -> string
  (** [name d] is [d]'s name. *)

  val doc : 'a def -> string
  (** [doc d] is [d]'s documentation string. *)

  val printer : 'a def -> (Format.formatter -> 'a -> unit)
  (** [printer d] is [d]'s type pretty-printer. *)

  val pp_def : Format.formatter -> 'a def -> unit
  (** [pp_def ppf d] prints an unspecified representation of [d] on [ppf]. *)

  val list : unit -> def_e list
  (** [tag_list ()] is the list of currently existing tag definitions. *)

  (** {1 Tags} *)

  (** The type for tags. Tuples the tag definition and its value. *)
  type t = V : 'a def * 'a -> t

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints an unspecified representation of [t] on [ppf]. *)

  (** {1 Tag sets} *)

  type set
  (** The type for tag sets. A tag set contains at most one tag per
      tag definition. *)

  val empty : set
  (** [empty] is the empty set. *)

  val is_empty : set -> bool
  (** [is_empty s] is [true] iff [s] is empty. *)

  val mem : 'a def -> set -> bool
  (** [mem d s] is [true] iff [s] has a tag with definition [d]. *)

  val add : 'a def -> 'a -> set -> set
  (** [add d v s] is [s] with the tag [(V (d, v))] added. If there was a tag
      with definition [d] in [s] it is replaced. *)

  val rem : 'a def -> set -> set
  (** [rem d s] is [s] without the tag defined by [d] (if there was one). *)

  val find : 'a def -> set -> 'a option
  (** [find d s] is the tag value with definition [d] in [s] (if any). *)

  val get : 'a def -> set -> 'a
  (** [get d s] is like [find d s] but @raise Invalid_argument if there
      is no tag with definition [d] in [s]. *)

  val fold : (t -> 'a -> 'a) -> set -> 'a -> 'a
  (** [fold f s acc] is the result of folding [f] over the tags
      of [s] starting with [acc]. *)

  val pp_set : Format.formatter -> set -> unit
  (** [pp_set ppf s] prints an unspecified representation of s on [ppf]. *)
end

type ('a, 'b) msgf =
  (?header:string -> ?tags:Tag.set ->
   ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
(** The type for client specified message formatting functions.

    Message formatting functions are called with a message
    construction function whenever a message needs to be reported. The
    message formatting function must call the given message
    construction function with a format string and its arguments to
    define the message contents, see the {{!logging}basics} for examples.
    The optional arguments of the message construction function are:
    {ul
    {- [header], an optional printable message header. Default to [None].}
    {- [tags], a set of tags to attach to the message. Defaults
       {!Tag.empty}.}} *)

type 'a log = ('a, unit) msgf -> unit
(** The type for log functions. See the {{!logging}basics} to understand
    how to use log functions. *)

val msg : ?src:src -> level -> 'a log
(** [msg ?src l (fun m -> m fmt ...)] logs with level [l] on the source
    [src] (defaults to {!default}) a message formatted with [fmt]. For the
    semantics of levels see the {{!usage}the usage conventions}. *)

val app : ?src:src -> 'a log
(** [app] is [msg App]. *)

val err : ?src:src -> 'a log
(** [err] is [msg Error]. *)

val warn : ?src:src -> 'a log
(** [warn] is [msg Warning]. *)

val info : ?src:src -> 'a log
(** [info] is [msg Info]. *)

val debug : ?src:src -> 'a log
(** [debug] is [msg Debug]. *)

val kmsg : (unit -> 'b) -> ?src:src -> level -> ('a, 'b) msgf -> 'b
(** [kmsg k] is like {!msg} but calls [k] for returning. *)

(** {2:result Logging [result] value [Error]s} *)

val on_error : ?src:src -> ?level:level -> ?header:string -> ?tags:Tag.set ->
  pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result -> 'a
(** [on_error ~level ~pp ~use r] is:
    {ul
    {- [v] if [r = Ok v]}
    {- [use e] if [r = Error e]. As a side effect [msg] is logged
       with [pp] on level [level] (defaults to {!Logs.Error}).}} *)

val on_error_msg : ?src:src -> ?level:level -> ?header:string ->
  ?tags:Tag.set -> use:(unit -> 'a) ->
  ('a, [`Msg of string]) result -> 'a
(** [on_error_msg] is like {!on_error} but uses
    {!Format.pp_print_text} to format the message. *)

(** {1:srcfunc Source specific log functions} *)

(** The type for source specific logging functions. *)
module type LOG = sig

  (** {1:func Log functions} *)

  val msg : level -> 'a log
  (** See {!Logs.msg}. *)

  val app : 'a log
  (** [app] is [msg App]. *)

  val err : 'a log
  (** [err] is [msg Error]. *)

  val warn : 'a log
  (** [warn] is [msg Warning]. *)

  val info : 'a log
  (** [info] is [msg Info]. *)

  val debug : 'a log
  (** [debug] is [msg Debug]. *)

  val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
  (** See {!Logs.kmsg}. *)

  (** {2:result Logging [result] value [Error]s} *)

  val on_error : ?level:level -> ?header:string -> ?tags:Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result ->
    'a
  (** See {!Logs.on_error}. *)

  val on_error_msg : ?level:level -> ?header:string -> ?tags:Tag.set ->
    use:(unit -> 'a) -> ('a, [`Msg of string]) result -> 'a
  (** See {!Logs.on_error_msg}. *)
end

val src_log : src -> (module LOG)
(** [src_log src] is a {{!LOG}set of logging functions} for [src]. *)

(** {1:reporters Reporters} *)

type reporter =
  { report : 'a 'b. src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
      ('a, 'b) msgf -> 'b }
(** The type for reporters.

    A reporter formats and handles log messages that get
    reported. Whenever a {{!func}log function} gets called on a source
    with a level equal or smaller to the {{!Src.level}source's reporting
    level}, the {{!reporter}current reporter}'s field [r.report]
    gets called as [r.report src level ~over k msgf]
    where:
    {ul
    {- [src] is the logging source.}
    {- [level] is the reporting level.}
    {- [over] must be called by the reporter once the logging operation is
       over from the reporter's perspective. This may happen before or
       after [k] is called.}
    {- [k] is the function to invoke to return.}
    {- [msgf] is the {{!msgf}message formatting function} to call.}}
    See an {{!ex1}example}. *)

val nop_reporter : reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it
    does nothing if a log message gets reported. *)

val reporter : unit -> reporter
(** [reporter ()] is the current repporter. *)

val set_reporter : reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)

(**/**)
val report : src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
  ('a, 'b) msgf -> 'b
val _err_count : int ref
val _warn_count : int ref
(**/**)

val pp_header : Format.formatter -> (level * string option) -> unit
(** [pp_header ppf (l, h)] prints an unspecified representation
    of log header [h] for level [l]. *)

(** {1:monitoring Logs monitoring} *)

val err_count : unit -> int
(** [err_count ()] is the number of messages logged with level [Error]
    across all sources. *)

val warn_count : unit -> int
(** [warn_count ()] is the number of messages logged with level
    [Warning] across all sources. *)

(** {1:basics Basics}

    {2:logging Logging}

    In order to minimize the overhead whenever a log message is not reported,
    message formatting only occurs on actual message report via the
    {{!msgf}message formatting function} you provide to log functions. This
    leads to the following logging structure:
{[
let k, v = ... in
Logs.err (fun m -> m "invalid kv (%a,%a)" pp_key k pp_val v);
Logs.err (fun m -> m "NO CARRIER");
]}
    The pattern is quite simple: it is as if you were formatting with
    a [printf]-like function except you get this function in the [m]
    argument of the function you give to the logging function.

    If you want to abstract a repeated log report it is better to keep
    the message formatting function structure for the arguments of the
    messages. Here's how the above examples can be abstracted and
    invoked:
{[
let err_invalid_kv args =
  Logs.err @@ fun m ->
  args (fun k v -> m "invalid kv (%a,%a)" pp_key k pp_val v)

let err_no_carrier args =
  Logs.err @@ fun m -> args (m "NO CARRIER")

let () =
  err_invalid_kv @@ fun args -> args "key" "value";
  err_no_carrier @@ fun () -> ();
  ()
]}
    Note that log messages are formatted and hit the reporter only if
    they have not been filtered out by the current
    {{!Src.level}reporting level} of the source you log on. See also
    the log source and reporting level {{!usage}usage conventions}.

    {2:setupreporter Reporter setup}

    If you are writing an application you must remember to
    {{!set_reporter}set} the reporter before any logging operation
    takes place otherwise no messages will be reported. For example if
    you are using the {{!Logs_fmt}formatter reporter}, logging
    can be setup as follows:
{[
let main () =
  Logs.set_reporter (Logs_fmt.reporter ());
  ...
  exit (if Logs.err_count > 0 then 1 else 0);
  ()
]}
    If you have logging code that is performed in the toplevel
    initialization code of modules (not a good idea) or you depend on
    (bad) libraries that do so, you must call and link the reporter
    install code before these initialization bits are being executed
    otherwise you will miss these messages.

    The documentation of {!Logs_cli} module has a {{!Logs_cli.ex}full setup
    example} that includes command line options to control color and log
    reporting level.

    If you are writing a library you should neither install reporters, nor
    set the reporting level of sources, nor log on the {!default} source or
    at the [App] level; follow the {{!usage}the usage conventions}. A
    library should simply log on an another existing source or define
    its own source like in the example below:
{[
let src = Logs.Src.create "mylib.network" ~doc:"logs mylib's network events"
module Log = (val Logs.src_log src : Logs.LOG)
]}
    The [Log] module defines logging functions that are specific to the
    source [src].

    {1:usage Usage conventions}

    A library should never log on the {!default} source or at the
    [App] level these are reserved for use by the application. It
    should either create a source for itself or log on the source
    defined by one of its dependencies. It should also never set the
    reporting level of the sources it deals with or install reporters since
    control over this must be left to the application.

    The semantics of {{!type:level}reporting levels} should be understood
    as follows:
    {ul
    {- [App], this level can be used for the standard output or console
       of an application. It should never be used by libraries.}
    {- [Error], error condition that prevent the program from
       running normally.}
    {- [Warning], suspicious condition that does not prevent the
       program from running normally but may eventually lead to an
       error condition.}
    {- [Info], condition that allows the program {e user} to get a better
       understanding of what the program is doing.}
    {- [Debug], condition that allows the program {e developer} to get a
       better undersanding of what the program is doing.}}

    {1:sync Note on synchronous logging}

    In synchronous logging, a client call to a log function proceeds
    only once the reporter has finished the logging operation. In
    [Logs] this depends on both the reporter and the client.

    The client gives to the reporter a continuation that defines the
    result type of the log function and a callback to be called whenever
    the log operation is over (see {!kmsg}). The later can be invoked
    before or after the continuation and can be used to unblock
    the continuation, this is what is done for example

    For example if the {!Logs_fmt.reporter} is used with formatters
    that write to channels all log functions will be synchronous
    and block until either the report is discarded or the formatted
    message has been written to the channel.

    However this the latter reporter is problematic in an cooperative
    concurency setting like {!Lwt}, since it will block the whole runtime
    system. In the latter case the log functions of {!Logs_lwt} can be
    used.



    {1:ex1 Example with custom reporter and tags}

    This example uses a {{!Tag}tag} to attach {!Mtime} time spans in
    log messages. The custom reporter uses these time spans to format
    relative timings for runs of a given function. Note that as done
    below the timings do include logging time.
{[
let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.pp_span

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime.count c))

let run () =
  let rec wait n = if n = 0 then () else wait (n - 1) in
  let c = Mtime.counter () in
  Logs.info (fun m -> m "Starting run");
  let delay1, delay2, delay3 = 10_000, 20_000, 40_000 in
  Logs.info (fun m -> m "Start action 1 (%d)." delay1 ~tags:(stamp c));
  wait delay1;
  Logs.info (fun m -> m "Start action 2 (%d)." delay2 ~tags:(stamp c));
  wait delay2;
  Logs.info (fun m -> m "Start action 3 (%d)." delay3 ~tags:(stamp c));
  wait delay3;
  Logs.info (fun m -> m "Done." ?header:None ~tags:(stamp c));
  ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h tags k ppf fmt =
      let stamp = match tags with
      | None -> None
      | Some tags -> Logs.Tag.find stamp_tag tags
      in
      let dt = match stamp with None -> 0. | Some s -> (Mtime.to_us s) in
      Format.kfprintf k ppf ("%a[%0+04.0fus] @[" ^^ fmt ^^ "@]@.") dt
        Logs.pp_header (level, h) dt
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }

let main () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Info);
  run ();
  run ();
  ()

let () = main ()
]}
Here is the standard output of a sample run of the program:
{v
[INFO][+000us] Starting run
[INFO][+168us] Start action 1 (10000).
[INFO][+206us] Start action 2 (20000).
[INFO][+243us] Start action 3 (40000).
[INFO][+303us] Done.
[INFO][+000us] Starting run
[INFO][+012us] Start action 1 (10000).
[INFO][+038us] Start action 2 (20000).
[INFO][+074us] Start action 3 (40000).
[INFO][+133us] Done.
v}

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
