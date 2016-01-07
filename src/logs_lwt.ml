(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

type 'a log = ('a, unit Lwt.t) Logs.msgf -> unit Lwt.t

let kmsg k ?(src = Logs.default) level msgf = match Logs.Src.level src with
| None -> k ()
| Some level' when level > level' ->
    (if level = Logs.Error then Logs.incr_err_count () else
     if level = Logs.Warning then Logs.incr_warn_count () else ());
    (k ())
| Some _ ->
    (if level = Logs.Error then Logs.incr_err_count () else
     if level = Logs.Warning then Logs.incr_warn_count () else ());
    let (ret, unblock) = Lwt.wait () in
    let k () = Lwt.bind ret k in
    let over () = Lwt.wakeup unblock () in
    Logs.report src level ~over k msgf

let kunit _ = Lwt.return ()
let msg ?src level msgf = kmsg kunit ?src level msgf
let app ?src msgf = kmsg kunit ?src Logs.App msgf
let err ?src msgf = kmsg kunit ?src Logs.Error msgf
let warn ?src msgf = kmsg kunit ?src Logs.Warning msgf
let info ?src msgf = kmsg kunit ?src Logs.Info msgf
let debug ?src msgf = kmsg kunit ?src Logs.Debug msgf

let on_error ?src ?(level = Logs.Error) ?header ?tags ~pp ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error e ->
      kmsg (fun () -> use e) ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" pp e

let on_error_msg ?src ?(level = Logs.Error) ?header ?tags ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error (`Msg e) ->
      kmsg use ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" Logs.pp_print_text e

(* Source specific functions *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log
  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Lwt.t) ->
    Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t

  val on_error : ?level:Logs.level -> ?header:string -> ?tags:Logs.Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a Lwt.t) ->
    ('a, 'b) result Lwt.t -> 'a Lwt.t

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) ->
    ('a, [`Msg of string]) result Lwt.t -> 'a Lwt.t
end

let src_log src =
  let module Log = struct
    let msg level msgf = msg ~src level msgf
    let kmsg ?over k level msgf = kmsg k ~src level msgf
    let app msgf = msg Logs.App msgf
    let err msgf = msg Logs.Error msgf
    let warn msgf = msg Logs.Warning msgf
    let info msgf = msg Logs.Info msgf
    let debug msgf = msg Logs.Debug msgf
    let on_error ?level ?header ?tags ~pp ~use =
      on_error ~src ?level ?header ?tags ~pp ~use

    let on_error_msg ?level ?header ?tags ~use =
      on_error_msg ~src ?level ?header ?tags ~use
  end
  in
  (module Log : LOG)

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
