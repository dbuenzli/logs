(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) = match l with
| Logs.App ->
    (match h with None -> () | Some h -> Fmt.pf ppf "[%s]" h)
| Logs.Error ->
    pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
| Logs.Warning ->
    pp_h ppf warn_style (match h with None -> "WARNING" | Some h -> h)
| Logs.Info ->
    pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
| Logs.Debug ->
    pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)

let pp_exec_header =
  let x = Filename.basename Sys.executable_name in
  let pp_h ppf style h = Fmt.pf ppf "%s: [%a] " x Fmt.(styled style string) h in
  pp_header ~pp_h

let reporter pp_header app dst src level over k msgf =
  let k _ = over (); k () in
  msgf @@ fun ?header ?tags fmt ->
  let ppf = if level = Logs.App then app else dst in
  Fmt.kpf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, header)

let reporter
    ?(pp_header = pp_exec_header) ?(app = Fmt.stdout) ?(dst = Fmt.stderr) () =
  let report src level ~over k = reporter pp_header app dst src level over k in
  { Logs.report = report }

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style string) h in
  pp_header ~pp_h

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
