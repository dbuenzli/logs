(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let reporter prefix dst app src level over k msgf =
  let k _ = over (); k () in
  let with_header style header k ppf fmt =
    Format.kfprintf k ppf ("%s[%a] @[" ^^ fmt ^^ "@]@.") prefix
      Fmt.(styled style string) header
  in
  msgf @@ fun ?header ?tags fmt ->
  match level with
  | Logs.App ->
      begin match header with
      | None -> Format.kfprintf k app ("@[" ^^ fmt ^^ "@]@.")
      | Some l -> Format.kfprintf k app ("[%s] @[" ^^ fmt ^^ "@]@.") l
      end
  | Logs.Error ->
      let header = match header with None -> "ERROR" | Some h -> h in
      with_header err_style header k dst fmt
  | Logs.Warning ->
      let header = match header with None -> "WARNING" | Some h -> h in
      with_header warn_style header k dst fmt
  | Logs.Info ->
      let header = match header with None -> "INFO" | Some h -> h in
      with_header info_style header k dst fmt
  | Logs.Debug ->
      let header = match header with None -> "DEBUG" | Some h -> h in
      with_header debug_style header k dst fmt

let reporter ?prefix ?(dst = Fmt.stderr) ?(app = Fmt.stdout)  () =
  let prefix = match prefix with
  | None -> Printf.sprintf "%s: " (Filename.basename Sys.executable_name)
  | Some None -> ""
  | Some Some prefix -> prefix
  in
  let report src level ~over k = reporter prefix dst app src level over k in
  { Logs.report = report }

let pp_header ppf (l, h) =
  let pp_h ppf style h = Fmt.(pf ppf "[%a]" (styled style string) h) in
  match l with
  | Logs.App ->
      begin match h with
      | None -> ()
      | Some h -> Format.fprintf ppf "[%s]" h
      end
  | Logs.Error ->
      pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
  | Logs.Warning ->
      pp_h ppf err_style (match h with None -> "WARNING" | Some h -> h)
  | Logs.Info ->
      pp_h ppf err_style (match h with None -> "INFO" | Some h -> h)
  | Logs.Debug ->
      pp_h ppf err_style (match h with None -> "DEBUG" | Some h -> h)

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
