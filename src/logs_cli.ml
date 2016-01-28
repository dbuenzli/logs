(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner

let strf = Format.asprintf

let level ?env ?docs () =
  let vopts =
    let doc = "Increase verbosity. Repeatable, but more than twice does
               not bring more."
    in
    Arg.(value & flag_all & info ["v"; "verbose"] ~doc ?docs)
  in
  let verbosity =
    let enum =
      [ "warning", None; (* Hack for the option's absent rendering *)
        "quiet", Some None;
        "error", Some (Some Logs.Error);
        "warning", Some (Some Logs.Warning);
        "info", Some (Some Logs.Info);
        "debug", Some (Some Logs.Debug); ]
    in
    let log_level = Arg.enum enum in
    let enum_alts = Arg.doc_alts_enum List.(tl enum) in
    let doc = strf "Be more or less verbose. $(docv) must be %s. Takes over
                    $(b,-v)." enum_alts
    in
    Arg.(value & opt log_level None &
         info ["verbosity"] ?env ~docv:"LEVEL" ~doc ?docs)
  in
  let quiet =
    let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc ?docs)
  in
  let choose quiet verbosity vopts =
    if quiet then None else match verbosity with
    | Some verbosity -> verbosity
    | None ->
        match List.length vopts with
        | 0 -> Some Logs.Warning
        | 1 -> Some Logs.Info
        | n -> Some Logs.Debug
  in
  Term.(const choose $ quiet $ verbosity $ vopts)

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
