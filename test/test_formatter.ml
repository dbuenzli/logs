(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp_key = Format.pp_print_string
let pp_val = Format.pp_print_string

let err_invalid_kv args =
  Logs.err @@ fun m ->
  args (fun k v -> m "invalid kv (%a,%a)" pp_key k pp_val v)

let err_no_carrier args =
  Logs.err @@ fun m -> args (m "NO CARRIER")

let main () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.info (fun m -> m ~header:"START" ?tags:None "Starting main");
  Logs.warn (fun m -> m "Hey be warned by %d." 7);
  Logs.err (fun m -> m "Hey be errored.");
  Logs.debug (fun m -> m "Would you mind to be debugged a bit ?");
  Logs.app (fun m -> m "This is for the application console or stdout.");
  let k = "key" in
  let v = "value" in
  Logs.err (fun m -> m "invalid kv (%a,%a)" pp_key k pp_val v);
  Logs.err (fun m -> m "NO CARRIER");
  err_invalid_kv (fun args -> args k v);
  err_no_carrier (fun () -> ());
  Logs.info (fun m -> m "Ending main");
  exit (if (Logs.err_count () > 0) then 1 else 0)

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli.
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
