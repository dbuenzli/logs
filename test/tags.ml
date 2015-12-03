(* This code is in the public domain. *)

(* Example with tags and custom reporter. *)

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.pp_span

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime.count c))

let run () =
  let rec wait n = if n = 0 then () else wait (n - 1) in
  let c = Mtime.counter () in
  Logs.info "Starting run" Logs.unit;
  let delay1, delay2, delay3 = 10_000, 20_000, 40_000 in
  Logs.info "Start action 1 (%d)." (fun m -> m ~tags:(stamp c) delay1);
  wait delay1;
  Logs.info "Start action 2 (%d)." (fun m -> m ~tags:(stamp c) delay2);
  wait delay2;
  Logs.info "Start action 3 (%d)." (fun m -> m ~tags:(stamp c) delay3);
  wait delay3;
  Logs.info "Done." (fun m -> m ?header:None ~tags:(stamp c));
  ()

let reporter ppf =
  let report src level k fmt msgf =
    let k _ = k () in
    let with_stamp tags k ppf fmt =
      let stamp = match tags with
      | None -> None
      | Some tags -> Logs.Tag.find stamp_tag tags
      in
      let dt = match stamp with None -> 0. | Some s -> (Mtime.to_us s) in
      Format.kfprintf k ppf ("[%0+04.0fus] @[" ^^ fmt ^^ "@]@.") dt
    in
    msgf @@ fun ?header ?tags -> with_stamp tags k ppf fmt
  in
  { Logs.report = report }

let main () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Info);
  run ();
  run ();
  ()

let () = main ()
