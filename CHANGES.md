v0.5.0 2016-01-07 La Forclaz (VS)
---------------------------------

* Support for OCaml 4.01.0
* Change the logging structure from `Logs.err fmt (fun m -> m ...)`
  to `Logs.err (fun m -> m fmt ...)`. See the documentation basics
  for more details. Thanks to Edwin Török for suggesting this.
* Remove the `Logs.unit[_msgf]` functions, they are no longer needed.
* Rename the `Logs_stdo` library to `Logs_fmt`.
* Changes the signature of reporters to take a callback function to
  call unconditionally once the report is over. Thanks to Edwin Török
  for suggesting the mecanism.
* Add the optional `Logs_lwt` library. Provides logging functions
  returning `lwt` threads that proceed only once the report is over.
* Add `Logs_fmt.pp_header` and `Logs_fmt.{err_warn,info_debug}_style`.
* Add `Logs.pp_{level,header}`.


v0.4.2 2015-12-03 Cambridge (UK)
--------------------------------

First release.
