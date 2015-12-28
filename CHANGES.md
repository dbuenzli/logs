

v0.5.0
------

* The message formatting string was moved inside the continuation.
  The logging structure changes from `Logs.err fmt (fun m -> m ...)`
  to `Logs.err (fun m -> m fmt ...)`. This makes the logging structure
  easier to write, more regular and avoids the needs for
  `Logs.unit[_msgf]` which are dropped from the API. Thanks to Edwin
  Török for suggesting.


v0.4.2 2015-12-03 Cambridge (UK)
--------------------------------

First release.
