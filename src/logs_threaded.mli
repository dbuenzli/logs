(** Thread safe logging.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%%}homepage}} *)

val enable : unit -> unit
(** [enable ()] enables thread-safe logging.
    It is done setting [Logs.set_mutex], setting it again will remove thread-safety. *)
