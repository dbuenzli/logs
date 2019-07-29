(** Thread support for {!Logs}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%%}homepage}} *)

val thread_safe_reporter : Logs.reporter -> Logs.reporter
(** Make a reporter safe to be used in threaded programs. *)
