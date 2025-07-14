let src = Logs.Src.create "a_module" ~doc:"logs from a_module"

module Log = (val Logs.src_log src : Logs.LOG)

let log_name name = Log.warn (fun m -> m "My name is: %s" name)
