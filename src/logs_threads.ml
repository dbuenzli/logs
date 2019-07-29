let thread_safe_reporter reporter =
  let lock = Mutex.create () in
  let { Logs.report } = reporter in
  let report src level ~over k msgf =
    Mutex.lock lock;
    let x = report src level ~over k msgf in
    Mutex.unlock lock;
    x
  in
  { Logs.report }
