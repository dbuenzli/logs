let enable () =
  let lock = Mutex.create () in
  let lock () = Mutex.lock lock
  and unlock () = Mutex.unlock lock in
  Logs.set_mutex ~lock ~unlock
