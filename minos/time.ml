open Bap.Std
open Core_kernel.Std

let time tag f =
  let t = Unix.gettimeofday () in
  let res = Lazy.force f in
  Format.printf "[%s] Execution time: %f seconds\n%!" tag
    (Unix.gettimeofday () -. t);
  res

let test =
  let f = lazy (Unix.sleep 3) in
  time "test" @@ f
