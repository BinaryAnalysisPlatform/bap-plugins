open Core_kernel.Std
open Bap.Std
open Spec_types

module SM = Monad.State
open SM.Monad_infix

let calls_of_spec spec = []

class context p k  = object
  val k = k
  inherit Taint.context
  inherit Biri.context p
  method step = if k > 0 then Some {< k = k - 1 >} else None
end

class ['a] tainteval spec = object
  inherit ['a] Tainter.call (calls_of_spec spec)
  inherit ['a] Propagator.t
  inherit ['a] Sanitizer.jmp
end

class ['a] main spec = object
  constraint 'a = #context
  inherit ['a] tainteval spec as super
  method! eval_jmp jmp =
    SM.get () >>= fun ctxt ->
    match ctxt#step with
    | None -> SM.return ()
    | Some ctxt ->
      SM.put ctxt >>= fun () ->
      super#eval_jmp jmp
end

exception Starting_point_not_found

let run_from_tid p (biri : 'a #main) tid =
  match Program.lookup sub_t p tid with
  | Some sub -> biri#eval_sub sub
  | None -> raise Starting_point_not_found

let tid_of_name str =
  match Tid.from_string ("@"^str) with
  | Ok tid -> tid
  | Error _ -> raise Starting_point_not_found

let tid_of_ident mapping = function
  | `Term tid -> tid
  | `Name str -> tid_of_name str
  | `Addr add -> match mapping add with
    | None -> raise Starting_point_not_found
    | Some tid -> tid

let run_from_point mapping p biri point =
  run_from_tid p biri (tid_of_ident mapping point)

let run p k spec point =
  let ctxt = new context p k in
  let biri = new main spec in
  let map _ = None in
  let res = run_from_point map p biri point in
  SM.exec res ctxt |> Taint.compute_result
