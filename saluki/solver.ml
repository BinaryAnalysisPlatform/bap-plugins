open Core_kernel.Std
open Bap.Std
open Spec
open Format
open Utilities

module SM = Monad.State
open SM.Monad_infix

class type ['a] vis = object
  method arg : arg term -> (unit,'a) SM.t
  method phi : phi term -> (unit,'a) SM.t
  method def : def term -> (unit,'a) SM.t
  method jmp : jmp term -> (unit,'a) SM.t
end

let update f = SM.get () >>= fun s -> SM.put (f s)

let iterm ~f =
  Seq.fold ~init:(SM.return ()) ~f:(fun m v ->
      m >>= fun () -> f v)

let foreach cls t ~f = Term.enum cls t |> iterm ~f

let run on mrs f t =
  let step mr =
    update (fun s -> on s; State.step s t (f mr t)) in
  Seq.of_list mrs |> iterm ~f:step

let solver prog on : 'a vis =
  let rs = Match.patterns prog in
  object
    method arg = run on rs (fun r -> r#arg)
    method phi = run on rs (fun r -> r#phi)
    method def = run on rs (fun r -> r#def)
    method jmp = run on rs (fun r -> r#jmp)
  end


let search_sub vis sub =
  foreach arg_t sub ~f:vis#arg >>= fun () ->
  foreach blk_t sub ~f:(fun blk ->
      foreach phi_t blk ~f:vis#phi >>= fun () ->
      foreach def_t blk ~f:vis#def >>= fun () ->
      foreach jmp_t blk ~f:vis#jmp)

let search (vis : 'a vis) prog =
  foreach sub_t prog ~f:(search_sub vis) >>= fun () ->
  SM.update State.start_conclusions >>= fun () ->
  foreach sub_t prog ~f:(search_sub vis)


let do_nothing _ = ()

let run ?(on_step=do_nothing) state prog =
  let solver = solver prog on_step in
  SM.exec (search solver prog) state
