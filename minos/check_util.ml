open Bap.Std
open Core_kernel.Std

let (^::) = Seq.cons

let get_jmp_tids sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:Seq.empty ~f:(fun acc blk ->
      Term.enum jmp_t blk |> Seq.fold ~init:acc ~f:(fun acc jmp ->
          (Term.tid jmp) ^:: acc))
