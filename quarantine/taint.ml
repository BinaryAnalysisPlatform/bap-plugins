open Core_kernel.Std
open Bap.Std
open Spec_types


type taint = {
  term : tid;
  host : host;
} with bin_io, compare, sexp, fields

let create host term = {host; term}

include Regular.Make(struct
    type t = taint with compare, bin_io, sexp
    let hash t = Tid.hash t.term
    let module_name = Some "Taint"

    open Format

    let rec pp ppf t =
      fprintf ppf "%a:%a" Tid.pp t.term pp_host t.host
    and pp_call ppf (name,_) = fprintf ppf "%s" name
    and pp_read ppf = function
      | `Var v -> pp_var ppf v
      | `Mem e -> pp_mem ppf e
    and pp_seq ppf (h1,h2) =
      fprintf ppf "%a..%a" pp_host h1 pp_host h2
    and pp_host ppf = function
      | `Call c -> pp_call ppf c
      | `Read e -> pp_read ppf e
      | `Seq s  -> pp_seq ppf s
    and pp_mem ppf (v,n) = fprintf ppf "%a[%d]" pp_var v n
    and pp_var ppf = function
      | `Reg v -> Var.pp ppf v
      | `Pos n -> fprintf ppf "$%d" n
      | `Ret   -> fprintf ppf "$?"
      | `All   -> fprintf ppf "$*"
  end)

type t = taint
type taints = Set.t
