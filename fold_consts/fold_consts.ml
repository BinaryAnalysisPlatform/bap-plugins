open Core_kernel.Std
open Bap.Std
open Parameters
open Cmdliner
include Self()

let doc = "Perform global constant folding on each function"

let man = [
  `S "DESCRIPTION";
  `P "This plugin will perform a constant propagation on each \
      subroutine. Plugin operates on the IR level. The SSA form\
      is required. If program is not in SSA, theт it will be \
      transformed. A propagated definition is not removed from \
      a subroutine, since we can't know for sure, that it is not\
      used outside of the subroutine."
]

let info = Term.info ~man ~doc "fold_consts"

let fix_sp : string option option Term.t =
  let doc = "Fix stack pointer to a constant value. If stack \
             pointer address is not specified, then an address that \
             is 1GB above the maximum address is chosen" in
  Arg.(value & opt (some (some string)) ~vopt:(Some None)
         options.fix_sp & info ["fix-sp"] ~doc)

module Perm = struct
  type t = [`no | `ro | `rw] [@@deriving sexp]
  let to_string x = Sexp.to_string (sexp_of_t x)
  let variants = List.map [`no;`ro;`rw] ~f:(fun x -> to_string x, x)
end

let resolve_loads : Perm.t Term.t =
  let doc = sprintf
      "Resolves loads from memory addresses that are mapped from \
       a provided file. Accepted values are %s. If set to `%s' \
       then only loads from a write-protected memory are \
       considered. If set to %s then all loads are resolved. To \
       disable accessing to the static memory say `%s'"
      (Arg.doc_alts_enum Perm.variants)
      (Perm.to_string `ro) (Perm.to_string `rw) (Perm.to_string `no) in
  Arg.(value & opt (enum Perm.variants) ~vopt:`ro
         options.resolve_loads & info ["resolve-loads"] ~doc)

let set_options fix_sp resolve_loads =
  options.fix_sp <- fix_sp;
  options.resolve_loads <- resolve_loads

let top = Term.(pure set_options $fix_sp $resolve_loads)
let main proj = match Term.eval ~argv (top,info) with
  | `Ok () -> Main.run proj
  | _ -> invalid_arg "Bad user input"

let () = Project.register_pass main
