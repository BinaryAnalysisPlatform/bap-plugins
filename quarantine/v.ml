open Core_kernel.Std
open Regular.Std
open Bap.Std
open Format

module Vars = struct
  let pool = Vector.create ~capacity:64 ""

  let lookup = function
    | 0 -> "_"
    | n when n < 0 -> "_"
    | n when n < Vector.length pool -> Vector.get pool n
    | _ -> "_"

  let register = function
    | "" | "_" -> 0
    | var -> match Vector.index ~equal:String.equal pool var with
      | Some i -> i
      | None ->
        Vector.append pool var;
        Vector.length pool - 1
end

type t = int [@@deriving bin_io, compare, sexp]

let null = 0
let create = Vars.register
include Regular.Make(struct
    type t = int [@@deriving bin_io, compare, sexp]
    let module_name = None
    let pp ppf idx = fprintf ppf "%s" (Vars.lookup idx)
    let hash = ident
    let version = "0.1"
  end)
