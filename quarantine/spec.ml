open Core_kernel.Std
open Bap.Std
open Spec_types
open Format


module V = struct
  type t = Id.t with bin_io, compare, sexp
  include Regular.Make(struct
      type t = Id.t with bin_io, compare, sexp
      let module_name = None
      let pp = Id.pp
      let hash = Id.hash
    end)
end

let pp_list pp_sep pp_elem ppf xs =
  let rec pp ppf = function
    | [] -> ()
    | [x] -> pp_elem ppf x
    | x :: xs -> fprintf ppf "%a%t%a" pp_elem x pp_sep pp xs in
  pp ppf xs

let pp_comma ppf = fprintf ppf ",@;"
let pp_break ppf = fprintf ppf "@;"

module Constr = struct
  include Constr
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Dep (v1,v2) -> V.hash v1 lxor V.hash v2
        | Var (v,var) -> V.hash v lxor Var.hash var
        | Int (v,w) -> V.hash v lxor Word.hash w
        | Fun (id,_) -> Id.hash id

      let module_name = None

      let pp ppf = function
        | Dep (v1,v2) -> fprintf ppf "%a/%a" V.pp v1 V.pp v2
        | Var (v,var) -> fprintf ppf "%a = %a" V.pp v Var.pp var
        | Int (v,int) -> fprintf ppf "%a = %a" V.pp v Word.pp int
        | Fun (id,v) -> fprintf ppf "%a(%a)" Id.pp id V.pp v
    end)
end

module E = struct
  include E
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Reg v | Ptr v -> V.hash v

      let module_name = None
      let pp ppf = function
        | Reg v | Ptr v -> V.pp ppf v
    end)
end

module Pat = struct
  include Pat
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Call (id,_,_) -> Id.hash id
        | Jump (_,v1,v2) | Move (v1,v2) | Load (v1,v2) | Store (v1,v2) ->
          V.hash v1 lxor V.hash v2
        | Wild v -> V.hash v

      let module_name = None

      let string_of_kind = function
        | `call -> "call"
        | `goto -> "goto"
        | `ret  -> "ret"
        | `exn  -> "exn"
        | `jmp  -> "jmp"

      let pp_args ppf = pp_list pp_comma E.pp ppf

      let pp ppf = function
        | Call (id,as1,as2) ->
          fprintf ppf "%a = %a(%a)" pp_args as2 Id.pp id pp_args as1
        | Jump (k,c,d) ->
          fprintf ppf "when %a %s %a" V.pp c (string_of_kind k) V.pp d
        | Move (t,s) ->
          fprintf ppf "%a := %a" V.pp t V.pp s
        | Load (v,p)  ->
          fprintf ppf "%a := *%a" V.pp v V.pp p
        | Store (v,p) ->
          fprintf ppf "*%a := %a" V.pp p V.pp v
        | Wild v -> V.pp ppf v


    end)
end

module Rule = struct
  include Rule
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash j = Id.hash j.name
      let module_name = None

      let pp_pats = pp_list pp_comma Pat.pp

      let pp ppf r =
        fprintf ppf "@[rule %s ::=@;%a@ |-@ %a@]"
          r.name pp_pats r.premises pp_pats r.conclusions
    end)
end

module Definition = struct
  include Definition
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let module_name = None
      let hash d = Id.hash d.name

      let pp_cons = pp_list pp_break Constr.pp
      let pp_rules = pp_list pp_break Rule.pp

      let pp ppf d =
        Format.fprintf ppf "@[<v>define %s ::= @;%a@;%a@]"
          d.name  pp_cons d.constrs pp_rules d.rules
    end)
end

module Spec = struct
  type t = definition list
  with bin_io, compare, sexp
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let module_name = None
      let hash = Hashtbl.hash
      let pp_defs = pp_list pp_break Definition.pp
      let pp ppf spec =
        fprintf ppf "@[<v>%a@]" pp_defs spec
    end)
end
