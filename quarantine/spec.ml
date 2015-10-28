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

let rec pp_list pp ppf = function
  | [] -> ()
  | [x] -> pp ppf x
  | x :: xs -> fprintf ppf "%a, %a" pp x (pp_list pp) xs


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

      let pp_args = pp_list V.pp

      let pp ppf = function
        | Dep (v1,v2) -> fprintf ppf "%a/%a" V.pp v1 V.pp v2
        | Var (v,var) -> fprintf ppf "%a = %a" V.pp v Var.pp var
        | Int (v,int) -> fprintf ppf "%a = %a" V.pp v Word.pp int
        | Fun (id,[]) -> fprintf ppf "%a" Id.pp id
        | Fun (id,args) -> fprintf ppf "%a(%a)" Id.pp id pp_args args
    end)
end


module Ptr = struct
  include Ptr
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash p = V.hash p.base lxor V.hash p.off
      let module_name = None
      let pp ppf p =
        Format.fprintf ppf "%a[%a]" V.pp p.base V.pp p.off
    end)
  let create = Fields.create
end

module E = struct
  include E
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Reg v -> V.hash v
        | Ptr p -> Ptr.hash p

      let module_name = None
      let pp ppf = function
        | Reg v -> V.pp ppf v
        | Ptr p -> Ptr.pp ppf p
    end)
end

module Pat = struct
  include Pat
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Call (id,_,_) -> Id.hash id
        | Jump (_,v1,v2)
        | Move (v1,v2) -> V.hash v1 lxor V.hash v2
        | Load (v,p)
        | Store (v,p) -> V.hash v lxor Ptr.hash p
        | Wild v -> V.hash v

      let module_name = None

      let string_of_kind = function
        | `call -> "call"
        | `goto -> "goto"
        | `ret  -> "ret"
        | `exn  -> "exn"
        | `jmp  -> "jmp"

      let pp_args = pp_list E.pp

      let pp ppf = function
        | Call (id,as1,as2) ->
          fprintf ppf "%a = %a(%a)" Id.pp id pp_args as1 pp_args as2
        | Jump (k,c,d) ->
          fprintf ppf "when %a %s %a" V.pp c (string_of_kind k) V.pp d
        | Move (t,s) ->
          fprintf ppf "%a := %a" V.pp t V.pp s
        | Load (v,p)  ->
          fprintf ppf "%a := %a" V.pp v Ptr.pp p
        | Store (v,p) ->
          fprintf ppf "%a := %a" Ptr.pp p V.pp v
        | Wild v -> V.pp ppf v


    end)
end

module Rule = struct
  include Rule
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash r = Pat.hash r.pat

      let module_name = None

      let pp_constrs = pp_list Constr.pp
      let pp ppf v =
        fprintf ppf "%a, %a" Pat.pp v.pat pp_constrs v.constr

    end)
end

module Judgement = struct
  include Judgement
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash j = Id.hash j.name
      let module_name = None
      let pp ppf j = ()
    end)
end

module Definition = struct
  include Definition
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let module_name = None
      let hash d = Id.hash d.name
      let pp ppf d = ()
    end)
end

module Spec = struct
  type t = definition list
  with bin_io, compare, sexp
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let module_name = None
      let hash = Hashtbl.hash
      let pp ppf _ = ()
    end)
end
