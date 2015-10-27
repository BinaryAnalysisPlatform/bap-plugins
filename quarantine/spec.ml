open Core_kernel.Std
open Bap.Std
open Spec_types


module Constr = struct
  include Constr
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash c = Exp.hash c.exp
      let module_name = None
      let pp ppf e = match e.value with
        | None -> Exp.pp ppf e.exp
        | Some w -> Format.fprintf ppf "%a=%a"
                      Exp.pp e.exp Word.pp w
    end)
end

module V = struct
  include V
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash v = Id.hash v.id
      let module_name = None

      let pp_id ppf id =
        Format.fprintf ppf "%s"
          (Id.to_string id)

      let pp ppf v = match v.constr with
        | None -> pp_id ppf v.id
        | Some c -> Format.fprintf ppf "%a(%a)"
                      pp_id v.id Constr.pp c
    end)
end

module P = struct
  include P
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash p = V.hash p.base lxor V.hash p.off
      let module_name = None
      let pp ppf p =
        Format.fprintf ppf "%a[%a]" V.pp p.base V.pp p.off
    end)
  let create = P.Fields.create
end

module E = struct
  include E
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Reg v -> V.hash v
        | Ptr (base,off) -> P.hash @@ P.create ~base ~off

      let module_name = None
      let pp ppf = function
        | Reg v -> V.pp ppf v
        | Ptr (b,a) -> P.pp ppf @@ P.create ~base:b ~off:a
    end)
end

module Rule = struct
  include Rule
  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash = function
        | Pred (id,_) | Call (id,_,_) ->
          Id.hash id
        | Jump (v1,v2) | Dep (v1,v2) | Move (v1,v2) ->
          V.hash v1 lxor V.hash v2
        | Load (v,p) | Store (v,p) ->
          V.hash v lxor P.hash p

      let module_name = None
      let pp ppf v = ()

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
