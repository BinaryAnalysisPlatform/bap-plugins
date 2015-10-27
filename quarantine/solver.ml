open Core_kernel.Std
open Bap.Std
open Spec_types
open Spec

type t = spec


let create = ident


module Input = struct

  (* variable is defined in the scope of judgement of a rule. *)
  type t = {
    definition : definition;
    judgement  : judgement;
    var : v;
  } with bin_io, compare, fields, sexp

  include Regular.Make(struct
      type nonrec t = t with bin_io, compare, sexp
      let hash i = Id.hash (V.id i.var)
      let module_name = None
      let pp ppf d =
        Format.fprintf ppf "%s_%s_input_%s"
          (Definition.name d.definition)
          (Judgement.name d.judgement)
          (Id.to_string (V.id d.var))
    end)
end

type input = Input.t with bin_io, compare, sexp
let input = Value.Tag.register
    ~name:"solver_input"
    ~uuid:"1ab9a363-db8f-4ab4-9fb4-5ff54de97c5c"
    (module Input)

let input_of_dep = function
  | Rule.Dep (_,v) -> Some v
  | _ -> None

let inputs_of_deps rules =
  V.Set.of_list @@ List.filter_map rules ~f:input_of_dep


let inputs_of_judgement jud =
  Set.union
    (inputs_of_deps (Judgement.conclusion jud))
    (inputs_of_deps (Judgement.premises jud))

let is_target id = function
  | Indirect _ -> false
  | Direct tid -> Tid.name tid = "@"^id


(* ok, looks like that only definitions can be used as input variables. *)
let tag_call vars sub jmp call rule = match rule with
  | Rule.Call (id,_,_) when not (is_target id (Call.target call)) -> None
  | _ -> None


let collect_inputs spec =
  List.fold spec ~init:[] ~f:(fun init def ->
      List.fold (Definition.judgements def) ~init ~f:(fun init jud ->
          init))

let mark_inputs _ = ident
