open Core_kernel.Std
open Regular.Std
open Bap.Std
open Spec_types
open Format

module Id = String
type id = Id.t
  [@@deriving bin_io, compare, sexp]

let pp_list pp_sep pp_elem ppf xs =
  let rec pp ppf = function
    | [] -> ()
    | [x] -> pp_elem ppf x
    | x :: xs -> fprintf ppf "%a%t%a" pp_elem x pp_sep pp xs in
  pp ppf xs

let pp_comma ppf = fprintf ppf ","
let pp_break ppf = fprintf ppf "@;"

module Constr = struct
  include Constr
  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let hash = function
        | Dep (v1,v2) -> V.hash v1 lxor V.hash v2
        | Var (v,var) -> V.hash v lxor Var.hash var
        | Fun (id,_) -> Id.hash id

      let module_name = None

      let pp ppf = function
        | Dep (v1,v2) -> fprintf ppf "%a/%a" V.pp v1 V.pp v2
        | Var (v,var) -> fprintf ppf "%a=%a" V.pp v Var.pp var
        | Fun (id,v) -> fprintf ppf "%a(%a)" Id.pp id V.pp v
    end)
end

module S = struct
  include S
  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let hash = Hashtbl.hash

      let module_name = None
      let pp ppf = function
        | Reg -> fprintf ppf "reg "
        | Ptr -> fprintf ppf "reg *"
    end)
end

module Pat = struct
  include Pat

  (** substitutes each variable that is not in the set of constrained
      variables with a `_`. *)
  let nullify cvars pat =
    let f v = if Set.mem cvars v then v else V.null in
    match pat with
    | Pat.Never -> Pat.Never
    | Pat.Jump  (k,x,y) -> Pat.Jump (k, f x, f y)
    | Pat.Load  (x,y) -> Pat.Load (f x, f y)
    | Pat.Store (x,y) -> Pat.Store (f x, f y)
    | Pat.Move  (x,y) -> Pat.Move (f x, f y)
    | Pat.Wild x -> Pat.Wild (f x)
    | Pat.Call (id,r,xs) -> Pat.Call (id,f r, List.map xs ~f)

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let hash = function
        | Never -> Hashtbl.hash Never
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

      let pp_args ppf = pp_list pp_comma V.pp ppf
      let pp_ret ppf v = match (v : v :> int) with
        | 0 -> ()
        | e -> fprintf ppf "%a := " V.pp v

      let pp ppf = function
        | Never -> fprintf ppf "never"
        | Call (id,def,uses) ->
          fprintf ppf "%a%a(%a)" pp_ret def Id.pp id pp_args uses
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
  type t = {
    name : string;
    premises : pat list;
    conclusions : pat list;
  } [@@deriving bin_io, compare, fields, sexp]


  let nullify rule cvars = {
    rule with
    premises = List.map rule.premises ~f:(Pat.nullify cvars);
    conclusions = List.map rule.conclusions ~f:(Pat.nullify cvars);
  }

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let hash j = Id.hash j.name
      let module_name = None

      let pp_pats = pp_list pp_comma Pat.pp

      let pp ppf r =
        fprintf ppf "@[<4>rule %s ::=@;%a |-@ %a@]"
          r.name pp_pats r.premises pp_pats r.conclusions
    end)
end

type rule = Rule.t [@@deriving bin_io, compare, sexp]

module Defn = struct
  type t = {
    name : string;
    vars : (v * s) list;
    constrs : constr list;
    rules : rule list
  } [@@deriving bin_io, compare, fields, sexp]

  let ivars constrs =
    List.fold constrs ~init:V.Set.empty ~f:(fun ivars cs ->
        match cs with
        | Constr.Dep (_,v2) -> Set.add ivars v2
        | _ -> ivars)

  let assert_all_defined cvars vars =
    Set.find cvars ~f:(fun v -> not (List.Assoc.mem vars v)) |> function
    | None -> ()
    | Some v -> invalid_argf "Undefined variable %a" V.pps v ()

  let assert_ivars_defined ivars rules =
    let undef p v = Option.some_if (Set.mem ivars v) (p,v) in
    List.find_map rules ~f:(fun r ->
        List.find_map (Rule.premises r @ Rule.conclusions r)
          ~f:(fun p -> match p with
              | Pat.Move (_,v)
              | Pat.Load (_,v)
              | Pat.Wild v
              | Pat.Store (_,v) -> undef p v
              | Pat.Jump (_,x,y) ->
                Option.first_some (undef p x) (undef p y)
              | Pat.Never | Pat.Call _ -> None)) |> function
    | None -> ()
    | Some (p,v) ->
      invalid_argf "Variable %a bound in %a can't be used as input"
        V.pps v Pat.pps p ()

  let create name vars constrs rules =
    let cvars =
      List.fold constrs ~init:V.Set.empty ~f:(fun cvars cs ->
          match cs with
          | Constr.Dep (v1,v2) -> Set.add (Set.add cvars v1) v2
          | Constr.Var (v,_)
          | Constr.Fun (_,v) -> Set.add cvars v) in
    let ivars = ivars constrs in
    let rules = List.map rules ~f:(fun r -> Rule.nullify r cvars) in
    let vars = List.filter vars ~f:(fun (v,_) -> Set.mem cvars v) in
    assert_all_defined cvars vars;
    assert_ivars_defined ivars rules;
    {name; vars; constrs; rules}

  let ivars t = ivars t.constrs

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let module_name = None
      let hash d = Id.hash d.name

      let pp_decl ppf (v,t) = S.pp ppf t; V.pp ppf v
      let pp_vars p = pp_list pp_comma pp_decl p

      let pp_cons p = pp_list pp_comma Constr.pp p
      let pp_rules p = pp_list pp_break Rule.pp p

      let pp_variables ppf d =
        fprintf ppf "@[<2>vars@ @[{%a}@]@]"
          pp_vars d.vars
      let pp_constrs ppf d =
        fprintf ppf "@[<2>s.t.@ @[{%a}@]@]"
          pp_cons d.constrs

      let pp ppf d =
        fprintf ppf "@[<v2>define %s ::= @;%a@;%a@;%a@]@;"
          d.name pp_rules d.rules pp_variables d pp_constrs d
    end)
end

type defn = Defn.t [@@deriving bin_io, compare, sexp]

module Spec = struct
  type t = defn list
    [@@deriving bin_io, compare, sexp]

  let create defs =
    let compare x y = String.compare (Defn.name x) (Defn.name y) in
    match List.find_a_dup ~compare defs with
    | Some dup ->
      invalid_argf "Multiple definitions of %s" (Defn.name dup) ()
    | None -> defs

  let defns = ident

  let filter = List.filter

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let module_name = None
      let hash = Hashtbl.hash
      let pp_defs ppf spec = pp_list pp_break Defn.pp ppf spec
      let pp ppf spec =
        fprintf ppf "@[<v2>Specification ::= @;%a@]@." pp_defs spec
    end)
end

type constr = Constr.t [@@deriving bin_io, compare, sexp]
type v    = V.t    [@@deriving bin_io, compare, sexp]
type s    = S.t    [@@deriving bin_io, compare, sexp]
type pat  = Pat.t  [@@deriving bin_io, compare, sexp]
type spec = Spec.t [@@deriving bin_io, compare, sexp]


module Language = struct

  let id = ident
  let var = V.create

  let a = var "a"
  let b = var "b"
  let c = var "c"
  let d = var "d"
  let p = var "p"
  let q = var "q"
  let r = var "r"
  let s = var "s"
  let t = var "t"
  let u = var "u"
  let v = var "v"
  let x = var "x"
  let y = var "y"
  let z = var "z"

  let _' = var ""
  let a' = var "a'"
  let b' = var "b'"
  let c' = var "c'"
  let d' = var "d'"
  let p' = var "p'"
  let q' = var "q'"
  let r' = var "r'"
  let s' = var "s'"
  let t' = var "t'"
  let u' = var "u'"
  let v' = var "v'"
  let x' = var "x'"
  let y' = var "y'"
  let z' = var "z'"


  let is_marked = "is_marked"
  let is_black = "is_black"
  let is_red = "is_red"
  let is_green = "is_green"
  let is_yellow = "is_yellow"
  let is_blue = "is_blue"
  let is_magenta = "is_magenta"
  let is_cyan = "is_cyan"
  let is_white = "is_white"


  type dec = v * s


  let reg  : v -> dec = fun v -> v,S.Reg
  let ( * ) : (v -> dec) -> v -> dec = fun _t -> fun v -> v,S.Ptr

  type that = That type such = Such type vars = Vars
  let that = That
  let such = Such
  let vars = Vars


  let forall v Such That id = Constr.Fun (id,v)
  let (/) y x = Constr.dep y x
  let (=) v var = Constr.var v var


  let specification = Spec.create

  let define name rules Vars vars Such That constrs =
    Defn.create name vars constrs rules

  let rule name premises conclusions =
    Rule.Fields.create ~name ~premises ~conclusions

  let case cond jmp dst = jmp cond dst
  let goto = Pat.jump `goto
  let ret = Pat.jump `ret
  let exn = Pat.jump `exn
  let jmp = Pat.jump `jmp

  type rhs = v -> pat

  let use rhs lhs = Pat.move lhs rhs
  let any = Pat.wild
  let sub id args = Pat.call id V.null args
  let call id args (ret : v) = match (ret :> int) with
    | 0 -> sub id args
    | _ -> Pat.call id ret args
  let (:=) lhs term = term lhs

  let never = Pat.never
end
