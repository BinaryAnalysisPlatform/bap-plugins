open Core_kernel.Std
open Bap.Std


type t = {
  sat : 'a. 'a term -> var -> bool
}

exception Unbound_predicate of string with sexp


let preds = String.Table.create ()


let register name (p : t) =
  Hashtbl.add_exn preds ~key:name ~data:p


let lookup = Hashtbl.find preds

let is_marked = {
  sat = fun t _ -> Term.has_attr t mark
}

let has_color c = {
  sat = fun t _ -> match Term.get_attr t color with
    | Some c' -> c = c'
    | _ -> false
}

let test name term var =
  match lookup name with
  | None -> raise (Unbound_predicate name)
  | Some {sat} -> sat term var


let () =
  register "is_marked" is_marked;
  register "is_black" (has_color `black);
  register "is_red" (has_color `red);
  register "is_green" (has_color `green);
  register "is_yellow" (has_color `yellow);
  register "is_blue" (has_color `blue);
  register "is_magenta" (has_color `magenta);
  register "is_cyan" (has_color `cyan);
  register "is_white" (has_color `white);
