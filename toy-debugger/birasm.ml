open Core_kernel.Std
open Bap.Std
include Self ()
open Format
open Regular.Std

let go (type t) (module T: Regular.S with type t = t) get_addr proj  =
  let f x elt =
    match get_addr elt with
    | Some addr ->
      let s = sprintf "%a: " Word.pps addr in s^x
    | None -> x in
  match T.default_printer () with
  | None -> ()
  | Some (super,_,_) ->
    let to_bytes elt = f (T.to_bytes ~fmt:super elt) elt in
    let writer = Data.Write.create ~to_bytes () in
    let name = sprintf "decorated_%s" super in
    T.add_writer name ~ver:"1.0" writer;
    T.set_default_printer name

let main proj =
  let get_addr t = Term.get_attr t address in
  go (module Def) get_addr proj;
  go (module Jmp) get_addr proj;
  proj

let () = Project.register_pass main
