open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Color

let left_justify =
  String.concat_map ~f:(fun c ->
      if c = '\n' then "\\l" else Char.to_string c)

(** filter html and turn \n into html align left *)
let format =
  String.concat_map ~f:(function
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "'&amp;"
      | '\n' -> "<BR ALIGN=\"left\"/>"
      | c -> Char.to_string c)

(** grep from the start of a tid to t a newline, and color that. *)
let color_pc ?pc node_str =
  match pc with
  | Some tid ->
    let pc = Tid.to_string tid in
    String.substr_replace_first
      ~pattern:pc ~with_:(sprintf "<b><FONT COLOR=\"yellow\">%s</FONT></b>" pc)
      node_str
  | None -> node_str

let dot_cfg ?pc ?(highlight_node=[]) sub ~filename =
  let module Cfg = Graphs.Ir in
  let cfg = Sub.to_cfg sub in

  let string_of_node node =
    sprintf "\"%s\"" @@ Blk.to_string @@ Cfg.Node.label node |> left_justify
  in

  let node_attrs node =
    let node_tid = Term.tid (Cfg.Node.label node) in
    let node_str =
      sprintf "%s" @@ Blk.to_string @@ Cfg.Node.label node
    in
    if List.Assoc.mem highlight_node node_tid then
      [`Shape `Box; `Style `Filled; `Fontcolor !White;
       `Fillcolor (List.Assoc.find_exn highlight_node node_tid);
       `HtmlLabel (node_str |> format |> color_pc ?pc);
       `Fontname "Monospace"]
    else
      [`Shape `Box; `Style `Filled; `Fillcolor !White; `Fontname "Monospace"] in
  Graphlib.to_dot (module Cfg) ~node_attrs ~string_of_node ~filename
    cfg

let save_cfg ?pc ~filename sub trace =
  let highlight_node = List.map trace ~f:(fun tid -> (tid,!Gray)) in
  dot_cfg ?pc ~highlight_node sub ~filename
