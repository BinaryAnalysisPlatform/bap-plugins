open Core_kernel.Std
open Bap.Std

(** A graph.

    This is the most trivial implementation of an interface that is
    required by [Graph.Graphviz.Dot] functor.  Basically it takes a
    [Block.Cfg] module and extends it with few functions, required by
    [Dot]. This functions allow one to control visual aspect of the
    graph.

    The most important (and the only one non-trivial) is [vertex_name]
    that returns a name of a block. Make sure that you properly
    delimit the name with quotes, otherwise dot will fail. ("%S"
    specifier, will do the delimiting).

*)
module Callgraph = struct
  include Block.Cfg
  let vertex_name b =
    sprintf "%S" (Addr.string_of_value (Block.addr b))
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end

(** Cmdline interface.

    This module implements command line interface. It defines only one
    term [symbol] that is required and evaluates to a value of type
    [string].
*)
module Cmdline = struct
  open Cmdliner
  let symbol : string Term.t =
    let doc = "symbol name for which to build the CFG" in
    Arg.(required & opt (some string) None &
         info ["symbol"] ~doc)

  let info =
    Term.info ~doc:"Output a CFG for a given graph" "cfg"

  let parse argv : string =
    match Term.eval ~argv (symbol,info) with
    | `Ok sym -> sym
    | _ -> exit 1
end

(** Plugin entry point.

    We instantiate a Dot module, get command line argument,
    lookup for a so named function in a symbol table of a loaded
    binary and print the graph.
*)
let main argv proj =
  let module Dot = Graph.Graphviz.Dot(Callgraph) in
  let symbol = Cmdline.parse argv in
  let symbols = Project.symbols proj in
  match Symtab.find_by_name symbols symbol with
  | None -> eprintf "No such symbol in binary: %s\n" symbol
  | Some fn ->
    let name = sprintf "%s.dot" (String.escaped symbol) in
    let bound = unstage (Symtab.create_bound symbols fn) in
    let entry = Symtab.entry_of_fn fn in
    let _,graph = Block.to_graph ~bound entry in
    Out_channel.with_file name ~f:(fun chan ->
        Dot.output_graph chan graph)

(* Do not forget to register your plugin. *)
let () = Project.register_pass_with_args' "print-cfg" main
