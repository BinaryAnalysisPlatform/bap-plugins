
(** A cut is a subgraph that contains a call to a sink we are
    interested in. Each cut has a unique caller (and callstring up to the
    root of the program). We define cut *groups* since a single cut may
    contain multiple sinks within the final calling sub.

    E.g., bar below will form a single cut group, which calls sink1 and sink2.
          bazz forms a second cut group.

      foo
     /   \
    /    bazz
   /     -> calls sink3 on some path
bar
-> calls sink1 on some path
-> calls sink 2 on some other path

*)

open Bap.Std

type src_config =
  {src_at_root : bool;
   src_at_nth : int;
   src : string }

type cut_group = {
  (* Stores the blks that call a given source *)
  src_caller_blks : Blk.t seq;
  sink_caller_blks : Blk.t seq;
  (* The callstring from src to the lca *)
  src_callstring : tid seq;
  sink_callstring : tid seq;
  (* The sub that calls src. src_caller_blks are all contained in
     this sub *)
  src_caller_sub : Sub.t;
  sink_caller_sub : Sub.t;
  lca_sub : Sub.t; (* root *)
  lca_name : string;
  depth: int; (* max depth we would have to inline to hit this *)
  id: int; (* group id, for output *)
}

val print_cut_group : cut_group -> unit

val output_cut_group : cut_group -> unit

val cuts : project -> Graphlib.Callgraph.t -> src_config -> string -> cut_group seq
