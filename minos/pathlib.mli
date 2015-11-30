open Bap.Std

(** A path terminates if it has been [`Seen], reaches a [`Max_depth]
    or hits a [`Terminal] point *)
type exit_type = [`Seen | `Max_depth | `Terminal]

(** [state] is the local state passed to each path. I typically use it
    to preserve two things I care about: the current path, and the
    current path depth. [acc] is the global state that holds while
    traversing along all paths. I use it to store project, the total
    path counts, and any other data (such as output directory) for
    paths. Note that acc is returned by this function (so you get back
    your global state).

    [blk] is the blk to start at. [finish] is called at the
    termination of a path. [f] is called on each node except the final
    node (which is called by finish). A continuation function is
    passed in [f]: (state:'a -> 'b). This allows propagating local
    state forward. *)

val fold_paths:
sub:Sub.t ->
state:'a ->
acc:'b ->
blk:tid ->
finish:('a -> tid -> 'b -> 'b) ->
f:('a -> tid -> (state:'a -> 'b) -> 'b -> 'b) -> 'b

(** This is the variant of fold_paths that operates over the graph
    representation. Supports reverse traversal. *)
val fold_paths_graph :
?rev:bool ->
(module Graphlib.Graph
  with type edge = Graphlib.Tid.Tid.edge and
  type node = tid and
  type t = Graphlib.Tid.Tid.t) ->
sub:Graphlib.Tid.Tid.t ->
state:'a ->
acc:'b ->
blk:tid ->
finish:('a -> tid -> 'b -> 'b) ->
f:('a -> tid -> (state:'a -> 'b) -> 'b -> 'b) -> 'b

(** Operates in the same manner as fold_paths, but starts at a given
    [sub] and traverses over the callgraph. This can optionally be done
    in reverse *)
val fold_paths_prog:
?rev:bool ->
prog:Graphlib.Callgraph.t ->
state:'a ->
acc:'b ->
sub:tid ->
finish:('a -> tid -> 'b -> 'b) ->
f:('a -> tid -> (state:'a -> 'b) -> 'b -> 'b) -> 'b

(** Convert a sequence of tids to blks *)
val blks_of_path : tid seq -> Sub.t -> Blk.t seq
