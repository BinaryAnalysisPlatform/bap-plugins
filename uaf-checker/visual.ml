open Bap.Std
open Core_kernel.Std
open Uaf_error

class marker (error : Uaf_error.t) = object(self)
  inherit Term.mapper as super
  method! map_term cls t =
    super#map_term cls t |> fun t ->
    (* Highlight everything visited in the trace *)
    (if List.mem error.trace (Term.tid t)
     then Term.set_attr t foreground `cyan
     else t) |> fun t ->
    (* Highlight use site *)
    (if Term.tid t = error.use_tid
     then Term.set_attr t background `red
          |> fun t -> Term.set_attr t foreground `white
     else t) |> fun t ->
    (* Highlight free site *)
    (if Term.tid t = error.free_tid
     then Term.set_attr t background `yellow
          |> fun t -> Term.set_attr t foreground `white
     else t) |> fun t ->
    (* Highlight alloc site *)
    if Term.tid t = error.alloc_tid
    then Term.set_attr t background `green
         |> fun t -> Term.set_attr t foreground `white
    else t
end

let tag_visited proj error =
  let marker = new marker error in
  Project.program proj |> marker#run |> Project.with_program proj
