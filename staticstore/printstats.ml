open Core_kernel.Std
open Bap.Std
open Program_visitor
open Format

let () = register' (fun p ->
    Memmap.to_sequence p.annots |>
    Seq.fold ~init:(0,0,0) ~f:(fun (r,g,y) (_,(tag,color)) ->
        if tag = "staticstore" then match color with
          | "red" ->    (r+1,g,y)
          | "green" ->  (r,g+1,y)
          | "yellow" -> (r,g,y+1)
          | c -> invalid_arg c
        else (r,g,y)) |> fun (r,g,y) ->
    let print name var =
      eprintf "%-7s %d/%d (%2.2g %%)@."
        name var (r+g+y) (100. *. (float var /. float (r+g+y))) in
    print "Green" g;
    print "Yellow" y;
    print "Red" r)
