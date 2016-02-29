open Core_kernel.Std
open Bap.Std
open Format

let () = Project.register_pass' ~deps:["staticstore"]
    (fun p ->
       Memmap.to_sequence (Project.memory p) |>
       Seq.fold ~init:(0,0,0) ~f:(fun (r,g,y) (_,v) ->
           match Value.get color v with
           | Some `red ->    (r+1,g,y)
           | Some `green ->  (r,g+1,y)
           | Some `yellow -> (r,g,y+1)
           | _ -> (r,g,y)) |> fun (r,g,y) ->
       let print name var =
         eprintf "%-7s %d/%d (%2.2g %%)@."
           name var (r+g+y) (100. *. (float var /. float (r+g+y))) in
       print "Green" g;
       print "Yellow" y;
       print "Red" r)
