open Core_kernel.Std
open Format

let to_csv ?filename data =
  let open Out_channel in
  let write chan =
    output_lines chan @@ List.map data ~f:(String.concat ~sep:",") in
  match filename with
  | Some filename -> with_file filename ~f:write
  | None -> write stdout
