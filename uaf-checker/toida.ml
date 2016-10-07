open Core_kernel.Std
open Bap.Std
open Uaf_error

let code_of_color = function
  | `green -> 0x00ff00
  | `red -> 0x0000ff
  | `yellow -> 0x00FFFF
  | _ -> invalid_arg "unexpected color"

let comment_cmd addr c =
  sprintf "MakeComm(0x%x, %S)\n" (Addr.to_int addr |> ok_exn) c

let color_cmd addr c =
  sprintf "idaapi.set_item_color(DecodeInstruction(0x%x).ea, 0x%x)\n"
    (Addr.to_int addr |> ok_exn)
    (code_of_color c)

let output filename addrs =
  Out_channel.with_file filename ~f:(fun chan ->
      fprintf chan "from idautils import *\n\
                    Wait()\n";
      let attrs = [(`red,"~use"); (`yellow,"~free") ; (`green,"~alloc")] in
      List.iter2_exn addrs attrs ~f:(fun addr (color,comment) ->
          fprintf chan "%s" (color_cmd addr color);
          fprintf chan "%s" (comment_cmd addr comment)))

let output_all filename errors proj =
  let open Option in
  if List.length errors > 0 then
    Out_channel.with_file filename ~f:(fun chan ->
        fprintf chan "from idautils import *\n\
                      Wait()\n";
        List.iter errors ~f:(fun error ->
            (let (!) tid = Program.lookup def_t (Project.program proj) tid
                           |> Option.value_exn in
             Term.get_attr !(error.use_tid) address >>=
             fun use_addr ->
             Term.get_attr !(error.free_tid) address >>=
             fun free_addr ->
             Term.get_attr !(error.alloc_tid) address >>=
             fun alloc_addr ->
             let addrs = [use_addr; free_addr; alloc_addr] in
             let attrs = [(`red,"~use"); (`yellow,"~free") ; (`green,"~alloc")] in
             List.iter2_exn addrs attrs ~f:(fun addr (color,comment) ->
                 fprintf chan "%s" (color_cmd addr color);
                 fprintf chan "%s" (comment_cmd addr comment));
             return ()) |> ignore))
