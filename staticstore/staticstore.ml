open Core_kernel.Std
open Bap.Std
include Self()
open Format
open Option.Monad_infix

let version = "0.2"
let max_exp_size = ref 100
let yellow_format = ref "$symbol YELLOW\n"
let red_format = ref "$symbol RED\n"
let green_format = ref "$symbol GREEN\n"

let make_printer fmt sym =
  let b = Buffer.create 16 in
  Buffer.add_substitute b (function
      | "symbol" -> sym
      | s -> s) (Scanf.unescaped !fmt);
  Buffer.contents b

let yellow = make_printer yellow_format
let red = make_printer red_format
let green = make_printer green_format

module Cmdline = struct
  open Cmdliner

  let max_size : int Term.t =
    let doc = "Limit maximum depth of expression" in
    Arg.(value & opt int !max_exp_size & info ["max-exp-size"] ~doc)

  let print : [`green | `yellow | `red] list Term.t =
    let variants = [
      "green", `green;
      "yellow", `yellow;
      "red", `red
    ] in
    let doc = sprintf
        "Print result. Accepted values are %s. Multiple variants \
         can be specified by enumerating this option several times" @@
      Arg.doc_alts_enum variants in
    Arg.(value & opt_all (enum variants) [] & info ["print"] ~doc)

  let format name default : string Term.t =
    let doc = sprintf "Print %s using specified format. \
                       Every occurence of $symbol is substituted with\
                       a symbol name" name in
    Arg.(value & opt string default &
         info [sprintf "%s-format" name] ~doc)

  let info =
    let doc = "classify all functions based on memory store \
               operations. If address of a store operation \
               is known, then it would be classified as safe \
               or green." in
    let man = [
      `S "DESCRIPTION";
      `P "This plugin will classify all functions into three categories:";
      `Noblank;
      `P "- red;"; `Noblank;
      `P "- yellow;"; `Noblank;
      `P "- green.";
      `P
        "`green` functions perform all writes to memory \
         only to a statically known offsets, i.e., a compile \
         time constants. SP is also considered a constant \
         iff it is only defined with constant in the ENTRY or EXIT \
         blocks. If it is defined by a non-constant value, like \
         `SP := SP - R0` it is considered unsafe. If it is defined \
         by a constant value, but outside of the ENTRY or EXIT \
         blocks, but the overall function is classified as green, \
         then such function will be classified as `yellow`.:"
    ] in
    Term.info ~man ~doc "staticstore" ~version

  let options size green yellow red printers =
    max_exp_size := size;
    List.iter [green_format; yellow_format; red_format] ~f:(fun fmt ->
        fmt := "");
    List.iter printers ~f:(function
        | `green -> green_format := green;
        | `yellow -> yellow_format := yellow;
        | `red -> red_format := red)

  let main =
    Term.(pure options $max_size
          $format "green" !green_format
          $format "yellow" !yellow_format
          $format "red" !red_format
          $print)

  let eval argv =
    match Term.eval ~argv (main,info) with
    | `Ok () -> ()
    | _ -> exit 1

end

let is_safe = function
  | Bil.Int _ -> true
  | _ -> false


let collect_unsafe sub =
  Term.enum blk_t sub |> Seq.fold ~init:[] ~f:(fun unsafe blk ->
      Term.enum def_t blk |>
      Seq.fold ~init:unsafe ~f:(fun unsafe def ->
          Exp.fold ~init:unsafe (object
            inherit [exp list] Exp.visitor
            method! enter_store ~mem:_ ~addr ~exp:_ _ _ unsafe =
              if is_safe addr then unsafe
              else addr :: unsafe
          end) (Def.rhs def)))

let stub_names = [".plt"; "__symbol_stub"; "__picsymbol_stub"]

let entry_of_sub proj sub =
  let syms = Project.symbols proj in
  Term.first blk_t sub >>= fun entry ->
  Term.get_attr entry Disasm.block >>= fun addr ->
  Symtab.find_by_start syms addr >>| snd3


let stubs proj : unit memmap =
  Project.memory proj |> Memmap.to_sequence |>
  Seq.filter_map ~f:(fun (mem,tag) -> match Value.get Image.section tag with
      | Some name when List.mem stub_names name -> Some mem
      | _ -> None) |>
  Seq.fold ~init:Memmap.empty ~f:(fun map mem -> Memmap.add map mem ())

let main proj =
  Cmdline.eval argv;
  let arch = Project.arch proj in
  let module Target = (val target_of_arch arch) in
  let prog = Project.program proj in
  let annotate sub mem : project =
    let sym = Sub.name sub in
    let mark = match collect_unsafe sub with
      | [] -> print_string (green sym); `green
      | exps  -> print_string (red sym); `red in
    Project.tag_memory proj (Block.memory mem) color mark in
  let stubs = stubs proj in
  let is_plt blk = Memmap.contains stubs (Block.addr blk) in
  Term.enum sub_t prog |> Seq.fold ~init:proj ~f:(fun proj sub ->
      match entry_of_sub proj sub with
      | None -> proj
      | Some entry when is_plt entry -> proj
      | Some entry -> annotate sub entry)

let () = Project.register_pass main
