open Core_kernel.Std
open Bap.Std
open Format
open Option

let find_section_by_name project name =
  let memory = Project.memory project in
  Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
      Option.(Value.get Image.section x >>= fun n ->
              Option.some_if (n = name) m))

let read_string mem w =
  let open Or_error.Monad_infix in
  let (!) = Char.to_string in
  Memory.view ~word_size:`r8 ~from:w mem >>= fun mem' ->
  Memory.foldi ~word_size:`r8 mem' ~init:(false,"")
    ~f:(fun addr word (fin,acc) ->
        let char = Word.enum_chars word LittleEndian |> Seq.hd_exn in
        match fin,char with
        | (false,'\x00') -> (true,acc)
        | (false,c) -> (false,acc^(!c))
        | (true,c) -> (true,acc)) |> snd |> Or_error.return

let get_rodata_str project w =
  find_section_by_name project ".rodata" >>= fun mem ->
  if Memory.contains mem w then
    match read_string mem w with
    | Ok res -> Some res
    | Error _ -> None
  else None

let try_get_rodata project addr : 'a option =
  let scale = match Arch.addr_size (Project.arch project) with
    | `r32 -> `r32
    | `r64 -> `r64 in
  find_section_by_name project ".text" >>= fun mem ->
  match Memory.get ~scale ~addr mem with
  | Ok w ->
    let get_str = get_rodata_str project in
    Option.first_some (get_str w)
      (find_section_by_name project ".got" >>= fun got ->
       let got_addr = Memory.min_addr got in
       Option.first_some
         (Addr.add addr got_addr |> get_str)
         (Addr.add w got_addr |> get_str))
  | Error _ -> None

let resolve_string project def_tid =
  Program.lookup def_t (Project.program project) def_tid >>= fun def ->
  match Def.rhs def with
  | Bil.Int w ->
    Option.first_some ((get_rodata_str) project w) (try_get_rodata project w)
  | Bil.Load (_,Bil.Int w,_,_) -> try_get_rodata project w
  | _ -> None

let stringify project sub =
  Term.map blk_t sub ~f:(fun blk ->
      Term.map def_t blk ~f:(fun def ->
          match resolve_string project @@ Term.tid def with
          | Some data -> Term.set_attr def comment (sprintf "%S" data)
          | None -> def))

let main project : project =
  let prog = Project.program project in
  (Term.map sub_t prog ~f:(fun sub -> stringify project sub) |> some)
  |> function
  | Some prog -> Project.with_program project prog
  | None -> failwith "Could not stringify!"

let () = Project.register_pass main
