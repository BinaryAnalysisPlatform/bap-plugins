open Core_kernel.Std
open Bap.Std
open Cabs


type arg_size =
  | Word                        (** same size as CPU word  *)
  | Size of Size.t              (** the specified size     *)

type pos =
  | Ret_0
  | Ret_1
  | Arg of int

type arg = {
  arg_name : string;
  arg_pos  : pos;
  arg_intent : intent option;
  arg_size : arg_size;
}

type fn_proto = arg list

include struct
  let stack mem sp endian sz off =
    let width = Size.to_bits sz in
    let off = Word.of_int ~width (off * (Size.to_bytes sz)) in
    let mem = Bil.var mem in
    let addr = if Word.is_zero off
      then Bil.(var sp)
      else Bil.(var sp + int off) in
    Bil.load ~mem ~addr endian sz

  let arm_stack = ARM.CPU.(stack mem sp LittleEndian `r32)
  let x86_stack = IA32.CPU.(stack mem sp LittleEndian `r32)
  let x64_stack = AMD64.CPU.(stack mem sp LittleEndian `r64)

  open Bil
  let abi = function
    | #Arch.arm ->
      ARM.CPU.(function
          | Ret_0 -> var r0
          | Ret_1 -> var r1
          | Arg 0 -> var r0
          | Arg 1 -> var r1
          | Arg 2 -> var r2
          | Arg 3 -> var r3
          | Arg n -> arm_stack Int.(n-4))
    | `x86_64 ->
      AMD64.CPU.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg 0 -> var rdi
          | Arg 1 -> var rsi
          | Arg 2 -> var rdx
          | Arg 3 -> var rcx
          | Arg 4 -> var r.(0)
          | Arg 5 -> var r.(1)
          | Arg n -> x64_stack Int.(n-6))
    | `x86 ->
      IA32.CPU.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg n -> x86_stack Int.(n+1))
    | _ -> raise Not_found
end

let intent_of_type = function
  | PTR (CONST _) -> Some In
  | PTR (_) -> Some Both
  | _ -> Some In

let size_of_type typ = match typ with
  | PTR _ -> Word
  | CHAR _ -> Size `r8
  | INT (SHORT,_) -> Size `r16
  | INT (LONG_LONG,_) -> Size `r64
  | FLOAT _ -> Size `r32
  | DOUBLE false -> Size `r64
  | DOUBLE true -> Size `r128
  | _ -> Word

let arg_of_name n = function
  | (_,VOID,_,_) -> None
  | (name,typ,_,_) -> Some {
      arg_pos = Arg n;
      arg_name = if name <> "" then name else sprintf "x%d" (n+1);
      arg_intent = intent_of_type typ;
      arg_size = size_of_type typ;
    }

let string_of_single_name n (_,_,name) = arg_of_name n name

let args_of_single_names = List.filter_mapi ~f:string_of_single_name

let ret_word n = {
  arg_pos = n;
  arg_name = if n = Ret_1 then "result_ext" else "result";
  arg_intent = Some Out;
  arg_size = Word; (* compiler will cast return value itself *)
}

let push = List.map ~f:(fun a -> match a with
    | {arg_pos = Arg n} -> {a with arg_pos = Arg (n+1)}
    | a -> a)

let fn_of_definition = function
  | DECDEF (_,_,[(name, PROTO (ret,args,false),[],NOTHING)])
  | FUNDEF ((_,_,(name, PROTO (ret,args,false),[], NOTHING)), _) ->
    let args = args_of_single_names args in
    let args = match ret with
      | VOID -> args
      | STRUCT _ | CONST (STRUCT _) ->
        {(ret_word (Arg 0)) with arg_intent = Some In} :: push args
      | INT (LONG_LONG,_) -> args @ [ret_word Ret_0; ret_word Ret_1]
      | _ -> args @ [ret_word Ret_0] in
    Some (name,args)
  | _ -> None

let fns_of_definitions =
  List.fold ~init:String.Map.empty ~f:(fun fns defn ->
      match fn_of_definition defn with
      | None -> fns
      | Some (name,data) -> Map.add fns ~key:name ~data)

let args_of_file file =
  let open Frontc in
  match Frontc.parse_file file stderr with
  | PARSING_ERROR -> raise Parsing.Parse_error
  | PARSING_OK ds -> fns_of_definitions ds

let (>:) s1 s2 e =
  match Size.(to_bits s2 - to_bits s1) with
  | 0 -> e
  | n when n > 0 -> Bil.(cast high n e)
  | n -> Bil.(cast low n e)

let term_of_arg arch sub {arg_name; arg_size; arg_intent; arg_pos} =
  let word_size = Arch.addr_size arch in
  let size = match arg_size with
    | Word -> (word_size :> Size.t)
    | Size size -> size in
  let typ = Type.imm (Size.to_bits size) in
  let exp = try abi arch arg_pos with
      exn -> Bil.unknown "unkown abi" typ in
  (* so far we assume, that [abi] returns expressions of word size,
     if this will ever change, then we need to extend abi function
     to return the size for us. *)
  let exp = (word_size >: size) exp in
  let var = Var.create (Sub.name sub ^ "_" ^ arg_name) typ in
  Arg.create ?intent:arg_intent var exp

let fill_args arch fns program =
  Term.map sub_t program ~f:(fun sub ->
      match Map.find fns (Sub.name sub) with
      | None -> sub
      | Some args ->
        List.fold args ~init:sub ~f:(fun sub arg ->
            Term.append arg_t sub (term_of_arg arch sub arg)))

module Cmdline = struct
  include Cmdliner
  let file : string option Term.t =
    let doc = "C header with function prototypes" in
    Arg.(value & opt (some file) None & info ["file"] ~doc)

  let doc =
    "Extract C function prototypes from a specified file,
            infer arguments and fill in arg terms in matching \
     subroutines"

  let parse argv =
    let info = Term.info ~doc "header" in
    let spec = Term.(pure ident $file) in
    match Term.eval ~argv (spec,info) with
    | `Ok Some file -> file
    | `Ok None -> invalid_arg "Please, specify file"
    | _ -> assert false
end

let main argv proj =
  let prog = Project.program proj in
  let arch = Project.arch proj in
  let file = Cmdline.parse argv in
  let args = args_of_file file in
  fill_args arch args prog |>
  Project.with_program proj


let () = Project.register_pass_with_args "header" main
