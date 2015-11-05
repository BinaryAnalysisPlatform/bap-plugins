open Core_kernel.Std
open Bap.Std
open Cabs


type arg_size =
  | Word                        (** same size as CPU word  *)
  | Size of Size.t              (** the specified size     *)

type arg = {
  arg_name : string;
  arg_intent : intent option;
  arg_size : arg_size;
}

type fn_proto = {
  args : arg list;
  ret  : arg option;
}

include struct
  open Bil
  let abi = function
    | #Arch.arm -> ARM.CPU.[|var r0; var r1; var r2; var r3|]
    | `x86_64 -> AMD64.CPU.[|
        var rdi; var rsi; var rdx;
        var rcx; var r.(0); var r.(1) |]
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
      arg_name = if name <> "" then name else sprintf "x%d" (n+1);
      arg_intent = intent_of_type typ;
      arg_size = size_of_type typ;
    }

let string_of_single_name n (_,_,name) = arg_of_name n name

let args_of_single_names = List.filter_mapi ~f:string_of_single_name

let fn_of_definition = function
  | DECDEF (_,_,[(name, PROTO (ret,args,false),[],NOTHING)])
  | FUNDEF ((_,_,(name, PROTO (ret,args,false),[], NOTHING)), _) ->
    let args = args_of_single_names args in
    let ret = match ret with
      | VOID -> None
      | _ -> Some {
          arg_name = "res";
          arg_intent = Some Out;
          arg_size = Word;
        }  in
    Some (name,{ret;args})
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

let term_of_arg arch n sub {arg_name; arg_size; arg_intent} =
  let word_size = Arch.addr_size arch in
  let size = match arg_size with
    | Word -> (word_size :> Size.t)
    | Size size -> size in
  let typ = Type.imm (Size.to_bits size) in
  let exp = try (abi arch).(n) with
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
      | Some {args;ret} ->
        let sub = List.foldi args ~init:sub ~f:(fun n sub arg ->
            Term.append arg_t sub (term_of_arg arch n sub arg)) in
        match ret with
        | None -> sub
        | Some ret ->
          Term.append arg_t sub (term_of_arg arch 0 sub ret))

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
