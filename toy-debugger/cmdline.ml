open Core_kernel.Std
open Format

open Options
open Flag

include Cmdliner

module T = struct
  open Sexp.O
  open Result.Monad_infix

  let expect exp ~got = Error (sprintf "expected %s got %S" exp got)

  (** Takes `eval_blk` and capitalizes it, so we can just use of_sexp *)
  let atom (s : Sexp.t) =
    match s with
    | Atom s ->
      String.capitalize s
      |> Sexp.of_string
      |> Hook.t_of_sexp
      |> Ok
    | s -> expect (grammar ()) ~got:(Sexp.to_string s)

  (** Parses list of sexps into hook types *)
  let sexp = function
    | List x -> List.map ~f:atom x
    | x -> [atom x]

  let start s = Result.all (sexp s)

  let parse s =
    try (start (Sexp.of_string s))
    with exn -> expect (grammar ()) ~got:s

  let parser s = match parse s with
    | Ok r -> `Ok r
    | Error e -> `Error e

  let to_string hook =
    Sexp.to_string (Hook.sexp_of_t hook)

  let pp ppf s =
    List.map ~f:to_string s |>
    String.concat ~sep:" " |>
    Format.fprintf ppf "%s"

  let go : 'a Cmdliner.Arg.converter = parser,pp
end

let fname : string option Term.t =
  let doc = "only one function" in
  Arg.(value & opt (some string) None & info ["fname"] ~doc)

let flags kind : hook list list Term.t =
  let doc = sprintf "Flags" in
  Arg.(value & opt_all T.go [] & info [kind] ~doc)

let dir : string option Term.t =
  let doc = "output directory" in
  Arg.(value & opt (some string) None & info ["dir"] ~doc)

let verbose : bool Term.t =
  let doc = "Verbose option" in
  Arg.(value & flag & info ["verbose"] ~doc)

let process_args fname dir memory regs dot path_counts trace self
    checkpoints verbose =
  let open Variantslib in
  let directives =
    let unbox var = var.Variantslib.Variant.constructor in
    let add hooks acc v =
      let hooks' = List.concat hooks in
      let v' = unbox v in
      (Flag (v',hooks')) :: acc in
    Flag.Directive.Variants.fold ~init:[]
      ~memory:(add memory)
      ~regs:(add regs)
      ~path_counts:(add path_counts)
      ~dot:(add dot)
      ~trace:(add trace)
      ~myself:(add self)
      ~checkpoints:(add self)
  in
  {fname;
   dir;
   directives;
   verbose}

let info = Term.info ~doc:"" "Debugger"

let parse argv =
  let args = Term.(pure process_args
                   $fname
                   $dir
                   $(flags "memory")
                   $(flags "regs")
                   $(flags "dot")
                   $(flags "path-counts")
                   $(flags "trace")
                   $(flags "myself")
                   $(flags "checkpoints")
                   $verbose) in
  match Term.eval ~argv (args,info) with
  | `Ok res -> res
  | `Error err -> exit 1
  | `Version | `Help -> exit 0
