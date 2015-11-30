open Core_kernel.Std
open Filename

(* any $(var) can be changed with TEST_VAR environment variable,
   for example, TEST_ARCH=x86, for $(arch).*)
let compile =
  "$(arch)-$(abi)-$(cc)-$(ver) $opt -g $(file).c -o $(dir)$(file)"

let bap =
  "bap $(options) $(dir)$(file) $(ida) $(symfile) $(plugin)"

let test_folder = "/tmp/bap/tests/"

let subst = [
  "arch", "arm";
  "abi",  "linux-gnueabi";
  "cc", "gcc";
  "ver", "4.7";
  "opt", "";
  "dir", test_folder;
  "options", "";
  "ida", "";
  "symfile", "";
  "plugin", "";

]

let verbose = try Sys.getenv "VERBOSE" with Not_found -> "0"

exception Command_failed of string with sexp


let result_of_string line =
  try Scanf.sscanf line "//! %s@\n" Option.some with exn -> None

let expected_results file =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.fold_lines inc ~init:String.Set.empty ~f:(fun rs str ->
          match result_of_string str with
          | None -> rs
          | Some res -> Set.add rs (String.strip res)))

let expand pat map =
  let buf = Buffer.create 64 in
  Buffer.add_substitute buf (fun key ->
      try Sys.getenv ("TEST_"^String.uppercase key) with
        Not_found -> try List.Assoc.find_exn map key with
          Not_found -> failwithf "no subst for %s" key ())
    pat;
  Buffer.contents buf

let pipe cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let sh cmd =
  if Sys.command cmd <> 0 then
    raise (Command_failed cmd)

let dump_syms f =
  expand (sprintf "--use-ida --dump-symbols=$(dir)%s.syms \
                   -dbir > $(dir)%s.bir"  f f) subst

let with_syms f =
  expand (sprintf "--syms=$(dir)%s.syms" @@ chop_extension f) subst

let build_file file =
  let file = chop_extension file in
  sh @@ expand compile @@ ["file", file] @ subst;
  sh @@ expand bap @@ ["file", file; "options", dump_syms file] @
                      subst

let mtime file =
  let file = expand (sprintf "%s" file) subst in
  try Unix.((stat file).st_mtime) with exn -> Float.nan

let needs_rebuild f =
  let s =
    expand (sprintf "$(dir)%s.syms" @@ chop_extension f) subst in
  mtime f > mtime s

let pipe_bap plugin file =
  sh @@ expand "mkdir -p $(dir)$(file)" @@
  ["file", dirname file] @ subst;
  if needs_rebuild file then build_file file;
  pipe @@ expand bap @@ [
    "file", chop_extension file;
    "symfile", with_syms file;
    "options", sprintf "-l%s" plugin] @
    subst

let print_result ~exp ~got =
  printf "Expected:\n";
  Set.iter ~f:print_endline exp;
  printf "Got:\n";
  Set.iter ~f:print_endline got

let ok plugin file =
  let exp = expected_results file in
  let got = String.Set.of_list (pipe_bap plugin file) in
  if verbose <> "0" then print_result ~exp ~got;
  Set.equal exp got

let check plugin file =
  if file <> Sys.argv.(0) then
    let r = ok plugin file in
    if r
    then printf "%-40s%s\n%!" file "ok"
    else printf "%-40s%s\n%!" file "fail";
    not r
  else false

let run plugin files =
  List.count files ~f:(check plugin) |> function
  | 0 -> printf "all ok\n"; exit 0
  | 1 -> printf "one failure\n"; exit 1
  | n -> printf "%d failures\n" n; exit 1

let main () =
  match Array.to_list Sys.argv with
  | _ :: plugin :: files -> run plugin files
  | _ ->
    eprintf "Usage: bapbuild test.native -- <plugin> <file1> <file2> .. ";
    exit 3

let () = main ()
