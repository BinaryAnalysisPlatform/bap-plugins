open Core_kernel.Std
open Filename
open Format

(* any $(var) can be changed with TEST_VAR environment variable,
   for example, TEST_ARCH=x86, for $(arch).*)
let compile =
  "$(arch)-$(abi)-$(cc)-$(ver) $opt -g $(file).c -o $(dir)$(file)"

let bap = "bap $(dir)$(file) $(options)"

let test_folder = "/tmp/bap/tests/"

let subst = [
  "arch", "arm";
  "abi",  "linux-gnueabi";
  "cc", "gcc";
  "ver", "5";
  "opt", "";
  "dir", test_folder;
  "options", "";
]

type expect = Expect.t

let verbose () = try Sys.getenv "VERBOSE" with Not_found -> "0"

exception Command_failed of string [@@deriving sexp]

let assoc = List.Assoc.find_exn ~equal:String.equal


let result_of_string line =
  try Scanf.sscanf line "//! %s@\n" Option.some with exn -> None

let expected_results file : expect =
  In_channel.read_lines file |>
  List.filter_map ~f:result_of_string |>
  Expect.create

let expand pat map =
  let buf = Buffer.create 64 in
  Buffer.add_substitute buf (fun key ->
      try Sys.getenv ("TEST_"^String.uppercase key) with
        Not_found -> try assoc map key with
          Not_found -> failwithf "no subst for %s" key ())
    pat;
  Buffer.contents buf

let pipe cmd : string list =
  let env = Unix.environment () in
  let out,inp,err = Unix.open_process_full cmd env in
  Out_channel.close inp;
  let res = List.concat [
      In_channel.input_lines out;
      In_channel.input_lines err;
    ] in
  List.iter ~f:In_channel.close [out;err];
  res

let sh cmd =
  if Sys.command cmd <> 0 then
    raise (Command_failed cmd)

let build_file file =
  let file = chop_extension file in
  sh @@ expand compile @@ ["file", file] @ subst

let mtime file =
  let file = expand (sprintf "%s" file) subst in
  try Unix.((stat file).st_mtime) with exn -> 0.0

let needs_rebuild f =
  let s = expand (sprintf "$(dir)%s" @@ chop_extension f) subst in
  mtime f > mtime s

let pipe_bap file =
  sh @@ expand "mkdir -p $(dir)$(file)" @@
  ["file", dirname file] @ subst;
  if needs_rebuild file then build_file file;
  pipe @@ expand bap @@ [
    "file", chop_extension file;
  ] @ subst

let print_result misses got =
  eprintf "@.@.%a in the following output:@.@."
    Expect.pp_misses misses;
  List.iter ~f:print_endline got

let set_of_list xs =
  List.fold xs ~init:String.Set.empty ~f:(fun set s ->
      Set.add set (String.strip s))

let ok file =
  let exp = expected_results file in
  let got = pipe_bap file  in
  match Expect.all_matches exp got with
  | `Yes -> true
  | `Missed misses ->
    if verbose () <> "0" then print_result misses got;
    false


let check file =
  if file <> Sys.argv.(0) then
    let r = ok file in
    if r
    then printf "%-60s%s\n%!" file "ok"
    else printf "%-60s%s\n%!" file "fail";
    not r
  else false

let run files =
  List.count files ~f:check |> function
  | 0 -> printf "all ok\n"; exit 0
  | 1 -> printf "one failure\n"; exit 1
  | n -> printf "%d failures\n" n; exit 1

let main () =
  match Array.to_list Sys.argv with
  | _ :: files -> run files
  | _ ->
    eprintf "Usage: TEST_OPTIONS='..' bapbuild test.native -- <file1> <file2> .. ";
    exit 3

let () = main ()
