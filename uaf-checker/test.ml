#use "topfind";;
#require "bap.top";;
#require "oUnit";;

open Core_kernel.Std
open Bap.Std
open Or_error
open OUnit2

let test ?(extra=[]) test_path expected ctxt : unit =
  let options =
    ["--no-byteweight"; "--symbolizer=ida"; "-lmain"; "--main"] @ extra in
  assert_command ~ctxt "bap" ([test_path] @ options) ~foutput:(fun stream ->
      let str =
        let res = ref [] in
        let appender v = res := v :: !res in
        Stream.iter appender stream;
        List.rev !res |> List.map ~f:String.of_char |> String.concat in
      assert_equal str expected)

(** Read test oracle *)
let expected filename = In_channel.read_all filename

let suite =
  "uaf" >:::
  [
    "dead-simpl-1" >:: test "tests/all/dead-simpl-uaf-arm"
      (expected "expected/dead-simpl-uaf-arm.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "dead-simple-2" >:: test "tests/all/dead-simpl-uaf-arm-2"
      (expected "expected/dead-simpl-uaf-arm-2.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "super-simpl" >:: test "tests/all/super-simpl-uaf-arm"
      (expected "expected/super-simpl-uaf-arm.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "simpl" >:: test "tests/all/simpl-uaf-arm"
      (expected "expected/simpl-uaf-arm.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "simpl-2" >:: test "tests/all/simpl-uaf-arm-2"
      (expected "expected/simpl-uaf-arm-2.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "no-assign" >:: test "tests/all/uaf-no-assign-arm"
      (expected "expected/uaf-no-assign-arm.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "gueb-example" >:: test "tests/all/gueb-example-uaf-arm-O0"
      (expected "expected/gueb-example-uaf-arm-O0.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "gueb-example-ite" >:: test "tests/all/gueb-example-ite-uaf-arm-O0"
      (expected "expected/gueb-example-ite-uaf-arm-O0.output")
      ~extra:["--main-fname=main";
              "--main-precision=2";];
    "gnome-nettool" >:: test "tests/all/gnome-nettool"
      (expected "expected/gnome-nettool.output")
      ~extra:["--main-precision=2";
              "--main-fname=info_get_nic_information";
              "--callsites"]
  ]

let () = run_test_tt_main suite
