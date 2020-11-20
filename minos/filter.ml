open Core_kernel
open Bap.Std

type t = string -> bool

let whitelist =
  [ "@\\..*" ; (* plt for x86 *)
    "@memcpy"; (* plt for ARM *)
    "@recvfrom";
    "@recv";
    "@system";
    "@sprintf";
    "@strcpy";
    "@strcat"]

let compile s =
  s |> List.map ~f:Re.Posix.re
  |> Re.alt
  |> Re.compile
  |> Re.execp

let apply f str = f str

let plt_filter ~extra : t =
  extra @ whitelist |> compile

let cpp_filter ~extra : t =
  [ "@_ZNS.*"] @ extra @ whitelist |> compile
