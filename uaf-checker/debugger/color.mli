open Core_kernel.Std

type t =
  | Pink | Aqua | Red | Green | Blue | Orange | Brown | Gray | Purple
  | White | Black

val to_int : t -> int

val ( ! ) : t -> int
