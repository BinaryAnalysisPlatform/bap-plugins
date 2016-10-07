open Core_kernel.Std

type t =
  | Pink | Aqua | Red | Green | Blue | Orange | Brown | Gray | Purple
  | White | Black

let to_int = function
  | Pink -> 0xff0099
  | Aqua -> 0x009999
  | Red -> 0xff0000
  | Green -> 0x009900
  | Blue -> 0x3366ff
  | Orange -> 0xFF6633
  | Brown -> 0x663300
  | Gray -> 0x666666
  | Purple -> 0x330066
  | White -> 0xffffff
  | Black -> 0x000000

let (!) = to_int
