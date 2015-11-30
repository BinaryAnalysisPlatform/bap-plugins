open Core_kernel.Std

type t =
  | Pink | Aqua | Red | Green | Blue | Orange | Brown | Gray | Purple | White

let monochrome_gradient =
  [0x212121; 0x474747; 0x6b6b6b; 0x8c8c8c;
   0xaaaaaa; 0xc4c4c4; 0xd3d3d3; 0xe8e8e8]

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

let to_terminal = function
  | Pink -> "\x1b[95m"
  | Aqua -> "\x1b[46m"
  | Red -> "\x1b[41m"
  | Green -> "\x1b[42m"
  | Blue -> "\x1b[44m"
  | Orange -> "\x1b[43m"
  | Brown -> "\x1b[40m" (* normal *)
  | Gray -> "\x1b[40m" (* normal *)
  | Purple -> "\x1b[35m"
  | White -> "\x1b[37m"

let make_color_map l c =
  List.fold l ~init:[] ~f:(fun acc name ->
      List.Assoc.add acc name c)

let (!) = to_int
let (!!) = to_terminal
