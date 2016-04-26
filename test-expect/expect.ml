open Core_kernel.Std
open Graph

type trial = {
  regexp : Re.re;
  string : string;
}
type expect = trial array

type misses = expect
type t = expect


let create =
  Array.of_list_map ~f:(fun s -> {
        regexp = Re.compile (Re_posix.re s);
        string = s;
      })

let sat e s = Re.execp e.regexp s


module G = struct
  type t = expect * string array
  module V = struct
    type t = Source | Sink | Person of int | Task of int [@@deriving compare]
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
  end
  module E = struct
    type label = unit

    type t = {src : V.t; dst : V.t} [@@deriving fields]
    let make src dst = {src; dst}
    let label _ = ()
  end
  type dir = Succ | Pred

  let iter dir f (workers,jobs) v =
    match v,dir with
    | V.Source,Pred -> ()
    | V.Source,Succ ->
      Array.iteri workers ~f:(fun i _  ->
          f @@ E.make V.Source (V.Person i))
    | V.Sink,Pred ->
      Array.iteri jobs ~f:(fun i _ ->
          f @@ E.make (V.Task i) V.Sink)
    | V.Sink,Succ -> ()
    | V.Person i as p,Pred -> f @@ E.make V.Source p
    | V.Person i as p,Succ ->
      Array.iteri jobs ~f:(fun j job ->
          if sat workers.(i) job then f (E.make p (V.Task j)))
    | V.Task j as t,Succ -> f @@ E.make t V.Sink
    | V.Task j as t,Pred ->
      Array.iteri workers ~f:(fun i worker ->
          if sat worker jobs.(j) then f (E.make (V.Person i) t))

  let iter_succ_e = iter Succ
  let iter_pred_e = iter Pred
end

module F = struct
  type t = int
  type label = unit
  let max_capacity () = 1
  let min_capacity () = 0
  let flow () = min_capacity ()
  let add = (+)
  let sub = (-)
  let zero = 0
  let compare = Int.compare
end

module FFMF = Flow.Ford_Fulkerson(G)(F)

let all_matches expect workers =
  let workers = Array.of_list workers in
  let (flow,_) = FFMF.maxflow (expect,workers) G.V.Source G.V.Sink in
  Array.filteri expect ~f:(fun i _ ->
      Sequence.range 0 (Array.length workers) |>
      Sequence.for_all ~f:(fun j ->
          flow (G.E.make (G.V.Person i) (G.V.Task j)) = 0)) |> function
  | [| |] -> `Yes
  | ms -> `Missed ms


let pp_one ppf ms =
  Format.fprintf ppf "@[Expectation %S is not satisfied@]" ms.(0).string

let pp_many ppf ms =
  Format.fprintf ppf "@[<v2>Expectations [";
  Array.iter ms ~f:(fun {string=s} -> Format.fprintf ppf "@;%S;" s);
  Format.fprintf ppf "@]@.] are not satisfied"

let pp_misses ppf ms =
  if Array.length ms = 1
  then pp_one ppf ms
  else pp_many ppf ms
