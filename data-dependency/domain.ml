open Core_kernel.Std

type t = {
  (* Boolean value that determines whether a set is infinite or not *)
  infinite : bool;
  (* Boolean value corresponding to zero or one*)
  value : bool;
  (* List of indexes that have the corresponding bit set *)
  indexes : Int.Set.t;
} with sexp

exception Not_invertible
exception Not_representable

let pp fmt t =
  Format.fprintf fmt "{Infinite: %B; Value: %B; Set: {" t.infinite t.value;
  Int.Set.iter t.indexes ~f:(fun t -> Format.fprintf fmt "%d, " t);
  Format.fprintf fmt "}}"

let create_infinite value = {indexes = Int.Set.empty; value; infinite = true}

let create_finite indexes value = match indexes with
  | [] -> raise Not_representable
  | _ -> {indexes = Int.Set.of_list indexes; value; infinite = false}

let empty = create_infinite false

let univ = create_infinite true

let invert ?(width = 0) v =
  match v.infinite with
  | false -> let indexes =
               let expanded = (List.range 0 @@ max width
                               @@ Int.Set.max_elt_exn v.indexes) in
               List.fold expanded ~init:Int.Set.empty ~f:(fun accum value ->
                   if Int.Set.mem v.indexes value then accum
                   else Int.Set.add accum value)
    in { indexes; value = not v.value; infinite = false }
  | true -> raise Not_invertible

(* Return the domain value with min/max set length *)
let min_set_pair a b =
  if Int.Set.(length a.indexes < length b.indexes) then a else b
let max_set_pair a b =
  if Int.Set.(length a.indexes < length b.indexes) then b else a

(* Applies a operator after performing some checks *)
let rec apply a b op ~f =
  match (a.value = b.value, a.infinite = b.infinite) with
  | (false, true) -> (match a.infinite with
      (* Both finite with different values, so invert the representation of the
       * set with more elements to the maximum width of both *)
      | false -> let size =
                   let a_indexes = (Int.Set.max_elt_exn a.indexes) in
                   let b_indexes = (Int.Set.max_elt_exn b.indexes) in
                   max a_indexes b_indexes in
        if Int.Set.(length a.indexes < length b.indexes)
        then
          apply a (invert ~width:size b) op ~f
        else apply (invert ~width:size a) b op ~f
      (* Both infinite with different values, so either empty or universal *)
      | true -> match op with
        | `Inter -> empty
        | `Union -> univ
        | `Diff -> if b.value then empty else univ)
  | (false, false) -> (match op with
      (* Return the infinite domain value with smaller length *)
      | `Inter -> min_set_pair a b
      (* Return the finite domain value with larger length *)
      | `Union -> max_set_pair a b
      (* empty - a = empty, a - empty = a,
       * universal - a = not a, a - universal = empty *)
      | `Diff -> match (a.infinite, b.infinite) with
        | (true, false) ->
          if not a.value then empty else
            {indexes = a.indexes; value = not a.value; infinite = a.infinite}
        | (false, true) -> if not b.value then a else empty
        | _ -> raise Not_representable)
  (* Apply the operator directly *)
  | (true, true) ->
    {indexes = f a.indexes b.indexes; value = a.value; infinite = a.infinite}
  | (true, false) -> (match op with
      (* The finite domain value has the larger length *)
      | `Inter -> max_set_pair a b
      (* The infinite domain value has the smaller length *)
      | `Union -> min_set_pair a b
      (* empty - a = empty, a - empty = a,
       * universal - a = not a, a - universal = empty *)
      | `Diff -> match (a.infinite, b.infinite) with
        | (true, false) ->
          if not a.value then empty else
            {indexes = a.indexes; value = not a.value; infinite = a.infinite}
        | (false, true) ->
          if not b.value then a else empty
        | _ -> raise Not_representable)

let inter a b = apply a b `Inter ~f:Int.Set.inter

let union a b = apply a b `Union ~f:Int.Set.union

let diff a b = apply a b `Diff ~f:Int.Set.diff

let is_finite v = not v.infinite

let get_indices v = v.indexes

let get_value v = v.value

let fold v1 ~init ~f = Int.Set.fold ~init ~f:(fun accum v2 ->
    f accum {indexes = Int.Set.singleton v2;
             value = v1.value; infinite = v1.infinite }) v1.indexes

include Comparable.Make(struct
    type nonrec t = t with sexp
    let rec compare a b = match (a.value = b.value, a.infinite = b.infinite) with
      | (true, true) -> Int.Set.compare a.indexes b.indexes
      | (false, true) -> let size =
                           let a_indexes = (Int.Set.max_elt_exn a.indexes) in
                           let b_indexes = (Int.Set.max_elt_exn b.indexes) in
                           max a_indexes b_indexes in
        if Int.Set.(length a.indexes < length b.indexes) then
          compare a (invert ~width:size b) else compare (invert ~width:size a) b
      | (true, false) -> if b.infinite then -1 else 1
      | (false, false) -> match (a.infinite, b.infinite) with
        | (true, false) -> if not a.value then -1 else 1
        | (false, true) -> if not b.value then 1 else -1
        | _ -> raise Not_representable
  end)
