open Core_kernel.Std
open Bap.Std

let label_matches l id = match l with
  | Indirect _ -> false
  | Direct tid -> Tid.name tid = "@"^id

let callee call prog = match Call.target call with
  | Indirect _ -> None
  | Direct tid -> Term.find sub_t prog tid

let call_of_jmp jmp = match Jmp.kind jmp with
  | Ret _ | Int _ | Goto _ -> None
  | Call call -> Some call

let call_matches call id =
  label_matches (Call.target call) id


let return caller sub =
  match Call.return caller with
  | None | Some (Indirect _) -> None
  | Some (Direct tid) -> Term.find blk_t sub tid

let intent_matches x y = match Arg.intent x with
  | None -> true
  | Some x -> match x,y with
    | In,In | Out,Out -> true
    | Both,_| _,Both -> true
    | _ -> false

let require x = Option.some_if x ()
