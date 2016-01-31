open Bap.Std

(** Paths are initially represented as a sequence of tids. If it
    reaches a checking stage, the check is passed the path in the form of
    a Sub.t. So, in the context of minos, an end-result path is a Sub.t.
    We use this for convenience *)
type path = Sub.t
