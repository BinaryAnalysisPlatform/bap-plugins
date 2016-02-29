open Bap.Std

let main proj =
  Project.with_program proj @@
  Term.map sub_t (Project.program proj) ~f:Sub.ssa

let () = Project.register_pass main
