open Flag

type t = {fname : string option;
          dir : string option;
          directives : Flag.t list;
          verbose : bool;}
