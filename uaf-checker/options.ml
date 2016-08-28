type t = {fname : string option;
          log_lookup : bool;
          log_update : bool;
          log_load : bool;
          log_store : bool;
          log_memory_save : bool;
          log_memory_load : bool;
          log_undefined_addr : bool;
          log_undefined_var : bool;
          log_def : bool;
          log_jmp : bool;
          log_dump_memory : bool;
          log_dump_memory_on_load : bool;
          log_dump_memory_on_store : bool;
          log_dump_bindings : bool;
          log_alloc : bool;
          log_free : bool;
          verbose : bool;
          (** This is temporary *)
          precision : int}
