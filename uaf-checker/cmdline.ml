open Options

include Cmdliner

let fname : string option Term.t =
  let doc = "only one function" in
  Arg.(value & opt (some string) None & info ["fname"] ~doc)

let log_lookup : bool Term.t =
  let doc = "Log lookups" in
  Arg.(value & flag & info ["log-lookup"] ~doc)

let log_update : bool Term.t =
  let doc = "Log update" in
  Arg.(value & flag & info ["log-update"] ~doc)

let log_load : bool Term.t =
  let doc = "Log load" in
  Arg.(value & flag & info ["log-load"] ~doc)

let log_store : bool Term.t =
  let doc = "Log store" in
  Arg.(value & flag & info ["log-store"] ~doc)

let log_memory_load : bool Term.t =
  let doc = "Log memory-load" in
  Arg.(value & flag & info ["log-memory-load"] ~doc)

let log_memory_save : bool Term.t =
  let doc = "Log memory-save" in
  Arg.(value & flag & info ["log-memory-save"] ~doc)

let log_undefined_addr : bool Term.t =
  let doc = "Log undefined-addr" in
  Arg.(value & flag & info ["log-undefined-addr"] ~doc)

let log_undefined_var : bool Term.t =
  let doc = "Log undefined-var" in
  Arg.(value & flag & info ["log-undefined-var"] ~doc)

let log_def : bool Term.t =
  let doc = "Log def" in
  Arg.(value & flag & info ["log-def"] ~doc)

let log_jmp : bool Term.t =
  let doc = "Log jmp" in
  Arg.(value & flag & info ["log-jmp"] ~doc)

let log_dump_memory : bool Term.t =
  let doc = "Log dump-memory" in
  Arg.(value & flag & info ["log-dump-memory"] ~doc)

let log_dump_memory_on_load : bool Term.t =
  let doc = "Log dump-storage-on-load" in
  Arg.(value & flag & info ["log-dump-memory-on-load"] ~doc)

let log_dump_memory_on_store : bool Term.t =
  let doc = "Log dump-storage-on-store" in
  Arg.(value & flag & info ["log-dump-memory-on-store"] ~doc)

let log_dump_bindings : bool Term.t =
  let doc = "Log dump-bindings" in
  Arg.(value & flag & info ["log-dump-bindings"] ~doc)

let log_alloc : bool Term.t =
  let doc = "Log alloc" in
  Arg.(value & flag & info ["log-alloc"] ~doc)

let log_free : bool Term.t =
  let doc = "Log free" in
  Arg.(value & flag & info ["log-free"] ~doc)

let verbose : bool Term.t =
  let doc = "Verbose option for dumping UAF info" in
  Arg.(value & flag & info ["verbose"] ~doc)

let precision : int Term.t =
  let doc =
    "Use exact addresses (1) or address ranges based on malloc size\
     (2)" in Arg.(value & opt int 1 & info ["precision"] ~doc)

let process_args fname log_lookup log_update log_load log_store
    log_memory_load log_memory_save log_undefined_addr
    log_undefined_var log_def log_jmp log_dump_memory
    log_dump_memory_on_load log_dump_memory_on_store
    log_dump_bindings log_alloc log_free verbose precision =
  {fname;
   log_lookup;
   log_update;
   log_load;
   log_store;
   log_memory_load;
   log_memory_save;
   log_undefined_addr;
   log_undefined_var;
   log_def;
   log_jmp;
   log_dump_memory;
   log_dump_memory_on_load;
   log_dump_memory_on_store;
   log_dump_bindings;
   log_alloc;
   log_free;
   verbose;
   precision}

let info = Term.info ~doc:"" "UAF"

let parse argv =
  let args = Term.(pure process_args
                   $fname
                   $log_lookup
                   $log_update
                   $log_load
                   $log_store
                   $log_memory_load
                   $log_memory_save
                   $log_undefined_addr
                   $log_undefined_var
                   $log_def
                   $log_jmp
                   $log_dump_memory
                   $log_dump_memory_on_load
                   $log_dump_memory_on_store
                   $log_dump_bindings
                   $log_alloc
                   $log_free
                   $verbose
                   $precision) in
  match Term.eval ~argv (args,info) with
  | `Ok res -> res
  | `Error err -> exit 1
  | `Version | `Help -> exit 0
