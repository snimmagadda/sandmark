type sample = {comp_dir: string; filename: string; line: int}

type profiling_result = {samples: sample list}

val create_process_env_paused :
     string
  -> string array
  -> string array
  -> Unix.file_descr
  -> Unix.file_descr
  -> Unix.file_descr
  -> int * Unix.file_descr

val unpause_and_start_profiling : int -> Unix.file_descr -> profiling_result

val write_profiling_result : string -> profiling_result -> unit
