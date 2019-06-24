type source_line = { filename: string option; function_name: string option; line: int; addr: int }
type sample = { current : source_line; call_stack: source_line list }
type profiling_result = { samples: sample list }

val create_process_env_paused :
     string
  -> string array
  -> string array
  -> Unix.file_descr
  -> Unix.file_descr
  -> Unix.file_descr
  -> int * Unix.file_descr

val start_profiling : int -> Unix.file_descr -> profiling_result

val write_profiling_result : string -> profiling_result -> unit
