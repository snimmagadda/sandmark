type source_line = { filename: string option; function_name: string option; line: int; offset: int }
type sample = { current : source_line; call_stack: source_line list }
type aggregate_result

val create_process_env_paused :
     string
  -> string array
  -> string array
  -> Unix.file_descr
  -> Unix.file_descr
  -> Unix.file_descr
  -> int * Unix.file_descr

val start_profiling : int -> Unix.file_descr -> aggregate_result

val write_profiling_result : string -> aggregate_result -> unit
