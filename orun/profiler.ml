type sample = {filename: string; line: int}
type profiling_result = {samples: sample list}

external unpause_and_start_profiling :
  int -> Unix.file_descr -> ( sample -> unit ) -> unit
  = "ml_unpause_and_start_profiling"

let samples_list = ref []

let sample_callback sample = 
  samples_list := sample :: !samples_list

let start_profiling pid pipe_fd =
  unpause_and_start_profiling pid pipe_fd sample_callback;
  let result = {samples = !samples_list} in
    samples_list := [];
    result

let int_of_fd (x : Unix.file_descr) : int = Obj.magic x

let rec file_descr_not_standard (fd : Unix.file_descr) =
  if int_of_fd fd >= 3 then fd else file_descr_not_standard (Unix.dup fd)

let safe_close fd = try Unix.close fd with Unix.Unix_error (_, _, _) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let new_stdin = file_descr_not_standard new_stdin in
  let new_stdout = file_descr_not_standard new_stdout in
  let new_stderr = file_descr_not_standard new_stderr in
  (*  The three dup2 close the original stdin, stdout, stderr,
      which are the descriptors possibly left open
      by file_descr_not_standard *)
  Unix.dup2 ~cloexec:false new_stdin Unix.stdin ;
  Unix.dup2 ~cloexec:false new_stdout Unix.stdout ;
  Unix.dup2 ~cloexec:false new_stderr Unix.stderr ;
  safe_close new_stdin ;
  safe_close new_stdout ;
  safe_close new_stderr

let rec wait_for_parent parent_ready =
  let read_fds, _write_fds, _exception_fds =
    Unix.select [parent_ready] [] [] (-1.0)
  in
  if List.mem parent_ready read_fds then () else wait_for_parent parent_ready

let create_process_env_paused cmd args env new_stdin new_stdout new_stderr =
  let parent_ready, parent_ready_write = Unix.pipe () in
  match Unix.fork () with
  | 0 -> (
    try
      perform_redirections new_stdin new_stdout new_stderr ;
      wait_for_parent parent_ready ;
      Unix.execvpe cmd args env
    with _ -> exit 127 )
  | id ->
      (id, parent_ready_write)

module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let update_line_map m l =
  IntMap.update l (fun e -> match e with | Some(v) -> (Some (v + 1)) | None -> (Some 1)) m

let update_source_map m s =
  StringMap.update s.filename (fun e -> let v = match e with | Some(x) -> x | None -> IntMap.empty in Some(update_line_map v s.line)) m

let slash_regex = Str.regexp "[/\.]"

let safe_file_name x =
  Str.global_replace slash_regex "_" x

let write_source_file name res src_file =
  let line_map = StringMap.find src_file res in
  let new_src_file = safe_file_name src_file in
  let original_src = open_in src_file in
  let new_src = open_out (name ^ "_prof_results/" ^ new_src_file ^ ".html") in
  let current_line = ref 1 in
    try
  while true; do
    let src_line = input_line original_src in
    let count = match IntMap.find_opt !current_line line_map with
                | Some(x) -> x
                | None -> 0 in
    output_string new_src ((string_of_int count) ^ ": " ^ src_line ^ "\n");
    incr current_line
  done; ()
with End_of_file ->
  begin
    close_out new_src;
    close_in original_src;
  end


let write_profiling_result output_name result =
  (* first write out the json representation of results *)
  let total_samples = List.length result.samples in
  let aggregate_results = List.fold_left update_source_map StringMap.empty result.samples in
  let profile_out = open_out_bin (output_name ^ ".prof") in
  let json_results =
    (`Assoc (List.map (fun (k,v) -> (k, `List (List.map (fun (a,b) -> `List [`Int a; `Int b]) (IntMap.bindings v)))) (StringMap.bindings aggregate_results)))
  in
  Yojson.Basic.to_channel profile_out json_results;
  close_out profile_out;
  let dir_name = output_name ^ "_prof_results" in
    if not (Sys.file_exists dir_name) then Unix.mkdir dir_name 0o740;
  (* Now we actually write out an HTML report *)
  let source_files_that_exist = List.filter (fun (k,v) -> Sys.file_exists k) (StringMap.bindings aggregate_results) in
    List.iter (write_source_file output_name aggregate_results) (List.map (fun (k,v) -> k) source_files_that_exist)