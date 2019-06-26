type source_line = { filename: string option; function_name: string option; line: int; offset: int }
type sample = { current : source_line; call_stack: source_line list }
type counts = { mutable self_time: int; mutable total_time: int }
type aggregate_result = (source_line, counts) Hashtbl.t

external unpause_and_start_profiling :
  int -> Unix.file_descr -> ( sample -> unit ) -> unit
  = "ml_unpause_and_start_profiling"

let get_or d o = 
  match o with
  | None -> d
  | Some(x) -> x

exception ExpectedSome

let unwrap = function
  | None -> raise ExpectedSome()
  | Some(x) -> x

let agg_hash = Hashtbl.create 1000

let update_line src_line self_time_inc total_time_inc = 
  match Hashtbl.find_opt agg_hash src_line with
  | None -> Hashtbl.add agg_hash src_line { self_time = 1; total_time = 1 }
  | Some(x) -> begin
                x.self_time <- (x.self_time+self_time_inc);
                x.total_time <- (x.total_time+total_time_inc);
              end
  
let rec update_lines = function
  | [] -> ()
  | (h :: t) -> 
    begin
      update_line h 0 1;
      update_lines t
    end

let sample_callback sample = 
  (* increment self for the current source line *)
  update_line sample.current 1 1;
  update_lines sample.call_stack


let start_profiling pid pipe_fd =
  unpause_and_start_profiling pid pipe_fd sample_callback;
  agg_hash

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

let slash_regex = Str.regexp "[/\.]"

let safe_file_name x =
  Str.global_replace slash_regex "_" x

let write_source_file name total_samples src_line_counts =
  let line_map = List.fold_left (fun m (k,v) -> IntMap.update k.line (function | None -> Some(v) | Some(x) -> Some({ self_time = v.self_time + x.self_time; total_time = v.total_time + x.total_time })) m) IntMap.empty src_line_counts in
  let src_file = match List.hd src_line_counts with
                 | ({ filename = Some(f) }, _) -> f in
  let new_src_file = safe_file_name src_file in
  let original_src = open_in src_file in
  let new_src = open_out (name ^ "_prof_results/" ^ new_src_file ^ ".html") in
  let current_line = ref 1 in
    try
  while true; do
    let src_line = input_line original_src in
    let counts = match IntMap.find_opt !current_line line_map with
                | Some(x) -> x
                | None -> { self_time = 0; total_time = 0 } in
    output_string new_src ((string_of_int counts.self_time) ^ " " ^ (string_of_int counts.total_time) ^ ": " ^ src_line ^ "\n");
    incr current_line
  done; ()
with End_of_file ->
  begin
    close_out new_src;
    close_in original_src;
  end

let add_to_line_list src_line counts l =
  match l with 
  | None -> Some([(src_line, counts)])
  | Some(v) -> Some((src_line, counts)::v)

let group_by_source_file src_line counts m =
  match src_line.filename with
  | None -> m
  | Some(f) -> StringMap.update f (function | None -> Some(IntMap.add src_line.line [(src_line, counts)] IntMap.empty) | Some(l) -> Some(IntMap.update src_line.line (add_to_line_list src_line counts) l)) m 

let rec take l n =
  match (l, n) with
  | ([], _) -> []
  | (_, 0) -> []
  | (h :: tl, _) -> h :: (take tl (n-1))

let source_line_counts_to_json src_line counts =
  `Assoc [("filename", `String (get_or "" src_line.filename));("function", `String (get_or "" src_line.function_name));("line", `Int src_line.line);("offset", `Int src_line.offset);("self_time", `Int counts.self_time);("total_time", `Int counts.total_time)]

let hotspots_to_json hotspots =
  (`List (List.map (fun (k,v) -> source_line_counts_to_json k v) hotspots))

let write_profiling_result output_name agg_result =
  (* first write out the json representation of results *)
  let total_samples = Hashtbl.fold (fun a b c -> c+(b.self_time)) agg_result 0 in
  let key_values = Hashtbl.fold (fun a b c -> (a, b)::c) agg_result [] in
  let only_present_filenames = List.filter (function | ({ filename = None }, _) -> false | ({ filename = Some(x) }, _) -> Sys.file_exists x) key_values in
  let grouped_by_filename = List.fold_left (fun m (k,v) -> StringMap.update (unwrap k.filename) (function | None -> Some([(k,v)]) | Some(l) -> Some((k,v)::l)) m) StringMap.empty only_present_filenames in
  let hotspots = take (List.sort (fun (k0,v0) (k1,v1) -> v1.self_time - v0.self_time) key_values) 10 in
  (*let results_by_file = Hashtbl.fold group_by_source_file agg_result StringMap.empty in*)
  let profile_out = open_out_bin (output_name ^ ".prof") in
  let hotspots_json =
    `Assoc [("hotspots", hotspots_to_json hotspots)]
  in
  Yojson.Basic.to_channel profile_out hotspots_json;
  close_out profile_out;
  let dir_name = output_name ^ "_prof_results" in
    if not (Sys.file_exists dir_name) then Unix.mkdir dir_name 0o740;
  (* Now we actually write out an HTML report *)
    List.iter (write_source_file output_name total_samples) (List.map (fun (k,v) -> v) (StringMap.bindings grouped_by_filename))