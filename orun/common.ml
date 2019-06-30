type source_line =
  { filename: string option
  ; function_name: string option
  ; line: int
  ; offset: int }
type sample = {current: source_line; call_stack: source_line list}
type counts = {mutable self_time: int; mutable total_time: int}
type aggregate_result = (source_line, counts) Hashtbl.t

let rec take l n =
  match (l, n) with
  | [], _ ->
      []
  | _, 0 ->
      []
  | h :: tl, _ ->
      h :: take tl (n - 1)