type machine = int array array;;
type mapping = int array;;
type thread_page_mapping = {thread_mapping: mapping;
                            page_mapping: mapping};;

type access_info = {thread: int; page: int};;
type access_type = Store | Load;;
type access = access_type * access_info;;

type remapping_info = {agent: agent; destination: processor};;
type remapping_type = Thread_remap | Page_remap;;
type remapping = remapping_type * remapping_info;;

type simulation_step = Access of access | Remapping of remapping;;
type trace = simulation_step list;;


let trace_file = "test.txt";;
let read_trace file =
  let ic = open_in file in
  let line = input_line ic in
  let regex = Str.regexp "^([0-9]+) ([SL]): 0x([0-9]+)$" in
  if Str.string_match regex line 0
  then
    let thread = Str.matched_group 0 in
    let action = Str.matched_group 1 in
    let address = Str.matched_group 2 in
    Printf.printf
      "thread: %s action: %s address: %s\n"
      thread
      action
      address
  else
    Printf.printf "%s is not valid\n" line;;

let () = read_trace trace_file;;

(*
let distance machine processor1 processor2 =
  Array.get (Array.get machine processor1) processor2;;

let processor mapping agent =
  try
    Array.get mapping agent
  with Invalid_argument ->
        let () = Printf.printf
                   "Attempting to access element %d in mapping of length %d\n"
                   agent
                   (Array.length mapping) in
        raise Invalid_argument;;

let remap mapping {agent; destination} =
  let () = Array.set mapping agent destination in
  mapping;;

let cost machine {thread_mapping; page_mapping} (_, {thread; page}) =
  let p1 = processor thread_mapping thread in
  let p2 = processor page_mapping page in
  distance machine p1 p2;;


let rec simulate machine {thread_mapping; page_mapping} = function
  | [] -> 0
  | Remapping (Thread_remap, r)::rest ->
     simulate
       machine
       {thread: (remap thread_mapping r);
        page: page}
       rest
  | Remapping (Page_remap, r)::rest ->
     simulate
       machine
       {thread: thread;
        page: (remap page_mapping r}
       rest
  | Access (a)::rest -> (cost machine {thread_mapping; page_mapping} a) +
                          (simulate
                             machine
                             {thread_mapping; page_mapping}
                             rest);;


 *)
