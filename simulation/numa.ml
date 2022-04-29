#use "useful.ml";;
#load "str.cma";;


type machine = int array array;;
type mapping = int array;;
type thread_page_mapping = {thread_mapping: mapping;
                            page_mapping: mapping};;

type access_info = {thread: int; page: int};;
type access_type = Store | Load;;
type access = access_type * access_info;;

type remapping_info = {agent: int; destination: int};;
type remapping_type = Thread_remap | Page_remap;;
type remapping = remapping_type * remapping_info;;

type simulation_step = Access of access | Remapping of remapping;;
type trace = simulation_step list;;
exception Conversion_error of string;;


let trace_file = "test.txt";;

let rec read_trace_file file =
  read_trace_chan (open_in file)
  
and read_trace_chan chan =
  try
    let line = input_line chan in
    access_of_str line::read_trace_chan chan
    
  with e ->
        match e with
        | Conversion_error s -> raise e
        | _ -> []

and access_of_str str =
  let regex = Str.regexp "^\\([0-9]+\\) \\([SL]\\): \\(0x[0-9A-Fa-f]+\\)$" in
  if Str.string_match regex str 0
  then
    let thread = int_of_string (Str.matched_group 1 str) in
    let access_type = access_type_of_string (Str.matched_group 2 str) in
    let address = int_of_string (Str.matched_group 3 str) in
    (access_type, {thread=thread;page=address})
  else
    raise (Conversion_error ("access_of_str: " ^ str))

and access_type_of_string = function
  | "S" -> Store
  | "L" -> Load
  | s -> raise (Conversion_error ("action_of_string: " ^ s));;

let rec string_of_access (access_type, access_info) =
  (string_of_access_type access_type) ^ " " ^ (string_of_access_info access_info)

and string_of_access_type = function
    | Store -> "Store"
    | Load -> "Load"

and string_of_access_info {thread; page} =
  Printf.sprintf "Thread = %d Addr = %d\n" thread page;;
  

let () = print_list string_of_access (read_trace_file trace_file);;

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
