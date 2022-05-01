#use "trace.ml";;

type machine = int array array;;
type mapping = int array;;
type thread_page_mapping = {thread_mapping: mapping;
                            page_mapping: mapping};;

type remapping_info = {agent: int; destination: int};;
type remapping_type = Thread_remap | Page_remap;;
type remapping = remapping_type * remapping_info;;


type simulation_step = Access of access | Remapping of remapping;;
type simulation = simulation_step list;;

let print_mapping mapping =
  let () = Array.iteri (Printf.printf "(%d, %d), ") mapping in
  print_string "\n";;

let rec create_mapping mapping_func num_agents num_processors =
  let mapping = Array.make num_agents (-1) in
  let () = fill_mapping mapping_func num_processors mapping 0 in
  mapping
  
and fill_mapping mapping_func num_processors mapping from =
  match Int.equal (Array.length mapping) from with
  | true -> ()
  | false -> let () = Array.set mapping from (mapping_func from num_processors) in
             fill_mapping mapping_func num_processors mapping (from + 1);;

let create_random_mapping =
  create_mapping
    (fun agent num_processors ->
      let () = Random.init agent in
      Random.int num_processors);;

let create_mod_mapping =
  create_mapping
    (fun agent num_processors ->
      agent mod num_processors);;

let print_machine machine =
  let num_proc = Array.length machine in
  Array.iter
    (Array.iteri
       (fun idx dist ->
         let () = Printf.printf "%d, " dist in
         if idx == (num_proc - 1)
          then print_string "\n"
         else ()))
    machine;;

let create_machine distance_func num_processors =
  Array.init
    num_processors
    (fun proc1 -> Array.init num_processors (distance_func proc1));;
    

let distance machine processor1 processor2 =
  Array.get (Array.get machine processor1) processor2;;

let processor mapping agent =
  try
    Array.get mapping agent
  with Invalid_argument s ->
        let () = Printf.printf
                   "Attempting to access element %d in mapping of length %d\n"
                   agent
                   (Array.length mapping) in
        raise (Invalid_argument s);;

let remap mapping {agent; destination} =
  let () = Array.set mapping agent destination in
  mapping;;

let access_cost machine {thread_mapping; page_mapping} (_, {thread; page}) =
  let p1 = processor thread_mapping thread in
  let p2 = processor page_mapping page in
  distance machine p1 p2;;


let rec trace_cost machine {thread_mapping; page_mapping} = function
  | [] -> 0
  | Remapping (Thread_remap, r)::rest ->
     trace_cost
       machine
       {thread_mapping = (remap thread_mapping r);
        page_mapping = page_mapping;}
       rest
  | Remapping (Page_remap, r)::rest ->
     trace_cost
       machine
       {thread_mapping = thread_mapping;
        page_mapping = (remap page_mapping r);}
       rest
  | Access (a)::rest ->
     (access_cost
       machine
       {thread_mapping; page_mapping}
       a)
     +
       (trace_cost
         machine
         {thread_mapping; page_mapping}
         rest);;

