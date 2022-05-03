#use "machine.ml";;
#use "trace.ml";;
#use "mapper.ml";;
#use "useful.ml";;
#load "str.cma";;

let trace_file = "../dijkstra/trace20_64_10";;
let num_threads = 64;;
let num_pages = 30648;;
let () = Printf.printf
           "Assuming %d threads and %d pages\n" num_threads num_pages;;

(*let test_read chan n =
  let (accesses, lines_read) = read_trace_chan chan n in
    let () = Printf.printf "%d lines read\n" lines_read in
    print_list string_of_access accesses;;

let () = Array.iter (test_read (open_in trace_file)) [|0; 1; 2|];;*)

(*let () =
  let mapping1 = create_random_mapping 10 5 in
  let mapping2 = create_random_mapping 10 5 in
  let mapping3 = create_mod_mapping 10 5 in
  let () = print_mapping mapping1 in
  let () = print_mapping mapping2 in
  print_mapping mapping3;;*)
(*
let () =
  let machine1 = create_machine (fun p1 p2 -> if p1 == p2 then 0 else 1) 5 in
  let machine2 = create_machine (fun p1 p2 -> p1 - p2) 5 in
  let () = print_machine machine1 in
  print_machine machine2;;
 *)

let rec simulation_cost machine mapper trace_file accesses_per_remap num_threads num_pages dp =
  let (initial, mapper_f) =
    try
      mapper machine num_threads num_pages (num_processors machine)
    with e ->
      let () = Printf.printf "21%!" in
      raise e in
  let chan = open_in trace_file in
  try
    simulation_cost_helper
      machine
      mapper_f
      initial
      (fun () ->
        read_trace_chan chan accesses_per_remap)
      (dp, 0)
  with e ->
    let () = Printf.printf "22%!" in
    raise e
    
and simulation_cost_helper machine mapper mapping trace_fetch (dp, accum_cost) =
  match trace_fetch () with
  | [] -> (dp, accum_cost)
  | accesses ->
     let (next_dp, mapping) =
       try
         mapper accesses dp
       with e ->
         let () = Printf.printf "23: %d %!" accum_cost in
         raise e in
     let () = match mapping with
       | {thread_mapping; page_mapping} ->
          let () = print_mapping thread_mapping in
          Printf.printf "\n\n%!" in
     simulation_cost_helper
       machine
       mapper
       mapping
       trace_fetch
       (next_dp, accum_cost + trace_cost machine mapping accesses);;

let run dp =
  let remap_rate = 10000 in
  let (next_dp, cost_1) = simulation_cost
                            asym_max
                            bipart_mapper
                            trace_file
                            remap_rate
                            num_threads
                            num_pages
                            dp in
  let () = Printf.printf "Cost was %d for bipart on asym_max\n%!" cost_1 in
  next_dp;;

let () =
  let dp = Searches.empty in
  ignore (run dp);;
