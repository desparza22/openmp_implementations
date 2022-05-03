#use "machine.ml";;
#use "bipartition.ml";;

type mapping = int array;;
type thread_page_mapping = {thread_mapping: mapping;
                            page_mapping: mapping};;

type remapping_info = {agent: int; destination: int};;
type remapping_type = Thread_remap | Page_remap;;
type remapping = remapping_type * remapping_info;;

let consistent_mapper map_func num_threads num_pages num_processors =
  let initial =
    {thread_mapping = map_func num_threads num_processors;
     page_mapping = map_func num_pages num_processors;} in
  (initial, (fun accesses -> initial));;


let rec create_mapping mapping_func num_agents num_processors =
  let mapping = Array.make num_agents (-1) in
  let () = fill_mapping mapping_func num_processors mapping 0 in
  mapping
  
and fill_mapping mapping_func num_processors mapping from =
  match Int.equal (Array.length mapping) from with
  | true -> ()
  | false -> let () = Array.set mapping from (mapping_func from num_processors) in
             fill_mapping mapping_func num_processors mapping (from + 1);;

let create_random_mapping seed =
  create_mapping
    (fun agent num_processors ->
      let () = Random.init (100000 * seed + agent) in
      Random.int num_processors);;

let create_mod_mapping =
  create_mapping
    (fun agent num_processors ->
      agent mod num_processors);;

let mod_mapper = consistent_mapper create_mod_mapping;;
let random_mapper seed = consistent_mapper (create_random_mapping seed);;

let processor mapping agent =
  try
    Array.get mapping agent
  with Invalid_argument s ->
        let () = Printf.printf
                   "Attempting to access element %d in mapping of length %d\n"
                   agent
                   (Array.length mapping) in
        raise (Invalid_argument s);;


let print_mapping mapping =
  let () = Array.iteri (Printf.printf "(%d, %d), ") mapping in
  print_string "\n";;


let remap mapping {agent; destination} =
  let () = Array.set mapping agent destination in
  mapping;;

let access_cost machine {thread_mapping; page_mapping} (_, {thread; page}) =
  let p1 =
    try
      processor thread_mapping thread
    with e ->
      let () = Printf.printf "001%!" in
      raise e in
  let p2 =
    try
      processor page_mapping page
    with e ->
      let () = Printf.printf "002%!" in
      raise e in
  try
    distance machine p1 p2
  with e ->
    let () = Printf.printf "%d %d %!\n" p1 p2 in
    raise e;;

let rec trace_cost machine mapping = function
  | [] -> 0
  | a::rest -> (access_cost machine mapping a) +
                 (trace_cost machine mapping rest);;

type decision_state = {thread: int;
                       mutable node_current: node;
                       mutable node_other: node;
                       mutable cost_current: int;
                       mutable cost_other: int;};;
exception Complete;;

let rec bipart_mapper machine num_threads num_pages num_processors =
  let initial =
    {thread_mapping = create_mod_mapping num_threads num_processors;
     page_mapping = create_mod_mapping num_pages num_processors} in
  (initial, bipart_mapper_helper
              machine num_threads num_pages num_processors)

and bipart_mapper_helper machine num_threads num_pages num_processors accesses dp =
  (*create num_threads x num_pages array*)
  let access_array = Array.make_matrix num_threads num_pages 0 in
  let () = fill_access_array access_array accesses in
  (*create num_threads x num_threads array of shared accesses, cost is min*)
  let communication_array = Array.make_matrix num_threads num_threads 0 in
  let () = fill_communication_array communication_array access_array 0 0 1 in
  let (new_dp, thread_mapping) = bipart_threads
                                   dp
                                   machine
                                   bipart_tree
                                   communication_array
                                   num_threads
                                   num_processors in
  let page_mapping = bipart_pages
                       access_array thread_mapping num_pages num_processors in
  (new_dp, {thread_mapping=thread_mapping; page_mapping=page_mapping})

and fill_access_array arr acc =
  match acc with
  | [] -> ()
  | (_, {thread; page})::rest ->
     let prev =
       try
         Array.get (Array.get arr thread) page
       with e ->
         let () = Printf.printf "1%!" in
         raise e in
     
     let () =
       try
         Array.set (Array.get arr thread) page (prev + 1)
       with e ->
         let () = Printf.printf "2%!" in
         raise e in
     
     fill_access_array arr rest

and fill_communication_array communication_array access_array page t1 t2 =
  if page >= try
              Array.length (Array.get access_array 0)
            with e ->
              let () = Printf.printf "3%!" in
              raise e
  then ()
  else
    if t1 == t2
    then fill_communication_array
           communication_array access_array page 0 (t2 + 1)
    else
      if t2 == Array.length access_array
      then fill_communication_array
             communication_array access_array (page + 1) 0 1
      else
        let t1_acc =
          try
            Array.get (Array.get access_array t1) page
          with e ->
            let () = Printf.printf "4%!" in
            raise e in
        
        let t2_acc =
          try
            Array.get (Array.get access_array t2) page
          with e ->
            let () = Printf.printf "5%!" in
            raise e in
        
        let min = if t1_acc < t2_acc then t1_acc else t2_acc in
        let prev =
          try
            Array.get (Array.get communication_array t1) t2
          with e ->
            let () = Printf.printf "6%!" in
            raise e in
        
        let () =
          try
            Array.set (Array.get communication_array t1) t2 (prev + min)
          with e ->
            let () = Printf.printf "7%!" in
            raise e in
        
        let () =
          try
            Array.set (Array.get communication_array t2) t1 (prev + 1)
          with e ->
            let () = Printf.printf "8%!" in
            raise e in
        fill_communication_array
          communication_array access_array page (t1 + 1) t2

and bipart_threads dp machine tree communication_array num_threads num_processors =
  let states = init_states (num_threads-1) tree in
  let k = num_threads in
  let c = (Int.div num_threads 4) + 1 in
  let new_dp = push_to_bottom dp machine states k c 2 in
  let ordered_states = List.sort
                         (fun {thread=t1; _} {thread=t2; _} -> t1 - t2)
                         states in
  let machines = List.map
                   (fun {node_current;_} ->
                     match node_current with
                     | Node (node_id, Empty, proc::[], Empty) -> proc
                     | _ -> raise (Invalid_argument
                                     "thread mapped to non-leaf node\n"))
                   ordered_states in
  (new_dp, Array.of_list machines)
                     

and init_states thread head =
  match thread < 0 with
  | true -> []
  | false ->
     {thread=thread;
      node_current=head;
      node_other=head;
      cost_current=0; cost_other=0;}::init_states (thread-1) head

and push_to_bottom dp machine states k c nodes_considering =
  try
    let () = push_down states in
    let extra_per_node = 1 in
    let new_dp = shuffle
                   dp
                   machine
                   states
                   k
                   c
                   (Array.make
                      nodes_considering
                      extra_per_node) in
    push_to_bottom new_dp machine states k c (nodes_considering * 2)
    
  with Invalid_argument s -> raise (Invalid_argument s)
     | Complete -> dp

and push_down states =
  match states with
  | [] -> ()
  | state::rest -> match state with
                   | {node_current=Empty; _} -> raise (Invalid_argument "")
                   | {node_current=Node (_, Empty, _, _); _} -> raise Complete
                   | {thread=n; node_current=Node (_, node_l, _, node_r); _} ->
                      let () = if Random.bool ()
                               then let () = state.node_current <- node_l in
                                    state.node_other <- node_r
                               else let () = state.node_current <- node_r in
                                    state.node_other <- node_l in
                      push_down rest

and shuffle dp machine states k c switch_capacity =
  let new_dp = calc_costs dp machine states in
  match k <= 0 with
  | true -> new_dp
  | false -> let sorted =
               List.sort
                 (fun d1 d2 -> let gain1 = d1.cost_other - d1.cost_current in
                               let gain2 = d2.cost_other - d2.cost_current in
                               gain2 - gain1)
                 states in
             let () = flip_top sorted c switch_capacity in
             shuffle new_dp machine sorted (k - 1) c switch_capacity

and calc_costs dp machine states =
  let final_dp =
    List.fold_left
      (fun dp_run_outer state ->
        match state with
        | {node_current; node_other; _} ->
           let f x dp_cur =
             (List.fold_left
                (fun (dp_run, cost) {node_current=other_current; _} ->
                  let (new_dp, node_to_node_cost) = node_distances
                                                      dp_run
                                                      x
                                                      other_current
                                                      machine in
                  (new_dp, cost + node_to_node_cost))
                (dp_cur, 0)
                states) in
           let (new_dp2, cost_current) = f node_current dp_run_outer in
           let (new_dp3, cost_other) = f node_other new_dp2 in
           let (new_dp4, cost_to_self) = node_distances
                                           new_dp3
                                           node_other
                                           node_current
                                           machine in
           let () = state.cost_current <- cost_current in
           let () = state.cost_other <- cost_other - cost_to_self in
           new_dp4)
      dp
      states in
  final_dp

and flip_top states c switch_capacity =
  match states with
  | [] -> ()
  | decision::rest ->
     match decision with
     | {node_current=Node(current_id, _, _, _);
        node_other=Node(other_id, _, _, _);
        _} ->
        (match c <= 0 with
        | true -> ()
        | false -> (match
                      (try
                         Array.get switch_capacity other_id
                       with e ->
                         let () = Printf.printf "9%!" in
                         raise e)
                    with
                   | 0 -> flip_top rest c switch_capacity
                   | other_cap ->
                      let () =
                        try
                          Array.set
                            switch_capacity other_id (other_cap-1)
                        with e ->
                          let () = Printf.printf "01%!" in
                          raise e in
                      let current_cap =
                        try
                          Array.get switch_capacity current_id
                        with e ->
                          let () = Printf.printf "10%!" in
                          raise e in
                      let () =
                        try
                          Array.set
                            switch_capacity
                            current_id
                            (current_cap + 1)
                        with e ->
                          let () = Printf.printf "02%!" in
                          raise e in
                      let temp = decision.node_current in
                      let () = decision.node_current <- decision.node_other in
                      let () = decision.node_other <- temp in
                      flip_top rest (c - 1) switch_capacity))
     | _ -> raise (Invalid_argument "Decision in flip_top is at empty nodes\n")

and bipart_pages access_array thread_mapping num_pages num_processors =
  let base = Array.make num_pages (-1) in
  let num_threads = Array.length access_array in
  Array.mapi
    (fun idx _ ->
      let rec max_from access_array thread page =
        match thread == num_threads with
        | true -> (-1, -1)
        | false -> let (max_thread, max_access) =
                     max_from access_array (thread + 1) page in
                   let here =
                     try
                       Array.get (Array.get access_array thread) page
                     with e ->
                       let () = Printf.printf "11%!" in
                       raise e in
                   if here > max_access
                   then (thread, here)
                   else (max_thread, max_access) in
      let (max_thread, max_accesses) = max_from access_array 0 idx in
      Array.get thread_mapping max_thread)
    base;;

