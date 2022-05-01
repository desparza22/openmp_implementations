#use "trace.ml";;
#use "simulate.ml";;
#use "useful.ml";;
#load "str.cma";;

let trace_file = "test.txt";;

(*let test_read chan n =
  let (accesses, lines_read) = read_trace_chan chan n 1000 in
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
