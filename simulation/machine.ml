
type machine = int array array;;

let num_processors = Array.length;;

let print_machine machine =
  let num_proc = num_processors machine in
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

let symmetric_machine = create_machine (fun x y -> if x == y then 0 else 1);;

(** cost to send data from processor1 to processor2 
    (not necessarily symmetric) *)
let distance machine processor1 processor2 =
  Array.get (Array.get machine processor1) processor2;;


(** levels of asymmetry in the model machine *)
type asymmetry_levels = Zero | One | Two | Three | Four;;

(** in the most extreme asymmetry, cost mappings are:
    Zero -> 0
    One -> 10
    Two -> 15
    Three -> 25
    Four -> 35 *)
let asymmetric_machine =
  [|
    [| Zero; One; Two; Three; Three; Four; Three; Four; |];
    [| One; Zero; Three; Two; Four; Three; Four; Three; |];
    [| Three; Four; Zero; One; Three; Two; Three; Three; |];
    [| Four; Three; One; Zero; Three; Three; Four; Four; |];
    [| Three; Three; Three; Three; Zero; One; Two; Four; |];
    [| Four; Four; Two; Three; One; Zero; Three; Three; |];
    [| Two; Three; Three; Four; Three; Four; Zero; One; |];
    [| Three; Three; Three; Two; Four; Three; One; Zero; |];
  |];;

let machine_map machine f =
  Array.map (Array.map f) machine;;

let asym_0 =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | _ -> 24);;

let asym_1 =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | One -> 21
      | Two -> 22
      | Three -> 24
      | Four -> 26);;


let asym_2 =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | One -> 18
      | Two -> 20
      | Three -> 25
      | Four -> 28);;


let asym_3 =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | One -> 15
      | Two -> 19
      | Three -> 25
      | Four -> 31);;

let asym_4 =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | One -> 13
      | Two -> 17
      | Three -> 25
      | Four -> 33);;
  
let asym_max =
  machine_map
    asymmetric_machine
    (fun x ->
      match x with
      | Zero -> 0
      | One -> 10
      | Two -> 15
      | Three -> 25
      | Four -> 35);;
                 
