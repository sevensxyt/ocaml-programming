(* twice, no arguments *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)

let quad = twice double (* int -> int *)
let fourth = twice square (* int -> int *)

(* mystery operator 1 *)
let ( $ ) f x = f x
(*
applies f on x

precedence order from high to low:
function -> */ -> +- -> custom operators (&)
*)

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f (* f(g(x)) *)

(* repeat *)
let rec repeat f n x =
  match n with
  | 0 -> x
  | n -> repeat f (n-1) (f x)

(* product *)
let product_left lst = List.fold_left (fun acc n -> n +. acc) 1.0 lst
let product_right lst = List.fold_right (fun n acc -> n +. acc) lst 1.0

(* terse *)
let terse_product_left = List.fold_left ( *. ) 1.0
let terse_product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0
  
(* sum_cube_odd *)
let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j
let sum_cube_odd n =
  List.fold_left (+) 0 ( List.map (fun n -> n * n * n) (0--n) ) 

(* sum_cube_odd pipeline *)
let sum_cube_odd_pipelined n =
  0 -- n
  |> List.map(fun n -> n * n * n)
  |> List.fold_left (+) 0

(* exists *)
let rec exists_rec p lst =  
  match lst with
  | [] -> false
  | h::t -> if (p h) then true else exists_rec p t 

let exists_fold p lst =  
  List.fold_left (fun acc x -> (p x) || acc) false lst 

let exists_lib p lst =
  List.exists p lst

(* account balance *)
let account_balance_fold_left debits balance =
  List.fold_left (-) balance debits

let acount_balance_fold_right debits balance =
  List.fold_right (-) debits balance

let rec account_balance_rec debits balance =
  match debits with
    | [] -> balance
    | h::t -> account_balance_rec t (balance-h)

(* library uncurried *) 
let uncurried_append (lst, lst') = List.append lst lst' 
let uncurried_compare (c, c') = Char.compare c c'
let uncurried_max (n, n') = Stdlib.max n n'

(* map composition *) 
let map_comp f g lst =
  List.map (fun x -> f (g x)) lst

(* more list fun *)
let length_gt_3 lst =
  List.filter (fun s -> String.length s > 3) lst

let add_1 lst =
  List.map (fun n -> n +. 1.0) lst

let seperate lst sep =
  List.fold_left (fun acc s -> s ^ sep ^ acc ) "" lst

(* association list keys *)
let keys lst = List.sort_uniq (fun (h, _) (h', _) -> h - h') lst    

(* valid matrix *)
let is_valid_matrix matrix =
  let rows = List.map List.length matrix in
  match rows with
  | 0::_ | [] -> false
  | n::t -> List.for_all (fun n' -> n' = n) t

(* row vector add *)
let add_row_vectors v v' =
  List.map2 (fun e e' -> e + e') v v'

(* matrix add *)
let add_matrices m m' =
    List.map2 (fun v v' -> add_row_vectors v v') m m'

(* matrix transposition *)
let transpose matrix =
  if not (is_valid_matrix matrix) then failwith "Invalid matrix"
  else let rec helper matrix =
    match matrix with
    | []::_ | [] -> []
    | _::_ -> (List.map List.hd matrix)::(helper (List.map List.tl matrix))
  in helper matrix

(* row vector dot *)      
let row_vector_dot v v' =
  if not (List.compare_lengths v v' = 0) then failwith "Size mismatch"
  else
    List.map2 (fun e e' -> e * e') v v'
    |> List.fold_left (+) 0

(* matrix multiply *)
let multiply_matrices m m' =
  if not (is_valid_matrix m) || not (is_valid_matrix m') then failwith "Invalid matrix"
  else let m'_t = transpose m' in
  List.map (fun v -> List.map (fun v' -> row_vector_dot v v') m'_t) m
