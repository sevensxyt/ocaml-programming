let rec combine f acc = function
  | [] -> acc
  | h :: t -> f h (combine f acc t)

let rec combine' f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (combine' f t acc)

let rec fold_right f lst (acc : 'acc) = match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)
(* 3 - (2 - (1 - 0)) *)

let rec combine_tr f acc = function
  | [] -> acc
  | h :: t -> combine_tr f (f acc h) t 
(* (((0 - 3) - 2) - 1) *)

let lst_and lst = List.fold_left (fun acc x -> acc && x) true lst
let lst_or lst = List.fold_left (fun acc x -> acc || x) false lst

