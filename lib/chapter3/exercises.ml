(* list expressions *)
let _ = [1; 2; 3; 4; 5]
let _ = 1 :: 2 :: 3 :: 4 :: [5]
let _ = [1] @ [2; 3; 4] @ [5]

(* product *)
let rec product lst =
  match lst with
  | [] -> 1
  | h::t -> h * product t

let product_tr lst = 
  let rec helper lst acc = 
    match lst with 
    | [] -> acc
    | h::t -> helper t (acc*h)
  in 
  helper lst 1

(* concat *)
let rec concat lst = 
  match lst with
  | [] -> ""
  | h::t -> h ^ concat t

let concat_tr lst = 
  let rec helper lst acc = 
    match lst with
    | [] -> acc
    | h::t -> helper t acc ^ h 
  in 
  helper lst ""

(* product test *)
(* in test.ml *)

(* patterns *)
let f lst = 
  let rec helper lst count =
    match (lst, count) with 
    | ([], 2) | ([], 4) -> true
    | (h::_, 0) when h = "bigred" -> true
    | (a::b::_, 0) when a = b -> true 
    | (_::t, n) -> helper t (n+1)
    | _ -> false
  in 
  helper lst 0

(* library puzzle *)
let last lst = List.hd (List.rev lst)
let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* take drop *)
let take n lst = 
  let rec helper n lst acc = 
    match (n, lst) with 
    | (0, _) | (_, []) -> acc
    | (n, h::t) -> helper (n-1) t (h::acc)
  in
  List.rev (helper n lst [])

let rec drop n lst = 
  match (n, lst) with
  | (0, _) -> lst
  | (_, []) -> []
  | (n, _::t) -> drop (n-1) t

(* take drop t *)
(* in test.ml *)

(* unimodal *)

(* powerset *)

(* print int list rec *)
let rec print_int_list lst = 
  match lst with 
  | [] -> ()
  | h::t -> print_endline (string_of_int h); print_int_list t

(* print int list iter *)
let print_int_list' lst = 
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* student *)
type student = {first_name : string; last_name : string; gpa : float}
let student = {
  first_name = "seven";
  last_name = "xyt";
  gpa = 3.5;
}
let full_name student = student.first_name ^ " " ^ student.last_name
let create_student first last gpa = {
  first_name = first;
  last_name = last;
  gpa = gpa;
} 

(* pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype : poketype}

let charizard = {
  name = "charizard";
  hp = 78;
  ptype = Fire; 
}

let squirtle = {
  name = "squirtle";
  hp = 44;
  ptype = Water; 
}

(* safe hd and tl *)
let safe_hd lst = 
  match lst with 
  | [] -> None
  | h::_ -> Some h

let rec safe_tl lst = 
  match lst with 
  | [] -> None
  | h::[] -> Some h
  | _::t -> safe_tl t 

(* pokefun *)
let max_hp lst =
  match lst with
  | [] -> None
  | h::t -> Some (List.fold_left (fun acc p -> if p.hp > acc.hp then p else acc) h t)

(* date before *)
type date = int * int * int

let max_day m =
    match m with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 2 -> 28
    | 4 | 6 | 9 | 11 -> 30
    | _ -> 0

let is_valid_date (y, m, d) =
      y > 0 && (m >= 1 && m <= 12) && (d >= 1 && d <= max_day m)

let is_before (y1, m1, d1) (y2, m2, d2)=
   match (is_valid_date (y1, m1, d1), is_valid_date (y2, m2, d2)) with
    | (false, _) | (_, false) -> false
    | (true, true) ->
      (y1 < y2) ||
      (y1 = y2 && m1 < m2) ||
      (y1 = y2 && m1 = m2 && d1 < d2)

(* earliest date *)
let earliest lst =
  match lst with
  | [] -> None
  | h::t -> Some (List.fold_left (fun d acc -> if is_before d acc then d else acc) h t)

(* assoc list *)
let insert k v lst = (k, v) :: lst

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

let lst =
  []
  |> insert 1 "one"
  |> insert 2 "two"
  |> insert 3 "three"

let _ = lookup 2 lst 
let _ = lookup 4 lst

(* cards *)
type suit = Clubs | Diamonds | Hearts | Spades
type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type card = {suit : suit; rank : rank}
let ace_of_clubs = {suit = Clubs; rank = Ace} 
let queen_of_hearts = {suit = Hearts; rank = Queen} 
let two_of_diamonds = {suit = Diamonds; rank = Two} 
let seven_of_spades = {suit = Spades; rank = Seven} 

(* matching *)
(* Some x :: tl *)
let _ = None::[]
(* [Some 3110; None] *)
let _ = Some 1::[]
(* [Some x; _] *)
let _ = None::[] 
(* h1 :: h2 :: tl *)
let _ = 1::[]
(* h :: tl *)
(* impossible *)

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign x = 
  if x > 0 then Pos
  else if x = 0 then Zero
  else Neg

let _quadrant (x, y) = 
  match (sign x, sign y) with 
  | (Pos, Pos) -> Some I 
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III 
  | (Pos, Neg) -> Some IV 
  | (Zero, _) | (_, Zero) -> None

(* quadrant when *)
let quadrant_when (x, y) = 
  match (x, y) with
  | (x, y) when x > 0 && y > 0 -> Some I 
  | (x, y) when x < 0 && y > 0 -> Some II 
  | (x, y) when x < 0 && y < 0 -> Some III 
  | (x, y) when x > 0 && y < 0 -> Some IV 
  | (x, y) when x = 0 || y = 0 -> None
  | _ -> None

type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

(* depth *)
let depth tree =
  let rec helper tree acc =
    match tree with
    | Leaf -> 1 + acc
    | Node (_, l, r) -> max (helper l (acc+1)) (helper r (acc+1))
  in
  helper tree 0

(* shape *)
let rec same_shape t t' =
  match (t, t') with
  | (Leaf, Leaf) -> true 
  | (Node (_), Leaf) | (Leaf, Node (_)) -> false
  | (Node (_, l, r), Node (_, l', r')) -> (same_shape l l') && (same_shape r r')  

let same_shape_tr t1 t2 =
  let rec helper pairs =
    match pairs with
    | [] -> true
    | (Leaf, Leaf)::t -> helper t
    | (Node (_, _, _), Leaf)::_ | (Leaf, Node (_, _, _))::_ -> false
    | (Node (_, l1, r1), Node (_, l2, r2))::t -> helper ((l1, l2)::(r1, r2)::t)
  in
  helper [(t1, t2)]

(* list max exn *)
let list_max lst = 
  if lst = [] then failwith "list_max"
  else List.fold_left (fun acc x -> if acc > x then acc else x) min_int lst

(* list max exn string *)
let list_max_string lst =
  if lst = [] then "empty"
  else string_of_int (list_max lst)

(* list max exn ounit *)
(* in test.ml *)

(* is_bst *)
type bst_result =
  | Empty
  | Invalid
  | Bst of int * int

let is_bst tree =
  let rec helper tree =
    match tree with
    | Leaf -> Empty
    | Node (v, l, r) ->
      match (helper l, helper r) with
      | (Invalid, Invalid) -> Invalid
      | (Empty, Empty) -> Bst (v, v)
      | (Empty, Bst(min, max)) when v < min -> Bst(v, max) 
      | (Bst(min, max), Empty) when v > max -> Bst(min, v) 
      | (Bst (_, max), Bst (min, _)) when (v < min && v > max) -> Bst (min, max)
      | _ -> Invalid
  in
  match helper tree with
    | Bst (_, _) -> true
    | Empty | Invalid -> false

(* quadrant poly *)
let sign x = 
  if x > 0 then `Pos
  else if x = 0 then `Zero
  else `Neg

let quadrant (x, y) = 
  match (sign x, sign y) with 
  | (`Pos, `Pos) -> Some `I 
  | (`Neg, `Pos) -> Some `II
  | (`Neg, `Neg) -> Some `III 
  | (`Pos, `Neg) -> Some `IV 
  | (`Zero, _) | (_, `Zero) -> None

