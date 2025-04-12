(* values *)
let _: int = 7 * (1 + 2 + 3)
let _: string = "CS " ^ string_of_int 3110

(* operators *)
let _ = 42 * 10
let _ = 3.14 /. 2.0
let _ = 4.2 ** 7.

(* equality *)
let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(* assert *)
let _ = assert (2110 != 3110)

(* if *)
let x = if 2 > 1 then 42 else 7 

(* double fun *)
let double x = x * 2
let _ = assert (double 2 = 4)
let _ = assert (double 5 = 10)
let _ = assert (double 99 = 198)

(* more fun *)
let cube x = x *. x *. x
let _ = assert (cube 2. = 8.)
let _ = assert (cube 5. = 125.)
let _ = assert (cube 10. = 1000.)

let sign x = if x > 0 then 1 else if x < 0 then -1 else 0
let _ = assert (sign 2 = 1)
let _ = assert (sign (-2) = -1)
let _ = assert (sign 0 = 0)

let circle_area r = 3.142 *. r ** 2.
let _ = assert (circle_area 1.0 = 3.142)
let _ = assert (circle_area 2.0 = 12.568)
let _ = assert (abs_float (circle_area 0.5 -. 0.7855) < 1e-4)

(* RMS *)
let rms x y = sqrt ((x *. x +. y *. y) /. 2.)
let _ = assert (rms 5.0 5.0 = 5.0)
let _ = assert (abs_float (rms 3.0 0.0 -. sqrt 4.5) < 1e-10)
let _ = assert (rms (-5.0) 5.0 = 5.0)

(* date fun *)
let date d m = 
  let max_day m = 
    match m with 
    | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 31
    | "Feb" -> 28
    | "Apr" | "Jun" | "Sep" | "Nov" -> 30
    | _ -> 0
  in d >= 1 && d <= max_day m 
let _ = assert (date 31 "Jan" = true)
let _ = assert (date 29 "Feb" = false)
let _ = assert (date 0 "Apr" = false)

(* fib *)
let rec fib n =
  match n with 
  | 0 -> 0
  | 1 -> 1
  | 2 -> 1
  | n -> fib (n-1) + fib (n-2)
let _ = assert (fib 0 = 0)
let _ = assert (fib 1 = 1)
let _ = assert (fib 5 = 5)

(* fib fast *)
let fib n =
  if n = 0 then 0 else
  let rec h n pp p = 
    if n = 1 then p 
    else h (n-1) p (pp+p)
  in h n 0 1
let _ = assert (fib 30 = 832040)
let _ = assert (fib 35 = 9227465)
let _ = assert (fib 40 = 102334155)

(* poly types *)
let f x = if x then x else x
(* val f : bool -> bool = <fun> *)
let g x y = if y then x else x
(* val g : 'a -> bool -> 'a = <fun> *)
let h x y z = if x then y else z
(* val h : bool -> 'a -> 'a -> 'a = <fun> *)
let i x y = if x then y else y
(* val i : bool -> 'a -> 'b -> 'a = <fun> *)

(* divide *)
let divide numerator = fun denominator -> numerator /. denominator

(* associativity *)
let add x y = x + y 
let _: int = add 5 1
let _: int -> int = add 5
let _: int = (add 5) 1
(* let _ = add (5 1) *)

