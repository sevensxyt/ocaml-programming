open OUnit2

let make_list n = List.tl(List.init (n+1) Fun.id)

let make_unary_test name input expected f formatter =
  name >:: (fun _ -> assert_equal expected (f input) ~printer:formatter)

let string_of_int_list lst =
    "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let string_of_int_matrix matrix =
  "[" ^ String.concat "; " (List.map string_of_int_list matrix) ^ "]" 


