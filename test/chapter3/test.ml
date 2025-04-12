open OUnit2
open Exercises
open Test_utils

let make_product_test name input expected =
    make_unary_test name input expected product string_of_int

let make_list_max_string_test name input expected =
    make_unary_test name input expected list_max_string Fun.id

let make_take_test name (n, lst) expected =
    name >:: (fun _ -> assert_equal expected (take n lst) ~printer:string_of_int_list)

let make_drop_test name (n, lst) expected =
    name >:: (fun _ -> assert_equal expected (drop n lst) ~printer:string_of_int_list)

let make_list n = List.tl(List.init (n+1) Fun.id)

let product_tests = "test suite for product" >::: [
    make_product_test "empty list" [] 1;
    make_product_test "list from 1 to 5" (make_list 5) 120;
    make_product_test "list from 1 to 10" (make_list 10) 3_628_800;
    ]

let list_max_string_tests = "test suite for list_max_string" >::: [
    make_list_max_string_test "empty list" [] "empty";
    make_list_max_string_test "list from 1 to 5" (make_list 5) "5";
    make_list_max_string_test "list from 1 to 10" (make_list 10) "10";
    ] 

let big_list = make_list 100_000
let take_tests = "test suite for take" >::: [
    make_take_test "take 10_000" (10_000, big_list) (make_list 10_000);
    make_take_test "take 50_000" (50_000, big_list) (make_list 50_000);
    make_take_test "take 100_000" (100_000, big_list) (make_list 100_000);
    ]

let small_list = make_list 10
let drop_tests = "test suite for drop" >::: [
    make_drop_test "drop 0" (0, small_list) small_list;
    make_drop_test "drop 5" (5, small_list) ([6; 7; 8; 9; 10]);
    make_drop_test "drop 10" (10, small_list) [];
]

let tests = [product_tests; list_max_string_tests; take_tests; drop_tests]
let _ = List.map (fun lst -> run_test_tt_main lst) tests

