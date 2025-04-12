open OUnit2
open Exercises
open Test_utils

let make_is_valid_matrix_test name input expected =
    make_unary_test name input expected is_valid_matrix string_of_bool 

let make_transpose_matrix_test name input expected = 
    make_unary_test name input expected transpose string_of_int_matrix 

let make_test name (input1, input2) expected f throws_error error formatter =
    name >:: (fun _ ->
        if throws_error then
            assert_raises error (fun () -> f input1 input2)
        else
            assert_equal expected (f input1 input2) ~printer:formatter
    )

let make_add_row_vectors_test name (vector, vector') expected throws_error =
    make_test name (vector, vector') expected add_row_vectors throws_error (Invalid_argument "List.map2") string_of_int_list

let make_add_matrices_test name (matrix, matrix') expected throws_error =
    make_test name (matrix, matrix') expected add_matrices throws_error (Invalid_argument "List.map2") string_of_int_matrix

let make_row_vector_add_test name (vector, vector') expected throws_error =
    make_test name (vector, vector') expected row_vector_dot throws_error (Failure "Size mismatch") string_of_int

let make_multiply_matrices_test name (matrix, matrix') expected throws_error error =
    make_test name (matrix, matrix') expected multiply_matrices throws_error error string_of_int_matrix

let is_valid_matrix_tests = "test suite for is_valid_matrix" >::: [
    make_is_valid_matrix_test "empty 1D matrix" [] false;
    make_is_valid_matrix_test "empty 2D matrix" [[]] false;
    make_is_valid_matrix_test "valid 1D matrix" [[1; 2]] true;
    make_is_valid_matrix_test "invalid 2D matrix" [[1; 2]; [3]] false;
    make_is_valid_matrix_test "valid 2D matrix" [[1; 2]; [3; 4]] true;
    ]

let add_row_vectors_tests = "test suite for add_row_vectors" >::: [
    make_add_row_vectors_test "length mismatch" ([1; 2;], [1;]) [] true;
    make_add_row_vectors_test "matching length" ([1; 2; 3], [1; 2; 3]) [2; 4; 6] false;
    ]

let add_matrices_tests = "test suite for add_matrices" >::: [
    make_add_matrices_test "size mismatch" ([[1; 2; 3]; [4; 5; 6]], [[1; 2;]; [3; 4;]]) [] true;        
    make_add_matrices_test "matching size" ([[1; 2; 3]; [4; 5; 6]], [[1; 2; 3]; [4; 5; 6]]) [[2; 4; 6]; [8; 10; 12]] false;        
    ]

let transpose_matrix_tests = "test suite for transpose" >::: [
    "invalid matrix" >:: (fun _ -> assert_raises (Failure "Invalid matrix") (fun () -> transpose [[1; 2]; [3]]));
    make_transpose_matrix_test "2x1 matrix" [[1]; [2]] [[1; 2]];
    make_transpose_matrix_test "2x2 matrix" [[1; 2]; [3; 4]] [[1; 3]; [2; 4]];
    make_transpose_matrix_test "2x3 matrix" [[1; 2; 3;]; [4; 5; 6]] [[1; 4]; [2; 5]; [3; 6]];
    ]

let row_vector_dot_tests = "test suite for row_vector_dot" >::: [
    make_row_vector_add_test "size mismatch" ([1; 2], [3]) 0 true;
    make_row_vector_add_test "size 1x3" ([1; 2; 3], [4; 5; 6]) 32 false;
    make_row_vector_add_test "size 1x5" ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10]) 130 false;
    ]

let multiply_matrices_tests = "test suite for multiply_matrices" >::: [
    make_multiply_matrices_test "invalid matrix" ([[1; 2]; [3]], [[1; 2]; [1; 2]]) [] true (Failure "Invalid matrix"); 
    make_multiply_matrices_test "size mismatch" ([[1; 2]; [3; 4]], [[1]; [2]]) [] true (Failure "Size mismatch"); 
    make_multiply_matrices_test "2x2 * 2x2" ([[1; 2]; [3; 4]], [[5; 6]; [7; 8]]) [[19; 22]; [43; 50]] false (Failure ""); 
    make_multiply_matrices_test "2x3 * 3x1" ([[1; 2; 3]; [4; 5; 6]], [[1]; [2]; [3]]) [[14; 32]] false (Failure ""); 
    ]

let _  =
    [is_valid_matrix_tests; add_row_vectors_tests; add_matrices_tests; transpose_matrix_tests; row_vector_dot_tests]
    |> List.map (fun lst -> run_test_tt_main lst)
