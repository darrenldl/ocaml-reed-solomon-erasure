open OUnit2
open Reed_solomon_erasure__
open Reed_solomon_erasure__.Inversion_tree
open Matrix_tests

let test_new_inversion_tree test_ctxt =
  let tree = make 3 2 in

  let expect = Matrix.make_with_data [|Bytes.of_string "\001\000\000";
                                       Bytes.of_string "\000\001\000";
                                       Bytes.of_string "\000\000\001"|] in

  assert_equal (Some expect) (get_inverted_matrix tree [])

let test_get_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix = match get_inverted_matrix tree [] with
    | Some m -> m
    | None   -> failwith ""
  in

  let expect = Matrix.make_with_data [|Bytes.of_string "\001\000\000";
                                       Bytes.of_string "\000\001\000";
                                       Bytes.of_string "\000\000\001"|] in

  assert_equal expect matrix;

  let matrix = get_inverted_matrix tree [1] in
  assert_equal None matrix;

  let matrix = get_inverted_matrix tree [1; 2] in
  assert_equal None matrix;

  let matrix = Matrix.make 3 3 in
  let matrix_copy = Matrix.copy matrix in

  (match insert_inverted_matrix tree [1] matrix with
   | Ok _    -> ()
   | Error _ -> assert_failure ""
  );

  let cached_matrix = get_inverted_matrix tree [1] in
  assert_equal (Some matrix_copy) cached_matrix

let test_insert_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix      = Matrix.make 3 3 in
  let matrix_copy = Matrix.copy matrix in

  (match insert_inverted_matrix tree [1] matrix with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );
  (match insert_inverted_matrix tree [] matrix_copy with
   | Ok _  -> assert_failure ""
   | Error _ -> ()
  );

  let matrix = Matrix.make 3 2 in
  (match insert_inverted_matrix tree [2] matrix with
   | Ok _ -> assert_failure ""
   | Error _ -> ()
  );

  let matrix = Matrix.make 3 3 in
  (match insert_inverted_matrix tree [0;1] matrix with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  )

let test_double_insert_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix1 = Matrix_tests.make_random_matrix 3 in
  let matrix2 = Matrix_tests.make_random_matrix 3 in

  let matrix_copy1 = Matrix.copy matrix1 in
  let matrix_copy2 = Matrix.copy matrix2 in

  (match insert_inverted_matrix tree [1] matrix_copy1 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );
  (match insert_inverted_matrix tree [1] matrix_copy2 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );

  let cached_matrix = get_inverted_matrix tree [1] in
  assert_equal (Some matrix_copy2) cached_matrix

let suite =
  "inversion_tree_tests">:::
  ["test_new_inversion_tree">::            test_new_inversion_tree;
   "test_get_inverted_matrix">::           test_get_inverted_matrix;
   "test_insert_inverted_matrix">::        test_insert_inverted_matrix;
   "test_double_insert_inverted_matrix">:: test_double_insert_inverted_matrix
  ]
