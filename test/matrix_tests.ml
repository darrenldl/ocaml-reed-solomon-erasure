open Reed_solomon_erasure__.Matrix
open OUnit2

let make_random_matrix (size : int) : matrix =
  let data = Array.make size Bytes.empty in
  let data = Array.map (fun _ -> Bytes.map (fun _ -> char_of_int (Random.int 256)) (Bytes.make 100 '\x00')) data in
  make_with_data data

let test_matrix_row_count test_ctxt =
  let m1 = make_with_data [|Bytes.of_string "\x01\x00\x00"|] in
  let m2 = make_with_data [|Bytes.of_string "\x00\x00\x00";
                            Bytes.of_string "\x00\x00\x00"|] in
  let m3 = make 1 4 in

  assert_equal 1 (row_count m1);
  assert_equal 2 (row_count m2);
  assert_equal 1 (row_count m3)

let suite =
  "matrix_tests">:::
  ["test_matrix_row_count">:: test_matrix_row_count;
  ]

let () =
  run_test_tt_main suite
