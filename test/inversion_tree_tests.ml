open OUnit2
open Reed_solomon_erasure__
open Reed_solomon_erasure__.Inversion_tree

let test_new_inversion_tree test_ctxt =
  let tree = make 3 2 in

  let expect = Matrix.make_with_data [|Bytes.of_string "\001\000\000";
                                       Bytes.of_string "\000\001\000";
                                       Bytes.of_string "\000\000\001"|] in

  assert_equal (Some expect) (get_inverted_matrix tree [])

let suite =
  "inversion_tree_tests">:::
  ["test_new_inversion_tree">:: test_new_inversion_tree;
  ]
