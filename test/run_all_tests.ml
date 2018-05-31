open OUnit2

let () =
  run_test_tt_main Matrix_tests.suite;
  run_test_tt_main Galois_tests.suite
