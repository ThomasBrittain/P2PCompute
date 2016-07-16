open OUnit

let test_1 test_ctx =
  assert_equal ws_feed_received' (ws_type_of_string ws_feed_received)

let suite =
  "suite" >::: [
    "test_1" >:: test_1;
  ]

let _ = run_test_tt_main suite
