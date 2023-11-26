(* TEST *)

let test_consts () =
  assert (Int64.zero = 0L);
  assert (Int64.one = 1L);
  assert (Int64.minus_one = -1L);
  ()

let test_arith () =
  assert (Int64.add 2L 4L = 6L);
  assert (Int64.sub 6L 2L = 4L);
  assert (Int64.mul 6L 2L = 12L);
  assert (Int64.div 12L 2L = 6L);
  assert (Int64.rem 5L 2L = 1L);
  assert (Int64.succ 5L = 6L);
  assert (Int64.pred 5L = 4L);
  assert (Int64.abs (-5L) = 5L);
  assert (Int64.abs 5L = 5L);
  ()

let test_logops () =
  assert (Int64.logand 0xF0F0L 0xFFFFL = 0xF0F0L);
  assert (Int64.logor 0xF0FFL 0x0F0FL = 0xFFFFL);
  assert (Int64.logxor 0xF0FFL 0x0F0FL = 0xFFF0L);
  assert (Int64.lognot Int64.max_int = Int64.min_int);
  assert (Int64.shift_left 1L 4 = 16L);
  assert (Int64.shift_right 16L 4 = 1L);
  assert (Int64.shift_right (-16L) 4 = (-1L));
  assert (Int64.shift_right (-16L) 4 = (-1L));
  assert (Int64.shift_right_logical Int64.min_int 63 = 1L);
  ()

let test_equal () =
  assert (Int64.equal 1L 1L = true);
  assert (Int64.equal 1L 0L = false);
  ()

let test_compare () =
  assert (Int64.compare 3L 3L = 0);
  assert (Int64.compare 3L 4L = (-1));
  assert (Int64.compare 4L 3L = 1);
  assert (Int64.compare (-4L) 3L = -1);
  assert (Int64.compare 3L (-4L) = 1);
  ()

let test_float_conv () =
  assert (Int64.to_float 5L = 5.0);
  assert (Int64.of_float 5. = 5L);
  assert (Int64.of_float 5.9 = 5L);
  ()

let test_string_conv () =
  assert (Int64.to_string 50L = "50");
(*  assert (Int64.of_string "50" = Some 50);
  assert (Int64.of_string "" = None); *)
  ()

let test_min_max () =
  assert (Int64.max 2L 3L = 3L);
  assert (Int64.min 2L 3L = 2L)

let test_clz () =
  assert (Int64.count_leading_zeros 0L = 64);
  assert (Int64.count_leading_zeros (-1L) = 0);
  assert (Int64.count_leading_zeros Int64.min_int = 0);
  assert (Int64.count_leading_zeros Int64.max_int = 1);
  assert (Int64.count_leading_zeros (Int64.div Int64.max_int 2L) = 2);
  assert (Int64.count_leading_zeros (Int64.div Int64.min_int 2L) = 0);
  assert (Int64.count_leading_zeros 1L = 63);
  assert (Int64.count_leading_zeros 2L = 62);
  assert (Int64.count_leading_zeros 3L = 62)

let test_ctz () =
  assert (Int64.count_trailing_zeros 0L = 64);
  assert (Int64.count_trailing_zeros (-1L) = 0);
  assert (Int64.count_trailing_zeros Int64.min_int = 63);
  assert (Int64.count_trailing_zeros Int64.max_int = 0);
  assert (Int64.count_trailing_zeros (Int64.div Int64.max_int 2L) = 0);
  assert (Int64.count_trailing_zeros (Int64.div Int64.min_int 2L) = 62);
  assert (Int64.count_trailing_zeros 1L = 0);
  assert (Int64.count_trailing_zeros 2L = 1);
  assert (Int64.count_trailing_zeros 3L = 0)

let test_clrsb () =
  assert (Int64.count_leading_redundant_sign_bits 0L = 63);
  assert (Int64.count_leading_redundant_sign_bits (-1L) = 63);
  assert (Int64.count_leading_redundant_sign_bits Int64.min_int = 0);
  assert (Int64.count_leading_redundant_sign_bits Int64.max_int = 0);
  assert (Int64.count_leading_redundant_sign_bits (Int64.div Int64.max_int 2L) = 1);
  assert (Int64.count_leading_redundant_sign_bits (Int64.div Int64.min_int 2L) = 1);
  assert (Int64.count_leading_redundant_sign_bits 1L = 62);
  assert (Int64.count_leading_redundant_sign_bits 2L = 61);
  assert (Int64.count_leading_redundant_sign_bits 3L = 61)

let test_popcount () =
  assert (Int64.count_set_bits 0L = 0);
  assert (Int64.count_set_bits (-1L) = 64);
  assert (Int64.count_set_bits Int64.min_int = 1);
  assert (Int64.count_set_bits Int64.max_int = 63);
  assert (Int64.count_set_bits (Int64.div Int64.max_int 2L) = 62);
  assert (Int64.count_set_bits (Int64.div Int64.min_int 2L) = 2);
  assert (Int64.count_set_bits 1L = 1);
  assert (Int64.count_set_bits 2L = 1);
  assert (Int64.count_set_bits 3L = 2)

let tests () =
  test_consts ();
  test_arith ();
  test_logops ();
  test_equal ();
  test_compare ();
  test_float_conv ();
  test_string_conv ();
  test_min_max ();
  test_clz ();
  test_ctz ();
  test_clrsb ();
  test_popcount ();
  ()

let () =
  tests ();
  print_endline "OK"
