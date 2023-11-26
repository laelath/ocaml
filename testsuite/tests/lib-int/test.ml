(* TEST *)

let test_consts () =
  assert (Int.zero = 0);
  assert (Int.one = 1);
  assert (Int.minus_one = -1);
  ()

let test_arith () =
  assert (Int.add 2 4 = 6);
  assert (Int.sub 6 2 = 4);
  assert (Int.mul 6 2 = 12);
  assert (Int.div 12 2 = 6);
  assert (Int.rem 5 2 = 1);
  assert (Int.succ 5 = 6);
  assert (Int.pred 5 = 4);
  assert (Int.abs (-5) = 5);
  assert (Int.abs 5 = 5);
  ()

let test_logops () =
  assert (Int.logand 0xF0F0 0xFFFF = 0xF0F0);
  assert (Int.logor 0xF0FF 0x0F0F = 0xFFFF);
  assert (Int.logxor 0xF0FF 0x0F0F = 0xFFF0);
  assert (Int.lognot Int.max_int = Int.min_int);
  assert (Int.shift_left 1 4 = 16);
  assert (Int.shift_left (Int.compare 0 0) 63 = 0); (* Issue #8864 *)
  assert (Int.shift_right 16 4 = 1);
  assert (Int.shift_right (-16) 4 = (-1));
  assert (Int.shift_right (-16) 4 = (-1));
  assert (Int.shift_right_logical Int.min_int (Sys.int_size - 1) = 1);
  ()

let test_equal () =
  assert (Int.equal 1 1 = true);
  assert (Int.equal 1 0 = false);
  ()

let test_compare () =
  assert (Int.compare 3 3 = 0);
  assert (Int.compare 3 4 = (-1));
  assert (Int.compare 4 3 = 1);
  assert (Int.compare (-4) 3 = -1);
  assert (Int.compare 3 (-4) = 1);
  ()

let test_float_conv () =
  assert (Int.to_float 5 = 5.0);
  assert (Int.of_float 5. = 5);
  assert (Int.of_float 5.9 = 5);
  ()

let test_string_conv () =
  assert (Int.to_string 50 = "50");
(*  assert (Int.of_string "50" = Some 50);
  assert (Int.of_string "" = None); *)
  ()

let test_min_max () =
  assert (Int.max 2 3 = 3);
  assert (Int.min 2 3 = 2)

let test_hash () =
  let f n =
    assert (Hashtbl.hash n = Int.hash n);
    assert (Hashtbl.seeded_hash 16 n = Int.seeded_hash 16 n)
  in
  f 0; f 123; f (-456); f 0x3FFFFFFF; f (-0x40000000)

let test_clz () =
  assert (Int.count_leading_zeros 0 = Sys.int_size);
  assert (Int.count_leading_zeros (-1) = 0);
  assert (Int.count_leading_zeros Int.min_int = 0);
  assert (Int.count_leading_zeros Int.max_int = 1);
  assert (Int.count_leading_zeros (Int.max_int / 2) = 2);
  assert (Int.count_leading_zeros (Int.min_int / 2) = 0);
  assert (Int.count_leading_zeros 1 = Sys.int_size - 1);
  assert (Int.count_leading_zeros 2 = Sys.int_size - 2);
  assert (Int.count_leading_zeros 3 = Sys.int_size - 2)

let test_ctz () =
  assert (Int.count_trailing_zeros 0 = Sys.int_size);
  assert (Int.count_trailing_zeros (-1) = 0);
  assert (Int.count_trailing_zeros Int.min_int = Sys.int_size - 1);
  assert (Int.count_trailing_zeros Int.max_int = 0);
  assert (Int.count_trailing_zeros (Int.max_int / 2) = 0);
  assert (Int.count_trailing_zeros (Int.min_int / 2) = Sys.int_size - 2);
  assert (Int.count_trailing_zeros 1 = 0);
  assert (Int.count_trailing_zeros 2 = 1);
  assert (Int.count_trailing_zeros 3 = 0)

let test_clrsb () =
  assert (Int.count_leading_redundant_sign_bits 0 = Sys.int_size - 1);
  assert (Int.count_leading_redundant_sign_bits (-1) = Sys.int_size - 1);
  assert (Int.count_leading_redundant_sign_bits Int.min_int = 0);
  assert (Int.count_leading_redundant_sign_bits Int.max_int = 0);
  assert (Int.count_leading_redundant_sign_bits (Int.max_int / 2) = 1);
  assert (Int.count_leading_redundant_sign_bits (Int.min_int / 2) = 1);
  assert (Int.count_leading_redundant_sign_bits 1 = Sys.int_size - 2);
  assert (Int.count_leading_redundant_sign_bits 2 = Sys.int_size - 3);
  assert (Int.count_leading_redundant_sign_bits 3 = Sys.int_size - 3)

let test_popcount () =
  assert (Int.count_set_bits 0 = 0);
  assert (Int.count_set_bits (-1) = Sys.int_size);
  assert (Int.count_set_bits Int.min_int = 1);
  assert (Int.count_set_bits Int.max_int = Sys.int_size - 1);
  assert (Int.count_set_bits (Int.max_int / 2) = Sys.int_size - 2);
  assert (Int.count_set_bits (Int.min_int / 2) = 2);
  assert (Int.count_set_bits 1 = 1);
  assert (Int.count_set_bits 2 = 1);
  assert (Int.count_set_bits 3 = 2)

let tests () =
  test_consts ();
  test_arith ();
  test_logops ();
  test_equal ();
  test_compare ();
  test_float_conv ();
  test_string_conv ();
  test_min_max ();
  test_hash ();
  test_clz ();
  test_ctz ();
  test_clrsb ();
  test_popcount ();
  ()

let () =
  tests ();
  print_endline "OK"
