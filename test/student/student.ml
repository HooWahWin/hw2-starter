open OUnit2
open Basics

let test_sanity _ =
    assert_equal 1 1 ~msg:"Custom error message"

let test_rev_tup _ =
  assert_equal ("hello", 42) (rev_tup (42, "hello")) ~msg:"rev_tup with int and string";
  assert_equal (true, false) (rev_tup (false, true)) ~msg:"rev_tup with booleans"

let test_rev_triple _ =
  assert_equal ("c", 2, "a") (rev_triple ("a", 2, "c")) ~msg:"rev_triple with mixed types";
  assert_equal (true, 5, false) (rev_triple (false, 5, true)) ~msg:"rev_triple with bool and int"

let test_is_odd _ =
  assert_equal false (is_odd (-2)) ~msg:"is_odd with negative even";
  assert_equal true (is_odd (-1)) ~msg:"is_odd with negative odd"

let test_is_older _ =
  assert_equal false (is_older (2021, 5, 10) (2021, 5, 10)) ~msg:"is_older with identical dates";
  assert_equal true (is_older (1999, 12, 31) (2000, 1, 1)) ~msg:"is_older across year boundary"

let test_to_us_format _ =
  assert_equal (2, 14, 2020) (to_us_format (2020, 2, 14)) ~msg:"to_us_format February";
  assert_equal (11, 30, 2023) (to_us_format (2023, 11, 30)) ~msg:"to_us_format November"

let test_pow _ =
  assert_equal 1 (pow 5 0) ~msg:"pow base case: any number to power 0";
  assert_equal 1 (pow 1 10) ~msg:"pow base case: 1 to any power"

let test_fac _ =
  assert_equal 1 (fac 0) ~msg:"fac base case: 0! = 1";
  assert_equal 1 (fac 1) ~msg:"fac base case: 1! = 1"

let test_get_nth _ =
  assert_equal "hello" (get_nth (0, ["hello"; "world"])) ~msg:"get_nth with string list";
  assert_equal true (get_nth (1, [false; true; false])) ~msg:"get_nth with boolean list"

let test_larger _ =
  assert_equal [] (larger [1; 2] [3; 4]) ~msg:"larger with equal length lists";
  assert_equal [] (larger ["a"] ["b"]) ~msg:"larger with equal length string lists"

let test_sum _ =
  assert_equal (-5) (sum [-1; -2] [-2]) ~msg:"sum with negative numbers";
  assert_equal 10 (sum [10] []) ~msg:"sum with one empty list"

let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "rev_tup" >:: test_rev_tup;
    "rev_triple" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "is_older" >:: test_is_older;
    "to_us_format" >:: test_to_us_format;
    "pow" >:: test_pow;
    "fac" >:: test_fac;
    "get_nth" >:: test_get_nth;
    "larger" >:: test_larger;
    "sum" >:: test_sum
  ]

let _ = run_test_tt_main suite
