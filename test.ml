open Type
open Env
open Eval
open Printf

(* The tests *)
let test_type () =
  OUnit.assert_equal true (is_false (Type.Bool false))

let env_dont_have env v =
  try
    ignore(env_lookup !env v);
    false
  with
    Env_error msg -> true
  | _ -> false

let test_env () =
  let env = ref !global_env in
  let res = (env_set env "hello" (Int 1)) in
  OUnit.assert_equal 1 (int_value res);
  OUnit.assert_equal 1 (int_value (env_lookup !env "hello"));
  OUnit.assert_equal true (env_dont_have env "error");
  OUnit.assert_equal false (env_dont_have env "hello")

let test_env_extend() =
  let env = ref !global_env in
  let res = extend env "hello" (Int 1) in
  OUnit.assert_equal 1 (int_value (env_lookup !res "hello"));
  OUnit.assert_equal false (env_dont_have res "hello")

let run str =
  let e = Parser.expr Lexer.token (Lexing.from_string str) in
  eval e Env.global_env

let test_eval() =
  let r1 = run "(+ 1 2)" and
      r2 = run "(+ 1 2 3)" and
      r3 = run "(< 1 2)" and
      r4 = run "(let ((x 1) (y 2) (z 3)) (+ x y z))" in
  OUnit.assert_equal 3 (int_value r1);
  OUnit.assert_equal 6 (int_value r2);
  OUnit.assert_equal true (is_true r3);
  OUnit.assert_equal 6 (int_value r4)



let test_unit = [
    "Type" , `Quick, test_type;
    "Env", `Quick , test_env ;
    "Env-extend", `Quick, test_env_extend;
    "Eval", `Quick, test_eval;
  ]

(* Run it *)
let () =
  Alcotest.run "My test" [
                 "test_unit", test_unit;
               ]
