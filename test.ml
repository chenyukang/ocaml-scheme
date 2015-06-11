open Type
open Env
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


let test_unit = [
    "Type" , `Quick, test_type;
    "Env", `Slow , test_env ;
    "Env-extend", `Slow, test_env_extend;
  ]

(* Run it *)
let () =
  Alcotest.run "My test" [
                 "test_unit", test_unit;
               ]
