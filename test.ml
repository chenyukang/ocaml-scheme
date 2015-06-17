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

let run str trans expect =
  let e = Parser.expr Lexer.token (Lexing.from_string str) in
  (OUnit.assert_equal expect (trans (eval e Env.global_env)))

let test_eval() =
  env_clear global_env;
  (run "(+ 1 2)" int_value 3);
  (run "(+ 1 2 3)" int_value 6);
  (run "(let ((x 1) (y 2) (z 3)) (+ x y z))" int_value 6);
  (run "(let ((x 1) (y 2) (z (+ 1 3))) (+ x y z))" int_value 7);
  (run "(- (+ 3 (* 8 5)) 1)" int_value 42);
  (run "(< 1 2)" is_true true);
  (run "(or #t #t #t)" is_true true);
  (run "(or)" is_true false);
  (run "(and #t #t #f)" is_true false);
  (run "(and)" is_true true);
  (run "(and #t #t #t)" is_true true);;

let test_define() =
  env_clear global_env;
  (run "(define a 1)" int_value 1);
  (run "a" int_value 1);;

let test_func() =
  env_clear global_env;
  (run "(define a (lambda(a b) (< a b)))" is_proc true);
  (run "(a 1 2)" is_true true);
  (run "(a 2 1)" is_true false);
  (run "(define (fact x) (if (= x 0) 1  (* x (fact (- x 1)))))" is_proc true);
  (run "(fact 5)" int_value 120);;

let test_lambda() =
  env_clear global_env;
  (run "((lambda(a b) (+ a b)) 1 2)" int_value 3);
  (run "((lambda(x) (+ x 1)) 1)" int_value 2);
  (run "(let ((fu (lambda (x) (+ x 1)))) (fu 1))" int_value 2);
  (run "((lambda (x y ) (if (= y 0) 1 (* y (x x (- y 1)))))
        (lambda (x y ) (if ( = y 0) 1 (* y (x x (- y 1))))) 5)" int_value (5 * 4 * 3 * 2));;

let test_lambda_scope() =
  (run "(define foo (let ((x 4)) (lambda (y) (+ x y))))" is_proc true);
  (run "(foo 6)" int_value 10);;

let test_unit = [
    "Type" , `Quick, test_type;
    "Env", `Quick , test_env ;
    "Env-extend", `Quick, test_env_extend;
    "Def", `Quick, test_define;
    "Eval", `Quick, test_eval;
    "Eval-func", `Quick, test_func;
    "Eval-lambda", `Quick, test_lambda;
    "Eval-lambda_scope", `Quick, test_lambda_scope;
  ]

(* Run it *)
let () =
  Alcotest.run "My test" [
                 "test_unit", test_unit;
               ]
