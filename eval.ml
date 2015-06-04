open Type
open Env
open Printf

exception Runtime_error  of string
let error msg = raise (Runtime_error msg)



let rec eval exp env =
  match exp with
  | Nil | Int _ -> exp
  | Symbol s -> env_lookup env s
  | Cons(e1, e2) -> (
    match e1 with
    | Symbol "set"  -> eval_assign exp env
    | _ -> Int 3)
  | _ -> Int 2
  and
    eval_assign exp env =
    let name = Type.nth exp 1 in
    let value = eval (Type.nth exp 2) env in
    Env.global_set (Type.sym_string name) value;
    value
