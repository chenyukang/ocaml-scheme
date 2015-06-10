open Type
open Env
open Printf

exception Runtime_error  of string
let error msg = raise (Runtime_error msg)

let rec eval exp env =
  print_string "now eval ";
  Type.debug exp;
  match exp with
  | Nil | Int _ -> exp
  | Symbol s -> env_lookup env s
  | Bool v -> Bool v
  | Cons(e1, e2) -> (
    match e1 with
    | Symbol "set!"  -> eval_assign exp env
    | Symbol "define" -> eval_define exp env
    | Symbol "and" -> eval_and exp env
    | Symbol "or" -> eval_or exp env
    | _ -> Symbol "uneval")
  | _ -> Int 2
  and
    eval_define exp env =
    let name = Type.nth exp 1 in
    let value = eval (Type.nth exp 2) env in
    ignore(Env.global_set (Type.sym_string name) value);
    value
  and
    eval_assign exp env =
    let name = Type.sym_string (Type.nth exp 1) in
    try
      let _ = Env.global_lookup name in
      let value = eval (Type.nth exp 2) env in
      ignore(Env.global_set name value);
      value
    with
      Env.Env_error msg -> raise (Runtime_error  msg)
  and
    eval_and exp env =
    let conds = Type.cdr exp in
    let rec it exp =
      match exp with
      | Nil -> (Bool true)
      | Cons(a, Nil) -> (eval a env)
      | _ -> match (eval (car exp) env) with
             | Bool false -> Bool false
             | _ -> it (cdr exp) in
    it conds
  and
    eval_or exp env =
    let conds = Type.cdr exp in
    let rec it exp =
      match exp with
      | Nil -> (Bool false)
      | Cons(a, Nil) -> (eval a env)
      | _ -> match (eval (car exp) env) with
             | Bool true -> Bool true
             | _ -> it (cdr exp) in
    it conds
