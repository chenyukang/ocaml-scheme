open Type
open Env
open Printf

exception Runtime_error  of string
let error msg = raise (Runtime_error msg)

let rec eval exp env =
  debug "eval" exp;
  match exp with
  | Nil | Int _ | Bool _  -> exp
  | Symbol s -> env_lookup !env s
  | Cons(e1, e2) -> (
    match e1 with
    | Symbol "=" -> eval_equal exp env
    | Symbol "+" -> eval_add exp env
    | Symbol "-" -> eval_sub exp env
    | Symbol "*" -> eval_mul exp env
    | Symbol "<" -> eval_less exp env
    | Symbol ">" -> eval_larg exp env
    | Symbol "set!"  -> eval_assign exp env
    | Symbol "define" -> eval_define exp env
    | Symbol "and" -> eval_and exp env
    | Symbol "or" -> eval_or exp env
    | Symbol "begin" -> eval_begin exp env
    | Symbol "let" -> eval_let exp env
    | Symbol "lambda" -> eval_lambda exp env
    | _  -> (
      let e = eval e1 env in
      match e with
      | Lambda(_, _, save_env) -> eval_apply e e2 (merge_env save_env env)
      | _ -> Symbol "uneval-app"))
  | If(cond, exp1, exp2) -> eval_if cond exp1 exp2 env
  | Lambda(var, body, _) -> exp
  and
    eval_equal exp env =
    let args = cdr exp in
    let v1 = eval (car args) env and
        v2 = eval (car (cdr args)) env in
    if equal v1 v2 then Bool true else Bool false
  and
    eval_add exp env =
    let args = cdr exp in
    let rec add exp res =
      match exp with
        Nil -> Int res
      | Cons(a, l) -> add l (res + (int_value (eval a env)))
      | _ -> Int res in
    add args 0
  and
    eval_sub exp env =
    let args = cdr exp in
    let rec sub exp res =
        match exp with
          Nil -> Int res
        | Cons(a, l) -> sub l (res - (int_value (eval a env)))
        | _ -> Int res in
    sub (cdr args)  (int_value (eval (car args) env))
  and
    eval_mul exp env =
    let args = cdr exp in
    let rec mul exp res =
      match exp with
        Nil -> Int res
      | Cons(a, l) -> mul l (res * (int_value (eval a env)))
      | _ -> Int res in
    mul (cdr args)  (int_value (eval (car args) env))
  and
    eval_less exp env =
    let args = cdr exp in
    let v1 = eval (nth args 0) env in
    let v2 = eval (nth args 1) env in
    match v1, v2 with
    | Int(_v1), Int(_v2) -> if _v1 < _v2 then Bool true else Bool false
    | _ -> Bool false
  and
    eval_larg exp env =
    let args = cdr exp in
    let v1 = eval (nth args 0) env in
    let v2 = eval (nth args 1) env in
    match v1, v2 with
    | Int(_v1), Int(_v2) -> if _v1 > _v2 then Bool true else Bool false
    | _ -> Bool false
  and
    eval_define exp env =
    let name = nth exp 1 in
    debug "eval_define: " name;
    match name with
    (* define a simple variable *)
    | Symbol v -> (
      let value = eval (nth exp 2) env in
      ignore(env_set env v value);
      value
    )
    (* define a lambda *)
    | Cons(v, left) -> (
      let def = nth exp 1 in
      let def_name = (sym_string (car def)) in
      let def_vars = cdr def in
      let body = (cdr (cdr exp)) in
      let lambda = Lambda(def_vars, body, env) in
      ignore(env_set env def_name lambda);
      lambda;
    )
    | _ -> (error "eval_define")
  and
    eval_assign exp env =
    let name = sym_string (nth exp 1) in
    try
      let _ = env_lookup !env name in
      let value = eval (nth exp 2) env in
      ignore(env_set env name value);
      value
    with
      Env_error msg -> raise (Runtime_error  msg)
  and
    eval_and exp env =
    let conds = cdr exp in
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
    let conds = cdr exp in
    let rec it exp =
      match exp with
      | Nil -> (Bool false)
      | Cons(a, Nil) -> (eval a env)
      | _ -> match (eval (car exp) env) with
             | Bool true -> Bool true
             | _ -> it (cdr exp) in
    it conds
  and
    eval_begin exp env =
    let exps = cdr exp in
    let rec it exp =
      match exp with
      | Cons(a, Nil) -> (eval a env)
      | Cons(a, l) -> begin
          ignore (eval a env);
          (it l) end
      | _ -> (error "eval_begin") in
    it exps
  and
    eval_if cond exp1 exp2 env =
    match eval cond env  with
      Bool true -> eval exp1 env
    | _ -> eval exp2 env
  and
    eval_let exp env =
    let bindings = nth exp 1 and
        body = nth exp 2 in
    let bind_var exp =
      sym_string (car exp) in
    let bind_val exp =
      eval (car (cdr exp)) env in
    let rec extend_it env exp =
      match exp with
        Cons(a, left) -> (
        let new_env = extend env (bind_var a) (bind_val a) in
        if left = Nil then new_env
        else extend_it new_env left)
      | _ -> (error "eval_let") in
    let new_env = extend_it env bindings in
    eval body new_env
  and
    eval_lambda exp env =
    let vars = nth exp 1 and
        body = (cdr (cdr exp)) in
    Lambda(vars, body, env)
  and
    eval_apply lambda args env =
    let vars = lambda_vars lambda and
        body = lambda_body lambda in
    let rec extend_it env vars args =
      match vars with
        Nil -> env
      | Cons(Symbol(a), left) -> (
        let new_env =  extend env a (eval (car args) env) in
        if left = Nil then new_env
        else extend_it new_env left (cdr args))
      | _ -> (error "eval_apply") in
    let new_env = extend_it env vars args in
    let exe = Cons(Symbol("begin"), body) in
    eval exe new_env;
