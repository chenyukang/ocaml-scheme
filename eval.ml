open Type
open Env
open Printf

exception Runtime_error  of string
let error msg = raise (Runtime_error msg)

let is_debug = ref false

let rec eval exp env =
  if !is_debug then debug "eval" exp;
  match exp with
  | Nil | Int _ | Bool _  | Primitive _ -> exp
  | Symbol s -> env_lookup !env s
  | Cons(e1, e2) -> (
    match e1 with
    | Symbol "set!"  -> eval_assign exp env
    | Symbol "define" -> eval_define exp env
    | Symbol "begin" -> eval_begin exp env
    | Symbol "let" -> eval_let exp env
    | Symbol "lambda" -> eval_lambda exp env
    | _  -> (
      let app = eval e1 env in
      debug "now::" app;
      match app with
      | Lambda(_, _, save_env) ->
         eval_apply app e2 (merge_env save_env env)
      | Primitive s -> (s e2 env)
      | _ -> error "uneval-app"))
  | If(cond, exp1, exp2) -> eval_if cond exp1 exp2 env
  | Lambda(var, body, _) -> exp
  and
    eval_define exp env =
    let name = nth exp 1 in
    match name with
    | Symbol(v) -> (
      (* define a simple variable *)
      let value = eval (nth exp 2) env in
      env_set env v value
    )
    | Cons(v, left) -> (
      (* define a lambda *)
      let def = nth exp 1 in
      let def_name = (sym_string (car def)) in
      let def_vars = cdr def in
      let body = (cdr (cdr exp)) in
      let lambda = Lambda(def_vars, body, env) in
      env_set env def_name lambda
    )
    | _ -> (error "eval_define")
  and
    eval_assign exp env =
    let name = sym_string (nth exp 1) in
    try
      let _ = env_lookup !env name in
      let value = eval (nth exp 2) env in
      env_set env name value
    with
      Env_error msg -> raise (Runtime_error  msg)
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
    eval exe new_env;;

let eval_equal args env =
  let v1 = eval (car args) env and
      v2 = eval (car (cdr args)) env in
  if equal v1 v2 then Bool true else Bool false;;

let eval_add args env =
  debug "eval_add:" args;
  let rec add exp res =
    match exp with
      Nil -> Int res
    | Cons(a, l) -> add l (res + (int_value (eval a env)))
    | _ -> Int res in
  add args 0;;

let eval_sub args env =
  let rec sub exp res =
    match exp with
      Nil -> Int res
    | Cons(a, l) -> sub l (res - (int_value (eval a env)))
    | _ -> Int res in
  sub (cdr args)  (int_value (eval (car args) env));;

let eval_mul args env =
  let rec mul exp res =
    match exp with
      Nil -> Int res
    | Cons(a, l) -> mul l (res * (int_value (eval a env)))
    | _ -> Int res in
  mul (cdr args)  (int_value (eval (car args) env));;

let eval_div args env =
  let rec div exp res =
    match exp with
      Nil -> Int res
    | Cons(a, l) -> div l (res / (int_value (eval a env)))
    | _ -> Int res in
  div (cdr args)  (int_value (eval (car args) env));;

let eval_less args env =
  let v1 = eval (nth args 0) env and
      v2 = eval (nth args 1) env in
  match v1, v2 with
  | Int(_v1), Int(_v2) -> if _v1 < _v2 then Bool true else Bool false
  | _ -> Bool false;;

let eval_larg args env =
  let v1 = eval (nth args 0) env and
      v2 = eval (nth args 1) env in
  match v1, v2 with
  | Int(_v1), Int(_v2) -> if _v1 > _v2 then Bool true else Bool false
  | _ -> Bool false;;

let eval_and args env =
  let rec it exp =
    match exp with
      Cons(a, left) -> (
      match (eval a env) with
        Bool false -> Bool false
      | _ -> it left)
    | _ -> Bool true in
  it args;;

let eval_or args env =
  let rec it exp =
    match exp with
      Cons(a, left) -> (
      match (eval a env) with
      | Bool true -> Bool true
      | _ -> it left)
    | _ -> Bool false in
  it args;;

let eval_list args env =
  let rec it exp =
    match exp with
      Cons(a, left) -> Cons((eval a env), (it (cdr exp)))
    | _ -> Nil in
  it args;;

let eval_car args env =
  match (eval (car args) env) with
    Cons(a, left) -> a
  | _ -> (error "eval_car");;

let eval_cdr args env =
  match (eval (car args) env) with
    Cons(a, left) -> left
  | _ -> (error "eval_cdr");;

let eval_is_boolean arg env =
  let res = is_boolean (eval (car arg) env) in
  Bool res;;

let query_proc test_func: (expr -> (symbol list) ref -> expr) =
  let res = fun arg env ->
    let v = test_func (eval (car arg) env) in
    Bool v in
  res

let init_env() =
  let add_primitive name proc =
    ignore(Env.global_set name (Primitive proc)) in
  add_primitive "=" eval_equal;
  add_primitive "+" eval_add;
  add_primitive "-" eval_sub;
  add_primitive "*" eval_mul;
  add_primitive "/" eval_div;
  add_primitive "<" eval_less;
  add_primitive ">" eval_larg;
  add_primitive "and" eval_and;
  add_primitive "or" eval_or;
  add_primitive "list" eval_list;
  add_primitive "car" eval_car;
  add_primitive "cdr" eval_cdr;
  add_primitive "boolean?" (query_proc is_boolean);
  add_primitive "bool?" (query_proc is_boolean);
  add_primitive "integer?" (query_proc is_integer);
  add_primitive "pair?" (query_proc is_pair);;
