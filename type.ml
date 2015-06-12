open Printf
exception Type_error of string


type expr =
  Nil
  | Int of int
  | Symbol of string
  | Bool of bool
  | Cons of expr * expr
  | If of expr * expr * expr
  | Lambda of expr * expr * (symbol list) ref
   and symbol_value =
     Value of expr
   and
     symbol = { name: string; mutable value: expr };;

let car expr =
  match expr with
  | Cons(e1, e2) -> e1
  | _ -> Nil

let cdr expr =
  match expr with
  | Cons(e1, e2) -> e2
  | _ -> Nil

let lambda_vars expr =
  match expr with
  | Lambda(vars, _, _) -> vars
  | _ -> Nil

let lambda_body expr =
  match expr with
  | Lambda(_, body, _) -> body
  | _ -> Nil

let rec nth expr k =
  match expr with
    Cons(e1, e2) -> if k = 0 then e1 else nth e2 (k - 1)
  | _ -> Nil

let is_false expr =
  match expr with
  | Bool false -> true
  | _ -> false

let is_true expr =
  (is_false expr) <> true

let int_value expr =
  match expr with
  | Int v -> v
  | _ -> raise (Type_error "int_value")

let equal exp1 exp2 =
  match exp1, exp2 with
    Int(v1), Int(v2) -> v1 = v2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | _, _ -> false

let sym_string expr =
    match expr with
    | Symbol(s) -> s
    | _ -> raise (Type_error "sym_string")

let rec print x =
  match x with
    Nil -> print_string "nil"
  | Int i -> printf "int %d" i
  | Symbol s -> printf "symbol %s" s
  | Bool v -> printf "bool %s" (if v then "#t" else "#f")
  | Cons(car, cdr) -> printf "("; print car; printf " . "; print cdr; printf ")"
  | If(cond, e1, e2) -> printf "if ["; print cond; printf " ] -> "; print e1; printf " | "; print e2
  | Lambda(vars, body, env) -> printf "#proc"

let debug str x =
  Printf.printf "debug %s: " str;
  print x;
  print_newline()

let rec _show x =
  match x with
    Nil -> printf "()"
  | Symbol s -> print_string s
  | Bool v -> print_string (if v then "#t" else "#f")
  | Int i -> print_int i
  | Cons(car, cdr) -> printf "("; _show_list x; printf ")"
  | If(cond, e1, e2) -> printf "#<if>"
  | Lambda(vars, body, env) -> printf "#<proc>"
  and _show_list x =
    match x with
      Cons(car, Cons(e1, e2)) -> ( _show car; printf " ";
                                   _show_list(Cons(e1, e2)))
    | Cons(car, Nil) -> _show car
    | Cons(car, cdr) -> _show car; printf " . "; _show cdr
    | _ -> raise (Type_error "type error")

let show x =
  print_string "$: ";
  _show x;
  print_newline()
