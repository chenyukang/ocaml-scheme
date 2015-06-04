open Printf
exception Type_error of string

type expr =
  Nil
  | Int of int
  | Symbol of string
  | Cons of expr * expr
  | If of expr * expr * expr
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

let rec nth expr k =
  match expr with
    Cons(e1, e2) -> if k = 0 then e1 else nth e2 (k - 1)
  | _ -> Nil

let sym_string expr =
    match expr with
    | Symbol(s) -> s
    | _ -> raise (Type_error "sym_string")

let rec print x =
  match x with
    Nil -> print_string "nil ()"
  | Int i -> printf "int %d" i
  | Symbol s -> printf "symbol %s" s
  | Cons(car, cdr) -> printf "("; print car; printf " . "; print cdr; printf ")"
  | If(cond, e1, e2) -> printf "if ["; print cond; printf " ] -> "; print e1; printf " | "; print e2

let debug x =
  print_string "debug: ";
  print x;
  print_newline()

let rec _show x =
  match x with
    Nil -> printf "()"
  | Symbol s -> print_string s
  | Int i -> print_int i
  | Cons(car, cdr) -> printf "("; _show_list x; printf ")"
  | If(cond, e1, e2) -> printf "#<if>"
  and _show_list x =
    match x with
      Cons(car, Cons(e1, e2)) -> ( _show car; printf " ";
                                   _show_list(Cons(e1, e2)))
    | Cons(car, Nil) -> _show car
    | Cons(car, cdr) -> _show car; printf " . "; _show cdr
    | _ -> raise (Type_error "type error")


let show x =
  print_string "now: ";
  _show x;
  print_newline()
