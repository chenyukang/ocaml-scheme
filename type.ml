open Printf
exception TypeError
            
type expr =
  Nil
  | Int of int
  | Symbol of symbol
  | Cons of expr * expr  
  | If of expr * expr * expr
   and symbol_value =
     Value of expr
   and
     symbol = { name: string; mutable value: symbol_value };;

let rec print x =
  match x with
    Nil -> print_string "nil ()"
  | Int i -> printf "int %d" i
  | Symbol { name = n } -> printf "symbol %s" n
  | Cons(car, cdr) -> printf "("; print car; printf " . "; print cdr; printf ")"
  | If(cond, e1, e2) -> printf "if ["; print cond; printf " ] -> "; print e1; printf " | "; print e2

let debug x =
  print_string "debug";
  print x;
  print_newline()
               
let rec _show x =
  match x with
    Nil -> printf "()"
  | Symbol {name = n} -> print_string n
  | Int i -> print_int i
  | Cons(car, cdr) -> printf "("; _show_list x; printf ")"
  | If(cond, e1, e2) -> printf "#<if>"
  and _show_list x =
    match x with
      Cons(car, Cons(e1, e2)) -> _show car; printf " "; _show_list(Cons(e1, e2))
    | Cons(car, Nil) -> _show car
    | Cons(car, cdr) -> _show car; printf " . "; _show cdr
    | _ -> raise TypeError
                                                   

let show x =
  _show x;
  print_newline()
               
