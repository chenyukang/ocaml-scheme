open Printf
       
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

let rec debug x =
  print x;
  print_newline()
  
                          
