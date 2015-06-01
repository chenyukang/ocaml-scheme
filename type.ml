
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
