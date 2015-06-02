open Type
open Env
       
exception Runtime

let rec eval exp env =
  match exp with
    Nil | Int _ -> exp
    | Symbol {name = n} -> match (env_lookup env n).value with
                             Value v -> v
                           | _ -> raise Runtime
    | _ -> Int 2
               
