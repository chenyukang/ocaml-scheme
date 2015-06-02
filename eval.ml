open Type
open Env
open Printf

exception Runtime_error  of string
let error msg = raise (Runtime_error msg)

let rec eval exp env =
  match exp with
  | Nil | Int _ -> exp
  | Symbol s -> env_lookup env s
  | _ -> Int 2
