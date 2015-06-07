open Type

exception Env_error of string
let error msg = raise (Env_error msg)

let global_env = ref [];;

let env_lookup env name =
  try
    let r = List.find
              (function x -> match x with { name = n } -> n = name)
              env in
    match r with
      { name = _; value = v } -> v
  with Not_found ->
    error (Printf.sprintf "%s: not found in env" name)


let global_lookup name =
  env_lookup !global_env name

let global_set name value =
  try
    match List.find
            (function x -> match x with { name = n } -> n = name)
            !global_env with
      x -> x.value <- value ; x.value
  with Not_found ->
    let symbol = { name = name ; value = value }
    in global_env := symbol :: !global_env ;
       value;;

let env_init() =
  global_env := { name = "a"; value = Int(2) } :: !global_env

(* let find name = *)
(*   try *)
(*     global_lookup name *)
(*   with Not_found -> *)
(*     global_set name Nil;; *)
