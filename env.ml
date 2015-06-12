open Type
open Printf

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
    error (sprintf "%s: not found in env" name)

let global_lookup name =
  env_lookup !global_env name

let env_set env name value =
  try
    match List.find
            (function x -> match x with { name = n } -> n = name)
            !env with
      x -> x.value <- value ; x.value
  with Not_found ->
    let symbol = { name = name ; value = value }
    in env := symbol :: !env ;
       value;;

let global_set name value =
  env_set global_env name value

let rec env_debug env =
  match env with
  | x :: [] -> printf "now: %s\n" x.name
  | x :: tl -> begin
      printf "now: %s\n" x.name;
      env_debug tl;
    end
  | _ -> printf "end\n"

let extend env name value =
  debug "extend" value;
  let res = ref !env in
  ignore(env_set res name value);
  res

let env_init() =
  global_env := { name = "a"; value = Int(2) } :: !global_env
