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
  | x :: [] -> Printf.printf "now: %s\n" x.name
  | x :: tl -> begin
      Printf.printf "now: %s\n" x.name;
      env_debug tl;
    end
  | _ -> Printf.printf "end\n"

let extend env name value =
  (* Printf.printf "extend now: %s\n extend_value" name; *)
  Type.debug value;
  let res = ref !env in
  ignore(env_set res name value);
  (* env_debug !res; *)
  res

let env_init() =
  global_env := { name = "a"; value = Int(2) } :: !global_env
