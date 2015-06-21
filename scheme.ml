open Parser
open Eval

let in_debug = ref false
let in_iteractive = ref false

let eval_expr str env =
  try
    let e = Parser.expr Lexer.token (Lexing.from_string str) in
    Type.show (eval e env)
  with
  | Eval.Runtime_error msg -> print_endline msg
  | Type.Type_error msg -> print_endline msg
  | Env.Env_error msg -> print_endline msg
  | Parsing.Parse_error -> print_endline "Syntax error."

let run_with input =
  Env.env_init();
  Eval.init_primitive();
  try
    while true do
      output_string stdout "$-> ";
      flush stdout;
      Eval.is_debug := !in_debug;
      let str = input_line input in
      eval_expr str Env.global_env;
    done;
  with
  | End_of_file -> ( print_endline ("end"); close_in input; )

let iteractive() =
  print_endline "OCaml-Scheme: A scheme interpreter of OCaml";
  print_string (match Sys.os_type with
                  "Unix" | "Cygwin" -> "Ctrl-D"
                  | "Win32" -> "Ctrl-Z"
                  | _ -> "EOF") ;
  print_endline " to exit.";
  run_with stdin;;

let load_file file =
  let input = open_in file in
  let str = ref "" in
  try
    while true do
      str := !str ^ "\n" ^ (input_line input);
    done
  with
    End_of_file -> ( (* print_string !str; *)
    eval_expr !str Env.global_env;
  )

let main =
  begin
    let speclist = [("-d", Arg.Set in_debug, "Print debug information during executation");
                    ("-i", Arg.Set in_iteractive, "Run in iteractive mode");] in
    let usage_msg = "Ocaml-Scheme: A scheme interpreter of Ocaml" in
    Arg.parse speclist print_endline usage_msg;
    if !in_iteractive || Array.length Sys.argv - 1 = 0  then
      iteractive()
    else begin
        load_file Sys.argv.(1);
      end
  end
