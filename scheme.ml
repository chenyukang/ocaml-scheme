open Parser
open Eval

let main =
  print_endline "OCaml-Scheme: A scheme iterator of OCaml";
  print_string (match Sys.os_type with
                  "Unix" | "Cygwin" -> "Ctrl-D"
                  | "Win32" -> "Ctrl-Z"
                  | _ -> "EOF") ;
  print_endline " to exit." ;
  Env.env_init();
  try
    while true do
      try
        print_string "$-> ";
        let str = read_line() in
        let e = Parser.expr Lexer.token (Lexing.from_string str) in begin
            (* Type.debug e; *)
            Type.show (eval e Env.global_env);
          end
      with
      | Eval.Runtime_error msg -> print_endline msg
      | Type.Type_error msg -> print_endline msg
      | Env.Env_error msg -> print_endline msg
      | Parsing.Parse_error -> print_endline "Syntax error."
      | _ -> print_endline ("Parsing ERROR")
    done;
  with
  | _ -> print_endline ("ERROR")
