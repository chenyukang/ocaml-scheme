open Parser
open Eval
       
let main =
  print_endline "OCaml-Scheme: A scheme iterator of OCaml";
  print_string (match Sys.os_type with
                  "Unix" | "Cygwin" -> "Ctrl-D"
                  | "Win32" -> "Ctrl-Z"
                  | _ -> "EOF") ;
  print_endline " to exit." ;
  try
    while true do
      try
        print_string "$-> ";
        let str = read_line() in        
        let e = Parser.expr Lexer.token (Lexing.from_string str) in begin 
            Printf.printf "%s\n" str;
            Type.debug e;
            Type.show e;
            Type.show (eval e []);
          end
      with
      | _ -> print_endline ("Parsing ERROR")
    done
  with
  | _ -> print_endline ("ERROR")
                       
