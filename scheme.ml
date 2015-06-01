open Parser

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
        let l = Lexing.from_string str in
        let r = Lexing.lexeme l in begin
            Printf.printf "%s\n" str;
            Printf.printf "%s\n" r
          end
      with
      | _ -> print_endline ("ERROR")
    done
  with
  | _ -> print_endline ("ERROR")
