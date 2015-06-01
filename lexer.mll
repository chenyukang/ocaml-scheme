{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
         | [' ' '\t' '\r' '\n'] { token lexbuf }
         | ['0'-'9']+           { INT (int_of_string(Lexing.lexeme lexbuf)) }
         | "#t"               { TRUE }
         | "#f"               { FALSE }
         | "define"           { DEF }
         | "if"               { IF }
         | "begin"            { BEGIN }
         | "lambda"           { LAMBDA }
         | '<'                { LESS }
         | '>'                { LARGER }
         | '('                { LPAREN }
         | ')'                { RPAREN }
         | '+'                { PLUS }
         | '-'                { MINUS }
         | '*'                { TIMES }
         | '/'                { DIV }
         | '.'                { DOT }
         | var                { VAR (Lexing.lexeme lexbuf) }
         | eof                { EOF }
{}
