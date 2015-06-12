{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
         | [' ' '\t' '\r' '\n'] { token lexbuf }
         | ['0'-'9']+           { INT (int_of_string(Lexing.lexeme lexbuf)) }
         | "#t"               { TRUE }
         | "#f"               { FALSE }
         | "if"               { IF }
         | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.' '\''][^' ' '\t' '\n' '(' ')']* { SYMBOL(Lexing.lexeme lexbuf) }
         | '('                { LPAREN }
         | ')'                { RPAREN }
         | '.'                { DOT }
         | var                { SYMBOL (Lexing.lexeme lexbuf) }
         | eof                { EOF }
{}
