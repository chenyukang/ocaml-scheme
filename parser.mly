%{ open Type %}

%token <int> INT
%token <string> SYMBOL
%token TRUE FALSE
%token DEF
%token BEGIN
%token IF
%token LAMBDA
%token LPAREN RPAREN
%token LET
%token DOT
%token EOF

%start expr
%type <Type.expr> expr

%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIV MOD

%%

expr:
     INT                             { Type.Int($1) }
  |  SYMBOL                          { Type.Symbol($1) }
  |  LPAREN IF expr expr expr RPAREN { Type.If($3, $4, $5) }
  |  LPAREN list RPAREN              { $2 }


list:
    expr DOT expr      {  Type.Cons($1, $3) }
  | expr list          {  Type.Cons($1, $2) }
  |                    {  Type.Nil }
