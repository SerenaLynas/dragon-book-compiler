%{
    type basic =
        | INT
        | FLOAT
        | BOOLEAN

    open Printf
%}

(* This corresponds to both the Return Value and yyval *)
  (* The variant is the Return Value and yyval is in the associated data *)
  (* OCaml has no concept of NULL, this is how you would do it. *)   
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token WHILE "while"
%token DO "do"
%token BREAK "break"
%token <string> OP
%token <Parser.basic> BASIC
%token <string> ID
%token <int> NUM
%token <float> REAL
%token <bool> BOOLEAN
%token EOF

%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token LPAREN "("
%token RPAREN ")"
%token SEMICOLON ";"

%token ASSIGN "="

%token BANG "!"
%token EQ "=="
%token NEQ "!="
%token LT "<"
%token LTEQ "<="
%token GTEQ ">="
%token GT ">"
%token AND "&&"
%token OR "||"

%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"

%token TRUE "true"
%token FALSE "false"

%type <unit> program
%start program

%%

program:
  | block { printf "program -> block" }

block:
  | "{"; d = decl*; s = stmt*; "}" { printf "block -> { decls stmts }"}

decl:
  | ty; ID; ";" { printf "decl -> type id;"}

ty:
  | ty; "["; NUM; "]" { printf "type -> type [num]" }
  | BASIC             { printf "type -> basic" }

stmt:
  | loc; "="; bool                            { printf "stmt -> loc = bool ;" }
  | "if"; "("; bool; ")"; stmt                { printf "stmt -> if ( bool ) stmt" }
  | "if"; "("; bool; ")"; stmt; "else"; stmt  { printf "stmt -> if ( bool ) stmt else stmt"}
  | "while"; "("; bool; ")"; stmt             { printf "stmt -> while ( bool ) stmt" }
  | "do"; stmt; "while"; "("; bool; ")"; ";"  { printf "stmt -> do stmt while ( bool ) ;" }
  | "break"; ";"                              { printf "stmt -> break ;" }
  | block                                     { printf "stmt -> block" }

loc:
  | loc; "["; bool; "]" { printf "loc -> loc [ bool ]" }
  | ID                  { printf "loc -> id" }

bool:
  | bool; "||"; join  { printf "bool -> bool || join" }
  | join              { printf "bool -> join" }

join:
  | join; "&&"; equality  { printf "join -> join && equality" }
  | equality              { printf "join -> equality" }

equality:
  | equality; "=="; rel { printf "equality -> equality == rel" }
  | equality; "!="; rel { printf "equality -> equality != rel" }
  | rel                 { printf "equality -> rel" }

rel:
  | expr; "<"; expr   { printf "rel -> expr < expr" }
  | expr; "<="; expr  { printf "rel -> expr <= expr" }
  | expr; ">="; expr  { printf "rel -> expr >= expr" }
  | expr; ">"; expr   { printf "rel -> expr > expr" }
  | expr              { printf "rel -> expr" }

expr:
  | expr; "+"; term { printf "expr -> expr + term" }
  | expr; "-"; term { printf "expr -> expr - term" }
  | term            { printf "expr -> term" }

term:
  | term; "*"; unary  { printf "term -> term * unary" }
  | term; "/"; unary  { printf "term -> term / unary" }
  | unary             { printf "term -> term / unary" }

unary:
  | "!"; unary  { printf "unary -> ! unary" }
  | "-"; unary  { printf "unary -> - unary" }
  | factor      { printf "unary -> factor" }

factor:
  | "("; bool; ")"  { printf "factor -> ( bool )" }
  | loc             { printf "factor -> loc" }
  | NUM             { printf "factor -> num" }
  | REAL            { printf "factor -> real" }
  | "true"          { printf "factor -> true" }
  | "false"         { printf "factor -> false" }