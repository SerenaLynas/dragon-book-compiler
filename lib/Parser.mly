(* This corresponds to both the Return Value and yyval *)
  (* The variant is the Return Value and yyval is in the associated data *)
  (* OCaml has no concept of NULL, this is how you would do it. *)   
%token IF "if"
%nonassoc IF
%token THEN "then"
%token ELSE "else"
%nonassoc ELSE
%token WHILE "while"
%token DO "do"
%token BREAK "break"
%token <ParserTypes.basic> BASIC
%token <string> ID
%token <int> NUM
%token <float> REAL
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

%token <ParserTypes.error> ERR

%type <unit> program
%start program

%%

program:
  | block { print_endline "program -> block" }

block:
  | "{"; decl*; stmt*; "}" { print_endline "block -> { decl* stmt* }"}
  | error {
    print_endline "block -> ERROR";
    Params.errcount := !Params.errcount + 1;
    Printf.printf "\nError from %d:%d to %d:%d: Not a block or statement\n\n" $startpos.pos_lnum $startpos.pos_bol $endpos.pos_lnum $endpos.pos_bol
  }

decl:
  | ty; ID; ";" { print_endline "decl -> type id;"}

ty:
  | ty; "["; NUM; "]" { print_endline "type -> type [num]" }
  | BASIC             { print_endline "type -> basic" }

stmt:
  | loc; "="; bool; ";"                       { print_endline "stmt -> loc = bool ;" }
  | "if"; "("; bool; ")"; stmt %prec IF       { print_endline "stmt -> if ( bool ) stmt_if" }
  | "if"; "("; bool; ")"; stmt; "else"; stmt  { print_endline "stmt -> if ( bool ) stmt else stmt"}
  | "while"; "("; bool; ")"; stmt             { print_endline "stmt -> while ( bool ) stmt" }
  | "do"; stmt; "while"; "("; bool; ")"; ";"  { print_endline "stmt -> do stmt while ( bool ) ;" }
  | "break"; ";"                              { print_endline "stmt -> break ;" }
  | block                                     { print_endline "stmt -> block" }
  | error {
    print_endline "stmt -> ERROR";
    Params.errcount := !Params.errcount + 1;
    Printf.printf "\nError from %d:%d to %d:%d: Not a block or statement\n\n" $startpos.pos_lnum $startpos.pos_bol $endpos.pos_lnum $endpos.pos_bol
  }

(*stmt_if:
  | stmt; "else"; stmt { print_endline "stmt_if -> stmt else stmt" }
  | stmt               { print_endline "stmt_if -> stmt" }*)

loc:
  | loc; "["; bool; "]" { print_endline "loc -> loc [ bool ]" }
  | ID                  { print_endline "loc -> id" }

bool:
  | bool; "||"; join  { print_endline "bool -> bool || join" }
  | join              { print_endline "bool -> join" }

join:
  | join; "&&"; equality  { print_endline "join -> join && equality" }
  | equality              { print_endline "join -> equality" }

equality:
  | equality; "=="; rel { print_endline "equality -> equality == rel" }
  | equality; "!="; rel { print_endline "equality -> equality != rel" }
  | rel                 { print_endline "equality -> rel" }

rel:
  | expr; "<"; expr   { print_endline "rel -> expr < expr" }
  | expr; "<="; expr  { print_endline "rel -> expr <= expr" }
  | expr; ">="; expr  { print_endline "rel -> expr >= expr" }
  | expr; ">"; expr   { print_endline "rel -> expr > expr" }
  | expr              { print_endline "rel -> expr" }

expr:
  | expr; "+"; term { print_endline "expr -> expr + term" }
  | expr; "-"; term { print_endline "expr -> expr - term" }
  | term            { print_endline "expr -> term" }

term:
  | term; "*"; unary  { print_endline "term -> term * unary" }
  | term; "/"; unary  { print_endline "term -> term / unary" }
  | unary             { print_endline "term -> unary" }

unary:
  | "!"; unary  { print_endline "unary -> ! unary" }
  | "-"; unary  { print_endline "unary -> - unary" }
  | factor      { print_endline "unary -> factor" }

factor:
  | "("; bool; ")"  { print_endline "factor -> ( bool )" }
  | loc             { print_endline "factor -> loc" }
  | NUM             { print_endline "factor -> num" }
  | REAL            { print_endline "factor -> real" }
  | "true"          { print_endline "factor -> true" }
  | "false"         { print_endline "factor -> false" }
  | "("; ")" {
    (* Required to prevent infinite loop *)
    Params.errcount := !Params.errcount + 1;
    print_endline "factor -> ERROR\n\nError: Parens are empty\n\n";
  }
  | error {
    print_endline "factor -> ERROR";
    Params.errcount := !Params.errcount + 1;
    Printf.printf "\nError from %d:%d to %d:%d: Not a value\n\n" $startpos.pos_lnum $startpos.pos_bol $endpos.pos_lnum $endpos.pos_bol
  }