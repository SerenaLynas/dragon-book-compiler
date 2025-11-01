(* This corresponds to both the Return Value and yyval *)
  (* The variant is the Return Value and yyval is in the associated data *)
  (* OCaml has no concept of NULL, this is how you would do it. *)   
%token IF "if"
%nonassoc IF
%token ELSE "else"
%nonassoc ELSE
%token WHILE "while"
%token DO "do"
%token BREAK "break"
%token <Ast.basic> BASIC
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

%token <Compiler.error> ERR

%type <Ast.stmt> program
%start program

%%

program:
  | blk = block {
      print_endline "program -> block";
      blk
    }
  | error {
    print_endline "program -> ERROR";
    Compiler.errcount := !Compiler.errcount + 1;
    Printf.printf "\nFailed to parse program";
    Ast.BLOCK ([], [])
  }

block:
  | "{"; decls = decl*; stmts = stmt*; "}" {
    Ast.BLOCK (decls, stmts)
  }
  | "{"; error {
    print_endline "block -> ERROR";
    Compiler.errcount := !Compiler.errcount + 1;
    Printf.printf "\nError from %d:%d to %d:%d: Not a block or statement\n\n" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol) $endpos.pos_lnum ($endpos.pos_cnum - $endpos.pos_bol);
    Ast.BLOCK ([], [])
  }

decl:
  | ty = ty; id = ID; ";" {
    print_endline "decl -> type id;";
    Ast.DECL (id, ty)
  }

ty:
  | ty = ty; "["; num = NUM; "]" {
    print_endline "type -> type [num]";
    {
      basic = ty.basic;
      dim = ty.dim @ [num]
    } : Ast.ty
  }
  | basic = BASIC {
    print_endline "type -> basic";
    {
      basic = basic;
      dim = []
    } : Ast.ty
  }

stmt:
  | dst = loc; "="; src = bool; ";" {
      print_endline "stmt -> loc = bool ;";
      Ast.ASSIGN (dst, src)
    }
  | "if"; "("; cond = bool; ")"; dst = stmt %prec IF {
      print_endline "stmt -> if ( bool ) stmt_if";
      Ast.IF (cond, dst, None)
    }
  | "if"; "("; cond = bool; ")"; then_stmt = stmt; "else"; else_stmt = stmt {
      print_endline "stmt -> if ( bool ) stmt else stmt";
      Ast.IF (cond, then_stmt, Some else_stmt)
    }
  | "while"; "("; cond = bool; ")"; stmt = stmt {
      print_endline "stmt -> while ( bool ) stmt";
      Ast.WHILE (cond, stmt)
    }
  | "do"; stmt = stmt; "while"; "("; cond = bool; ")"; ";" {
      print_endline "stmt -> do stmt while ( bool ) ;";
      Ast.WHILE (cond, stmt)
    }
  | "break"; ";" {
      print_endline "stmt -> break ;";
      Ast.BREAK
    }
  | blk = block {
      print_endline "stmt -> block";
      blk
    }

(*stmt_if:
  | stmt; "else"; stmt { print_endline "stmt_if -> stmt else stmt" }
  | stmt               { print_endline "stmt_if -> stmt" }*)

loc:
  | loc = loc; "["; idx = bool; "]" {
      print_endline "loc -> loc [ bool ]";
      {
        id = loc.id;
        idx = loc.idx @ [idx];
      } : Ast.loc
    }
  | id = ID {
      print_endline "loc -> id";
      {
        id = id;
        idx = [];
      } : Ast.loc
    }

bool:
  | a = bool; "||"; b = join  {
      print_endline "bool -> bool || join";
      Ast.OR (a, b)
    }
  | a = join {
      print_endline "bool -> join";
      a
    }

join:
  | a = join; "&&"; b = equality {
      print_endline "join -> join && equality";
      Ast.AND (a, b)
    }
  | a = equality {
      print_endline "join -> equality";
      a
    }

equality:
  | a = equality; "=="; b = rel {
      print_endline "equality -> equality == rel";
      Ast.EQ (a, b)
    }
  | a = equality; "!="; b = rel {
      print_endline "equality -> equality != rel";
      Ast.NOT (Ast.EQ (a, b))
    }
  | rel = rel {
      print_endline "equality -> rel";
      rel
    }

rel:
  | a = expr; "<"; b = expr   { 
      print_endline "rel -> expr < expr";
      Ast.LT (a, b)
    }
  | a = expr; "<="; b = expr  { 
      print_endline "rel -> expr <= expr";
      Ast.LTEQ (a, b)
    }
  | a = expr; ">="; b = expr  { 
      print_endline "rel -> expr >= expr";
      Ast.GTEQ (a, b)
    }
  | a = expr; ">"; b = expr   { 
      print_endline "rel -> expr > expr";
      Ast.GT (a, b)
    }
  | expr = expr {
      print_endline "rel -> expr";
      expr
    }

expr:
  | a = expr; "+"; b = term {
      print_endline "expr -> expr + term";
      Ast.ADD (a, b)
    }
  | a = expr; "-"; b = term {
      print_endline "expr -> expr - term";
      Ast.SUB (a, b)
    }
  | expr = term {
    print_endline "expr -> term";
    expr
  }

term:
  | a = term; "*"; b = unary  {
      print_endline "term -> term * unary";
      Ast.MUL (a, b)
    }
  | a = term; "/"; b = unary  {
      print_endline "term -> term / unary";
      Ast.DIV (a, b)
    }
  | unary = unary {
      print_endline "term -> unary";
      unary
    }

unary:
  | "!"; expr = unary  {
      print_endline "unary -> ! unary";
      Ast.NOT expr
    }
  | "-"; expr = unary {
      print_endline "unary -> - unary";
      Ast.MINUS expr
    }
  | expr = factor {
      print_endline "unary -> factor";
      expr
    }

factor:
  | "("; expr = bool; ")"  {
      print_endline "factor -> ( bool )";
      expr
    }
  | loc = loc {
      print_endline "factor -> loc";
      Ast.LOC loc
    }
  | int = NUM {
      print_endline "factor -> num";
      Ast.LIT_INT int
    }
  | float = REAL {
      print_endline "factor -> real";
      Ast.LIT_FLOAT float
    }
  | "true"  {
      print_endline "factor -> true";
      Ast.LIT_BOOL true
    }
  | "false" {
      print_endline "factor -> false";
      Ast.LIT_BOOL false
    }