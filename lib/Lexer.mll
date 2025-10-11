{
  open Parser
}

let ws = [' ' '\t' '\r']
let letter = ['A' - 'Z' 'a' - 'z']
let digit = ['0' - '9']

let id = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9']*
let num = digit+
let real = digit+ ('.' digit)? ('E' ['+' '-']? digit+)?

rule program = parse
  | '\n'
    { Lexing.new_line lexbuf; program lexbuf }
  | ws
    {
      program lexbuf
    }
  | "if"    { IF }
  | "else"  { ELSE }
  | "while" { WHILE }
  | "do"    { DO }
  | "break" { BREAK }

  | "true"  { TRUE }
  | "false" { FALSE }

  | "int" { BASIC INT }
  | "float" { BASIC FLOAT }
  | "boolean" { BASIC BOOLEAN }

  | id as word
    { ID word }
  | num as word
    { NUM (int_of_string word) }
  | real as word
    { REAL (Float.of_string word) }

  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";" { SEMICOLON }

  | "=" { ASSIGN }
  
  | "!"  { BANG }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<"  { LT }
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | ">"  { GT }
  | "&&" { AND }
  | "||" { OR }

  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }

  | _ as c { ERR (UNKNOWN_CHAR c) }
  | eof
    { EOF }

{
  let lex lexbuf = program lexbuf;;

  let rec lex_all lexbuf fn =
    let tok = program lexbuf in
      if tok != EOF then (
        fn tok;
        lex_all lexbuf fn;
      )
}