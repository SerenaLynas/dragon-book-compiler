{
  open Parser
}

let delim = [' ' '\t' '\n' '\r']
let ws = delim+
let letter = ['A' - 'Z' 'a' - 'z']
let digit = ['0' - '9']

let id = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9']*
let num = digit+
let real = digit+ ('.' digit)? ('E' ['+' '-']? digit+)?

rule program = parse
  | ws
    { program lexbuf }
  | "int" { BASIC INT }
  | "float" { BASIC FLOAT }
  | "boolean" { BASIC BOOLEAN }
  | "if"    { IF }
  | "else"  { ELSE }
  | "while" { WHILE }
  | "do"    { DO }
  | "break" { BREAK }
  | "true"  { TRUE }
  | "false" { FALSE }
  | id as word
    { ID word }
  | num as word
    { NUM (int_of_string word) }
  | real as word
    { REAL (Float.of_string word) }
  | "&&" { AND }
  | "||" { OR }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<"  { LT }
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | ">"  { GT }
  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }
  | "!"  { BANG }
  | _ as c
    { CHAR c }
  | eof
    { EOF }

{
  let rec parse lexbuf fn =
    let tok = program lexbuf in
      if tok != EOF then (
        fn tok;
        parse lexbuf fn;
      )
}