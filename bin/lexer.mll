{
  open Printf

  type token =
    | BASIC
    | KEYWORD
    | ID
    | PUNCT
    | NUM
    | REAL
    | CHAR
    | WS
    | EOF
}

let delim = [' ' '\t' '\n' '\r']
let ws = delim+
let letter = ['A' - 'Z' 'a' - 'z']
let digit = ['0' - '9']

let id = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9']*
let basic = "int" | "float" | "boolean"
let num = digit+
let real = digit+ ('.' digit)? ('E' ['+' '-']? digit+)?

rule program = parse
  | ws as word
    { (WS, word) }
  | basic as word
    { (BASIC, word) }
  | "if"
  | "else"
  | "while"
  | "do"
  | "break" as word
    { (KEYWORD, word) }
  | id as word
    { (ID, word)}
  | num as word
    { (NUM, word)}
  | real as word
    { (REAL, word) }
  | "&&"
  | "=="
  | "!="
  | "<"
  | "<="
  | ">="
  | ">"
  | "+"
  | "-"
  | "*"
  | "/"
  | "!"
  | "("
  | ")"
  | "["
  | "]"
  | "{"
  | "}" as word
    { (PUNCT, word) }
  | _ as c
    { (CHAR, String.make 1 c) }
  | eof as word
    { (EOF, word) }

{
  let rec parse lexbuf =
    let (ty, word) = program lexbuf in
      if ty != WS then printf "%s " word;
      if ty != EOF then parse lexbuf

  let main () =
    let lexbuf = Lexing.from_channel stdin in parse lexbuf

  let () = main ()
}