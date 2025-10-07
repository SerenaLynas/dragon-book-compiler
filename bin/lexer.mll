{
  type basic =
    | INT
    | FLOAT
    | BOOLEAN

  (* This corresponds to both the Return Value and yyval *)
  (* The variant is the Return Value and yyval is in the associated data *)
  (* OCaml has no concept of NULL, this is how you would do it. *)   
  type tok =
    | IF
    | THEN
    | ELSE
    | WHILE
    | DO
    | BREAK
    | CHAR of char
    | OP of string
    | BASIC of basic
    | ID of string
    | NUM of int
    | REAL of float (* this is a double *)
    | BOOLEAN of bool
    | EOF
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
  | "true"  { BOOLEAN true }
  | "false" { BOOLEAN false }
  | id as word
    { ID word }
  | num as word
    { NUM (int_of_string word) }
  | real as word
    { REAL (Float.of_string word) }
  | "&&"
  | "||"
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
  | "!" as word
    { OP word }
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