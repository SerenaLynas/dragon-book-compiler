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
let basic = "int" | "float" | "boolean"
let num = digit+
let real = digit+ ('.' digit)? ('E' ['+' '-']? digit+)?

rule program = parse
  | ws
    { None }
  | basic as word
    {
      match word with
      | "int" -> Some (BASIC INT)
      | "float" -> Some (BASIC FLOAT)
      | "boolean" -> Some (BASIC BOOLEAN)
      | _ -> None (*Unreachable*)
    }
  | "if"    { Some IF }
  | "else"  { Some ELSE }
  | "while" { Some WHILE }
  | "do"    { Some DO }
  | "break" { Some BREAK }
  | "true"  { Some (BOOLEAN true) }
  | "false" { Some (BOOLEAN false) }
  | id as word
    { Some (ID word) }
  | num as word
    { Some (NUM (int_of_string word)) }
  | real as word
    { Some (REAL (Float.of_string word)) }
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
    { Some (OP word) }
  | _ as c
    { Some (CHAR c) }
  | eof
    { Some EOF }

{
  let rec parse lexbuf fn =
    let tok = program lexbuf in
      if tok != Some EOF then (
        (match tok with
        | Some tok -> fn tok
        | None -> ());
        parse lexbuf fn;
      )
}