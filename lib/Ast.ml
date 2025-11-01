type basic =
  | INT
  | FLOAT
  | BOOLEAN

let string_of_basic basic = match basic with
  | INT -> "int"
  | FLOAT -> "float"
  | BOOLEAN -> "boolean"

type ty = {
  basic: basic;
  dim: int list;
}

let count = ref 0

let tmp_var () =
  count := count.contents + 1;
  "$t" ^ string_of_int count.contents

let string_of_dim dim = String.concat "" @@ let f int = "[" ^ (string_of_int int) ^ "]" in List.map f dim
let string_of_ty ty = (string_of_basic ty.basic) ^ (string_of_dim ty.dim)

type constant =
  | CONSTANT_INT of int
  | CONSTANT_FLOAT of float
  | CONSTANT_BOOLEAN of bool

let string_of_constant const = match const with
  | CONSTANT_INT int -> "constant integer " ^ string_of_int int
  | CONSTANT_FLOAT float -> "constant float " ^ string_of_float float
  | CONSTANT_BOOLEAN bool -> "constant boolean " ^ string_of_bool bool 

let short_string_of_constant const = match const with
  | CONSTANT_INT int -> string_of_int int
  | CONSTANT_FLOAT float -> string_of_float float
  | CONSTANT_BOOLEAN bool -> string_of_bool bool 

let size_of_basic basic = match basic with
  | INT -> 4
  | FLOAT -> 8
  | BOOLEAN -> 1

let size_of_ty ty =
  (size_of_basic ty.basic) * (let f a b = a * b in List.fold_left f 1 ty.dim)

type 'expr _loc = {
  id: string;
  idx: 'expr list
}

type expr =
  | OR of expr * expr
  | AND of expr * expr
  | EQ of expr * expr
  | NOT of expr
  | LT of expr * expr
  | LTEQ of expr * expr
  | GT of expr * expr
  | GTEQ of expr * expr
  | ADD of expr * expr
  | SUB of expr * expr
  | MUL of expr * expr
  | DIV of expr * expr
  | MINUS of expr
  | PLUS of expr
  | LOC of expr _loc
  | LIT_INT of int
  | LIT_FLOAT of float
  | LIT_BOOL of bool

type loc = expr _loc

type decl =
  | DECL of string * ty

type stmt =
  | ASSIGN of loc * expr
  | IF of expr * stmt * stmt option
  | WHILE of expr * stmt
  | BREAK
  | BLOCK of (decl list) * (stmt list)

