let errcount = ref 0;

open Ast

type error =
  | UNKNOWN_CHAR of char

type ident =
  | NAMED of string
  | TEMPORARY of int

let string_of_ident ident = match ident with
  | NAMED id -> id
  | TEMPORARY tmp -> "t_" ^ string_of_int tmp

type symbol = {
  stack_addr: int;
  ident: ident;
  ty: ty;
}

let string_of_symbol (symbol : symbol) =
  Printf.sprintf "%s (%#010x) : %s" (string_of_ident symbol.ident) symbol.stack_addr (string_of_ty symbol.ty)

class env prev stack_addr =
  object (self)
    val mutable symbols : (ident, symbol) Hashtbl.t = Hashtbl.create 16
    val mutable temp_counter = 0
    val mutable stack_addr = stack_addr
    method stack_addr = stack_addr
    method parent =
      match prev with
      | Some prev -> prev
      | None -> (self :> env)
    method put (id : string) (ty : ty) =
      let ident = NAMED id in
      Hashtbl.add symbols ident {
        stack_addr = stack_addr;
        ident = ident;
        ty = ty;
      };
      stack_addr <- stack_addr + size_of_ty ty;
      Printf.printf "env <- %s\n" id;
      ident
    method put_temp (ty : ty) =
      let count = temp_counter in (let ident = TEMPORARY count in (
        temp_counter <- temp_counter + 1;
        Hashtbl.add symbols ident {
          stack_addr = stack_addr;
          ident = ident;
          ty = ty;
        };
        stack_addr <- stack_addr + size_of_ty ty;
        Printf.printf "env <- %s\n" @@ string_of_ident ident;
        ident
      ))
    method get (ident : ident) : symbol option =
      Printf.printf "env -> %s\n" @@ string_of_ident ident;
      match Hashtbl.find_opt symbols ident with
      | Some value -> Some value
      | None ->
        (match prev with
        | Some e -> e#get ident
        | None -> None)
    method print =
      Printf.printf "symbol table {\n";
      Hashtbl.iter (fun id entry -> print_endline @@ "  " ^ string_of_symbol entry) symbols;
      match prev with
      | Some e ->
        Printf.printf "} with parent ";
        e#print
      | None ->
        Printf.printf "}\n\n"
  end

type addr =
  | CONSTANT_ADDR of constant
  | VARIABLE_ADDR of string * ty
  | INSTRUCTION of string

let string_of_addr addr = match addr with
  | CONSTANT_ADDR const -> string_of_constant const
  | VARIABLE_ADDR (id, ty) -> string_of_ty ty ^ " " ^ id
  | INSTRUCTION str -> "instruction " ^ str
  
let short_string_of_addr addr = match addr with
  | CONSTANT_ADDR const -> short_string_of_constant const
  | VARIABLE_ADDR (id, _) -> id
  | INSTRUCTION str -> "instruction " ^ str

type unary_ir =
  | MOV
  | MINUS
  | PLUS
  | NOT
  | WIDEN

let string_of_unary_ir ir = match ir with
  | MOV -> ""
  | MINUS -> "minus "
  | PLUS -> "plus "
  | NOT -> "not "
  | WIDEN -> "(float) "

type binary_ir =
  | ADD 
  | SUB 
  | MUL 
  | DIV 
  | OR
  | AND 
  | EQ
  | LT
  | LTEQ
  | GT
  | GTEQ

let string_of_binary_ir ir = match ir with
  | ADD -> " + "
  | SUB -> " - "
  | MUL -> " * "
  | DIV -> " / "
  | OR -> " || "
  | AND -> " && "
  | EQ -> " == "
  | LT -> " < "
  | LTEQ -> " <= "
  | GT -> " > "
  | GTEQ -> " >= "

type ir =
  | UNARY of unary_ir * ident * ident
  | BINARY of binary_ir * ident * ident * ident
  | LOAD of ident * constant
  | LOAD_IDX of ident * ident * ident
  | TYPE_ERROR of ident

let string_of_ir ir = match ir with
  | UNARY (unary_ir, dst, src) ->
    string_of_ident dst ^ " = " ^ (string_of_unary_ir unary_ir) ^ string_of_ident src
  | BINARY (binary_ir, dst, src1, src2) ->
    string_of_ident dst ^ " = " ^ string_of_ident src1 ^ (string_of_binary_ir binary_ir) ^ string_of_ident src2
  | LOAD (ident, const) ->
    string_of_ident ident ^ " = " ^ short_string_of_constant const ^ " (load constant)"
  | LOAD_IDX (dst, list, idx) ->
    string_of_ident dst ^ " = " ^ string_of_ident list ^ "[" ^ string_of_ident idx ^ "]"
  | TYPE_ERROR (ident) ->
    "TYPE ERROR of " ^ string_of_ident ident

class irc =
  object
    val mutable code = []
    method append (ir: ir) =
      code <- ir :: code;
      print_endline @@ string_of_ir ir
    method print: unit =
      print_endline "\nir code\n---";
      let _ = (let rev = List.rev code in
      let f x = print_endline @@ string_of_ir x in
      List.map f rev) in ();
      print_endline "==="
  end

let prod list =
  List.fold_left ( * ) 1 list

let widen (env: env) (irc: irc) ident dim =
  let ty = {
    basic = FLOAT;
    dim = dim;
  } in
  let dst = env#put_temp ty in
  irc#append (UNARY (WIDEN, dst, ident));
  (dst, ty)

let coerce_ty (env: env) (irc: irc) ident ty =
  let sty = (env#get ident |> Option.get).ty in
  if sty = ty then ident else
    if sty.dim = ty.dim && sty.basic = INT && ty.basic = FLOAT then
      let (ident, _) = widen env irc ident ty.dim in ident
    else (irc#append (TYPE_ERROR ident); ident)

let rec codegen_ir_loc_impl (env: env) (irc: irc) (id: ident) (idx: ident list) (ty: ty) = match idx with
  | [] -> (id, ty)
  | [x] ->
    let ty = {
      basic = ty.basic;
      dim = List.tl ty.dim
    } in
    let size = env#put_temp {
      basic = INT;
      dim = []
    } in
    let mult = env#put_temp ty in
    irc#append (LOAD (size, CONSTANT_INT (size_of_ty ty)));
    irc#append (BINARY (MUL, mult, size, id));
    let dst = env#put_temp ty in
    irc#append (LOAD_IDX (dst, id, x));
    (dst, ty)
  | x :: y :: rest ->
    let xsize :: ysize :: restsize = ty.dim in
    let width = env#put_temp {
      basic = INT;
      dim = [];
    } in
    let multiplied = env#put_temp {
      basic = INT;
      dim = [];
    } in
    let added = env#put_temp {
      basic = INT;
      dim = [];
    } in (
    irc#append (LOAD (width, CONSTANT_INT xsize));
    irc#append (BINARY (MUL, multiplied, y, width));
    irc#append (BINARY (ADD, added, multiplied, x));
    codegen_ir_loc_impl env irc id (added :: rest) {
      basic = ty.basic;
      dim = (xsize * ysize) :: restsize
    })

let rec codegen_ir_unary (env: env) (irc: irc) (op: unary_ir) (expr: expr) =
  let (ident, ty) = codegen_ir_expr env irc expr in
  let dst = env#put_temp ty in (
    irc#append (UNARY (op, dst, ident));
    (dst, ty)
  )
and codegen_ir_binary (env: env) (irc: irc) (op: binary_ir) (left: expr) (right: expr) =
  let (left_ident, left_ty) = codegen_ir_expr env irc left in
  let (right_ident, right_ty) = codegen_ir_expr env irc right in
  let ((left_ident, left_ty), (right_ident, right_ty)) =
    if left_ty.basic != right_ty.basic then (
      (if left_ty.basic != FLOAT then
        widen env irc left_ident left_ty.dim else (left_ident, left_ty)),
      if right_ty.basic != FLOAT then
        widen env irc right_ident right_ty.dim else (right_ident, right_ty)
    ) else ((left_ident, left_ty), (right_ident, right_ty)) in
  let dst = env#put_temp left_ty in (
    if left_ty <> right_ty then irc#append (TYPE_ERROR right_ident);
    irc#append (BINARY (op, dst, left_ident, right_ident));
    (dst, left_ty)
  )
and codegen_ir_expr (env: env) (irc: irc) (expr: expr) = match expr with
  | Ast.OR (a, b) -> codegen_ir_binary env irc OR a b
  | Ast.AND (a, b) -> codegen_ir_binary env irc AND a b
  | Ast.EQ (a, b) -> codegen_ir_binary env irc EQ a b
  | Ast.NOT a -> codegen_ir_unary env irc NOT a
  | Ast.LT (a, b) -> codegen_ir_binary env irc LT a b
  | Ast.LTEQ (a, b) -> codegen_ir_binary env irc LTEQ a b
  | Ast.GT (a, b) -> codegen_ir_binary env irc GT a b
  | Ast.GTEQ (a, b) -> codegen_ir_binary env irc GTEQ a b
  | Ast.ADD (a, b) -> codegen_ir_binary env irc ADD a b
  | Ast.SUB (a, b) -> codegen_ir_binary env irc SUB a b
  | Ast.MUL (a, b) -> codegen_ir_binary env irc MUL a b
  | Ast.DIV (a, b) -> codegen_ir_binary env irc DIV a b
  | Ast.MINUS a -> codegen_ir_unary env irc MINUS a
  | Ast.PLUS a -> codegen_ir_unary env irc PLUS a
  | Ast.LIT_INT int ->
    let ty = { basic = INT; dim = []; } in
    let ident = env#put_temp ty in
    irc#append (LOAD (ident, CONSTANT_INT int));
    (ident, ty)
  | Ast.LIT_FLOAT float ->
    let ty = { basic = FLOAT; dim = []; } in
    let ident = env#put_temp ty in
    irc#append (LOAD (ident, CONSTANT_FLOAT float));
    (ident, ty)
  | Ast.LIT_BOOL bool ->
    let ty = { basic = BOOLEAN; dim = []; } in
    let ident = env#put_temp ty in
    irc#append (LOAD (ident, CONSTANT_BOOLEAN bool));
    (ident, ty)
  | Ast.LOC loc -> 
    let id = NAMED loc.id in
    let ty = (env#get id |> Option.get).ty in
    let f expr =
      let (ident, _) = codegen_ir_expr env irc expr in ident
    in
    let idx = List.map f loc.idx in
    codegen_ir_loc_impl env irc id idx ty

let rec codegen_ir_stmt (env: env) (irc: irc) (stmt: stmt) = match stmt with
  | ASSIGN (loc, expr) ->
    let (loc, loc_ty) = codegen_ir_expr env irc (Ast.LOC loc) in
    let (expr, expr_ty) = codegen_ir_expr env irc expr in
    let expr = coerce_ty env irc expr loc_ty in
    irc#append (UNARY (MOV, loc, expr))
  | IF (cond, then_stmt, else_stmt) ->
    let (cond, _) = codegen_ir_expr env irc cond in
    (* todo: integrate branching *)
    codegen_ir_stmt env irc then_stmt;
    (match else_stmt with
      | Some else_stmt -> codegen_ir_stmt env irc else_stmt
      | None -> ())
  | WHILE (cond, then_stmt) ->
    let (cond, _) = codegen_ir_expr env irc cond in
    (* todo: integrate branching *)
    codegen_ir_stmt env irc then_stmt
  | BREAK -> ()
  | BLOCK (decls, stmts) ->
    let env = new env (Some env) env#stack_addr in
    let g (decl : decl) = (match decl with
      | DECL (id, ty) -> env#put id ty) in
    let _ = List.map g decls in
    let f stmt = codegen_ir_stmt env irc stmt in
    let _ = List.map f stmts in
    env#print

