open Compiler_lib
open Compiler_lib.Compiler
open Compiler_lib.Ast

let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))
let program = try Parser.program Lexer.lex lexbuf with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let pos_str = Printf.sprintf "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    Printf.eprintf "%s: Parser error: %a\n" pos_str output_bytes (Bytes.sub lexbuf.lex_buffer lexbuf.lex_curr_pos lexbuf.lex_buffer_len);
    exit(1);;

let code =
  let env = new env None 0 in
  let irc = new irc in
  codegen_ir_stmt env irc program;
  irc#print;
  irc;;

if !Compiler.errcount = 0 then
  Printf.printf "\nCompiling completed successfully.\n\n"
else
  (Printf.printf "\n\nCompiling failed with %d error(s)\n\n" !Compiler.errcount;
  exit(1))