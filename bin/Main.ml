open Compiler_lib

class env (prev: env option) =
  object
    val mutable tbl = Hashtbl.create 16
    method put id symbol =
      Hashtbl.add tbl id symbol
    method get id : unit option =
      match Hashtbl.find_opt tbl id with
      | Some symbol -> Some symbol
      | None ->
        (match prev with
        | Some e -> e#get id
        | None -> None)
    method print =
      Printf.printf "Symbol Table:\n----\n";
      Hashtbl.iter (fun id _ -> Printf.printf "ID: %s\n" id) tbl;
      match prev with
      | Some e ->
         Printf.printf "Parent env:\n";
         e#print
      | None -> ()
  end

  (*
let table = new env None

let handle_tok tok =
  Printf.printf "%s " (
    match tok with
    | IF -> "if"
    | THEN -> "then"
    | ELSE -> "else"
    | WHILE -> "while"
    | DO -> "do"
    | BREAK -> "break"
    | BASIC _ -> "basic"
    | ID _ -> "id"
    | NUM _ -> "num"
    | REAL _ -> "real"
    | TRUE -> "true"
    | FALSE -> "false"
    | _ -> ""
  );
  match tok with
  | ID id -> if Option.is_none (table#get id) then table#put id ()
  | _ -> ()
*)

let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))
let _ = try Parser.program Lexer.lex lexbuf with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let pos_str = Printf.sprintf "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    Printf.eprintf "%s: Parser error: %a\n" pos_str output_bytes (Bytes.sub lexbuf.lex_buffer lexbuf.lex_curr_pos lexbuf.lex_buffer_len);
    exit(1);;

if !Params.errcount = 0 then
  Printf.printf "\n\nParsing completed successfully.\n\n"
else
  Printf.printf "\n\nParsing failed with %d error(s)\n\n" !Params.errcount;
  exit(1)