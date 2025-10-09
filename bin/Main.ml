open compiler_lib.Lexer

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
    | CHAR ch -> String.make 1 ch
    | OP string -> string
    | BASIC _ -> "basic"
    | ID _ -> "id"
    | NUM _ -> "num"
    | REAL _ -> "real"
    | BOOLEAN bool -> string_of_bool bool
    | _ -> ""
  );
  match tok with
  | ID id -> if Option.is_none (table#get id) then table#put id ()
  | _ -> ()


let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))
let _ = parse lexbuf handle_tok;

Printf.printf "\n\nParsing completed successfully.\n\n";