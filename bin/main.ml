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
           Printf.printf "Symbols:\n";
           Hashtbl.iter (fun id _ -> Printf.printf "- %s\n" id) tbl;
           match prev with
           | Some e ->
              Printf.printf "Parent env:\n";
              e#print
           | None -> ()
  end
