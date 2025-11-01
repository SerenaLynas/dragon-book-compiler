open Compiler_lib
open Compiler_lib.Compiler
open Compiler_lib.Ast

let _ =
  print_endline "\nsize_of_ty test";
  Printf.printf "1. %d\n" (size_of_ty {
    basic = INT;
    dim = [];
  });
  Printf.printf "2. %d\n" (size_of_ty {
    basic = FLOAT;
    dim = [];
  });
  Printf.printf "3. %d\n" (size_of_ty {
    basic = FLOAT;
    dim = [10];
  });
  Printf.printf "3. %d\n" (size_of_ty {
    basic = FLOAT;
    dim = [50; 10];
  });

  print_endline "\naddr test";
  Printf.printf "3:     %s\n" (string_of_addr (CONSTANT_ADDR (CONSTANT_INT 3)));
  Printf.printf "false: %s\n" (string_of_addr (CONSTANT_ADDR (CONSTANT_BOOLEAN false)));
  Printf.printf "var:   %s\n" (string_of_addr (VARIABLE_ADDR ("x", {
    basic = FLOAT;
    dim = [50; 10];
  })));
  Printf.printf "var:   %s\n" (string_of_addr (VARIABLE_ADDR ("y", {
    basic = INT;
    dim = [];
  })));
  Printf.printf "label: %s\n" (string_of_addr (INSTRUCTION "label"));
  Printf.printf "label: %s\n" (string_of_addr (INSTRUCTION "here"));
