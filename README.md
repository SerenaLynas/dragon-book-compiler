# Prereqs
If you are OK not building it yourself and want to skip installing OCaml, I have provided a prebuilt version for Linux amd64 at `./validate/compiler`. (I use Linux, all positive test cases passed on my machine).

Install OCaml: https://ocaml.org/docs/installing-ocaml
You need:
- opam
- dune

You can skip the LSP server, utop, and the other stuff.

# Code
My code is at `bin/lexer.mll` and `bin/main.ml`. `bin/lexer.mll` is the ocamllex source that generates `bin/lexer.ml`

# Run
(Optional) rebuild the lexer
```sh
ocamllex bin/lexer.mll
```

Now you can run the project
```sh
dune exec main validate/TestSuites2/textbook.txt
```

I have provided demo output at `./textbook-output.txt` that I generated with
```sh
dune exec main validate/TestSuites2/textbook.txt > textbook-output.txt
```

# Build
```sh
dune build
```
The executable is located at `_build/default/bin/main.exe`.