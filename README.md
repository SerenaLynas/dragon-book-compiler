# Compiler for the Dragon Book
This is a compiler for the C subset given by the dragon book.

## Prereqs
Install OCaml and dune.

## Run
Run the project like so:
```sh
dune exec main -- ./validate/TestSuites2/textbook.txt
```
## Build
```sh
dune build
```
The executable is located at `_build/default/bin/main.exe`.