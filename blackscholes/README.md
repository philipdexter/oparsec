Benchmarking with single-threaded C code from parsec and ocamlopt
produced code.

## 65_6536 options with 100 iterations

Initial results hint that the C version runs in 1.08 seconds while the
direct-translation OCaml code runs in 1.79 seconds.

Compiling with flambda makes the code run a tad slower at 1.82s

## 10_000_000 options with 100 iterations

Initial results hint that the C version runs in 2:45 while the
direct-translation OCaml code runs in 4:36.

Compiling with flambda makes the code run a tad slower at 4:43
