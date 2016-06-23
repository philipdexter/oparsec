SHELL='zsh'

.PHONY: all
all: ocaml c

.PHONY: ocaml
ocaml:
	ocamlbuild blackscholes.native

.PHONY: c
c:
	g++ -O3 blackscholes.c -o blackscholes.out

.PHONY: bench
bench:
	echo c
	time ./blackscholes.out 1 input_small output.c
	echo ocaml
	time ./blackscholes.native < input_small > output.ocaml && echo $$SHELL

.PHONY: clean
clean:
	ocamlbuild -clean && rm -f blackscholes.out
