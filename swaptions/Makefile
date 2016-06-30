SHELL='zsh'

.PHONY: all
all: ocaml c

.PHONY: ocaml
ocaml:
	ocamlopt hjm_securities.ml -o swaptions.native

.PHONY: c
c:
	cd c && make && cp swaptions ../swaptions.out

.PHONY: bench
bench: ocaml c
	echo c
	time ./swaptions.out -ns 10 -sm 2 -sd 1
	echo ocaml
	time ./swaptions.native -ns 10 -sm 2 -sd 1

.PHONY: clean
clean:
	rm -f hjm_securities.{o,cm{x,i}} swaptions.{out,native}
	cd c && make clean
