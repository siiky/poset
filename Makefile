all: build examples.png

build: poset.scm
	chicken-install -n

examples: examples.scm build
	csc examples.scm -o examples

examples.gv: examples
	./examples > examples.gv

examples.png: examples.gv
	dot -Tpng -o examples.png examples.gv

clean:
	chicken-clean
	$(RM) examples

.PHONY: all build clean examples
