all: build examples.png S.svg S.png

build: poset.scm
	chicken-install -n

examples: examples.scm build
	csc examples.scm -o examples

examples.gv: examples
	./examples > examples.gv

%.gv: %.gvs
	gvs2gv $<

%.svg: %.gv
	dot -Tsvg -o $@ $<

%.png: %.gv
	dot -Tpng -o $@ $<

clean:
	chicken-clean
	$(RM) examples

.PHONY: all build clean examples
