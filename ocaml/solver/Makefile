.PHONY=all cleanall realclean clean

OCAMLBUILD= ocamlbuild -no-links -classic-display \
		-tags debug,annot \
		-ocamlyacc menhir \
		-yaccflags -v

TARGET=native

MAIN=solver
FRONT=front


all: $(MAIN)

native: TARGET := native
native: all
opt: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := byte
byte: all
$(MAIN).byte: byte


$(MAIN): $(MAIN).target
	cp _build/$(FRONT).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) $(FRONT).$(TARGET)


clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) *~

cleanall: realclean
