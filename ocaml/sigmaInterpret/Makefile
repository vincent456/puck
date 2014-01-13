# OCAMLBUILD= ocamlbuild -no-links -classic-display \
# 		-libs unix,nums,aez \
# 		-cflags "-I ../../lib/aez" \
# 		-lflags "-I ../../lib/aez" \
# 		-tags debug,annot

.PHONY=all cleanall realclean clean

OCAMLBUILD= ocamlbuild -no-links -classic-display \
		-tags debug,annot \
		-ocamlyacc menhir \
		-yaccflags -v -X tests

TARGET=native
MAIN=si
TESTS=utests

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
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) $(MAIN).$(TARGET)

$(TESTS): $(TESTS).target
	cp _build/$(TESTS).$(TARGET) $(TESTS)

$(TESTS).target:
	$(OCAMLBUILD) -pkg  oUnit $(TESTS).$(TARGET) 


clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) $(TESTS) *~

cleanall: realclean
