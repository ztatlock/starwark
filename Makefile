packs = js_of_ocaml
PACKS = $(packs:%=-package %)

libs  = str unix
LIBS  = $(libs:%=-lib %)

DEP = *.ml
OCB = ocamlbuild $(LIBS) $(PACKS)

.PHONY: default
default: StarWark.native

.PHONY: all
all: default

##
## executables
##

%.native: %.ml $(DEP)
	$(OCB) $@

%.byte: %.ml $(DEP)
	$(OCB) $@

# for ocamldebug (bytecode only)
%.d.byte: %.ml $(DEP)
	$(OCB) $@

# for gprof (native code only)
%.p.native: %.ml $(DEP)
	$(OCB) $@

%.inferred.mli: %.ml $(DEP)
	$(OCB) $@

##
## misc
##

.PHONY: clean
clean:
	ocamlbuild -clean
