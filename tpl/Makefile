name = dresina-server
packages = amall,amall.dbi,threads

topdirs = app config internal
files = $(shell find $(topdirs) -name '*.ml')
dirs = $(shell find $(topdirs) -type d)
ml_includes = $(addprefix -I , $(dirs))

lib = -package $(packages)
common_opts = $(lib) $(ml_includes)
camlc   = ocamlfind ocamlc   -thread $(common_opts)
camlopt = ocamlfind ocamlopt -thread $(common_opts)
camldep = ocamlfind ocamldep         $(common_opts)

objs    = $(files:.ml=.cmo)
optobjs = $(files:.ml=.cmx)

all: $(name)

$(name): $(optobjs)
	$(camlopt) `ocamldep-sorter $^ < .depend` -linkpkg -o $@

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(camlc) -c $<
.mli.cmi:
	$(camlc) -c $<
.ml.cmx:
	$(camlopt) -c $<

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~ $(name)
	-rm -f .depend

.depend: $(files)
	$(camldep) $(files:.ml=.mli) $(files) > .depend

FORCE:

-include .depend
