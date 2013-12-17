packages = amall,lwt,lwt.extra,extlib,cadastr,calendar
emls = $(wildcard views/*.eml)
views = $(emls:.eml=.ml)
files = $(views) $(shell ls *.ml)
subdirs = views/
subdir_includes = $(addprefix -I ,$(subdirs))
name = server

camlc   = ocamlfind ocamlc   $(subdir_includes) -thread $(lib)
camlopt = ocamlfind ocamlopt $(subdir_includes) -thread $(lib)
camldoc = ocamlfind ocamldoc $(subdir_includes) -thread $(lib)
camldep = ocamlfind ocamldep
lib = -package $(packages)

objs		= $(files:.ml=.cmo)
optobjs = $(files:.ml=.cmx)

all: $(name)

$(name): $(optobjs)
	$(camlopt) `ocamldep-sorter $^ < .depend` -linkpkg -o $@

.SUFFIXES: .ml .mli .cmo .cmi .cmx .eml

.eml.ml:
	ecaml -d -header "let f a buf =" -p 'Buffer.add_string buf' -esc-p 'View_helpers.esc_to_buf buf' $<

.ml.cmo:
	$(camlc) -c $<
.mli.cmi:
	$(camlc) -c $<
.ml.cmx:
	$(camlopt) -c $<

.PHONY: doc

doc: $(objs) $(files)
	-mkdir -p doc
	$(camldoc) -html -keep-code -d doc/ -charset utf-8 $(files)

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~
	for dir in $(subdirs); do \
	cd $$dir && rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~ ; \
	done
	-rm -f $(views)
	-rm -f $(name)
	-rm -f .depend

.depend: $(files)
	$(camldep) $(lib) $(files:.ml=.mli) $(files) > .depend

FORCE:

-include .depend
