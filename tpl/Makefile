server_name = dresina-server

server_packages = amall,amall.dbi,threads,lwt,lwt.preemptive
all_packages = amall,amall.dbi,threads,lwt,lwt.preemptive

server_topdirs = app config internal/common internal/server db
server_mls = $(shell find $(server_topdirs) -name '*.ml')
server_dirs = $(shell find $(server_topdirs) -type d)

all_mls = $(shell find . -name '*.ml')
all_dirs = $(shell find . -type d)
all_ml_includes = $(addprefix -I , $(server_dirs))

server_lib = -package $(server_packages)
all_lib = -package $(all_packages)

all_opts = $(all_lib) $(all_ml_includes)
all_comp_opts = -w A -thread

ocamlc_noopts = ocamlfind ocamlc
ocamlc   = $(ocamlc_noopts)   $(all_opts) $(all_comp_opts)
ocamlopt = ocamlfind ocamlopt $(all_opts) $(all_comp_opts)
ocamldep = ocamlfind ocamldep $(all_opts)

server_objs    = $(server_mls:.ml=.cmo)
server_optobjs = $(server_mls:.ml=.cmx)

sch_files = $(shell find db/migrate internal/common internal/make_schema \
                      internal/common \
                      -name '*.ml')
sch_ocamlc = $(ocamlc_noopts) $(all_comp_opts)
sch_objs = $(sch_files:.ml=.cmo)
sch_packages = cadastr,amall.dbi

all: $(server_name) db/schema.bin

# server_sorted = $(shell ocamldsort -opt $(server_optobjs) < .depend)
server_sorted = $(shell ocamldep-sorter $(server_optobjs) < .depend)

$(server_name): $(server_optobjs)
	$(ocamlopt) $(server_sorted) -linkpkg -o $@

# sch_sorted = $(shell ocamldsort -byte $(sch_objs) < .depend)
sch_sorted = $(shell ocamldep-sorter $(sch_objs) < .depend)

make_schema: $(sch_objs)
	$(sch_ocamlc) \
	  -package $(sch_packages) \
	  $(sch_sorted) \
	  -linkpkg -o $@

db/schema.bin: make_schema
	./make_schema $@

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(ocamlc) -c $<
.mli.cmi:
	$(ocamlc) -c $<
.ml.cmx:
	$(ocamlopt) -c $<

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~ $(server_name)
	-rm -f .depend

.depend: $(all_mls)
	$(ocamldep) $(all_mls:.ml=.mli) $(all_mls) > .depend

FORCE:

-include .depend
