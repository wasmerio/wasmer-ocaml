ROOT:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

OCAMLC?=ocamlc
OCAMLOPT?=ocamlopt
OCAMLDEP?=ocamldep

INCLUDES:=\
	-I $(OPAM_SWITCH_PREFIX)/lib/bigarray-compat \
	-I $(OPAM_SWITCH_PREFIX)/lib/bytes \
	-I $(OPAM_SWITCH_PREFIX)/lib/ctypes \
	-I $(OPAM_SWITCH_PREFIX)/lib/integers \
	-I $(OPAM_SWITCH_PREFIX)/lib/ocaml
OCAMLFLAGS+=-strict-sequence -strict-formats \
	-short-paths -keep-locs \
	-g

ifeq ($(SILENT),0)
SILENCER:=
else
SILENCER:=@
endif

all: .depend
.PHONY: all

.depend: src/bindings/wasmerBindings.ml
	$(SILENCER)$(OCAMLDEP) $(INCLUDES) src/bindings/wasmerBindings.ml > .depend.tmp
	$(SILENCER)mv .depend.tmp .depend
include .depend


all: wasmer_ocaml test/hello/hello
wasmer_ocaml: lib/Wasmer_ocaml.cmxs lib/Wasmer_ocaml.cmxa

obj/bindings/Wasmer_ocaml.cmo: src/bindings/wasmer_ocaml.ml
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-nopervasives -nostdlib \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml.cmo \
		-c src/bindings/wasmer_ocaml.ml

obj/bindings/Wasmer_ocaml.cmx: src/bindings/wasmer_ocaml.ml obj/bindings/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		-nopervasives -nostdlib \
		-I obj/bindings \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml.cmx \
		-c src/bindings/wasmer_ocaml.ml

obj/bindings/Wasmer_ocaml__WasmerBindings.cmi: src/bindings/wasmerBindings.mli
	$(SILENCER)cp src/bindings/wasmerBindings.mli obj/bindings/Wasmer_ocaml__WasmerBindings.mli
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		src/bindings/wasmerBindings.mli
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		obj/bindings/Wasmer_ocaml__WasmerBindings.mli

obj/bindings/Wasmer_ocaml__WasmerBindings.cmo: src/bindings/wasmerBindings.ml obj/bindings/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml__WasmerBindings.cmo \
		-c src/bindings/wasmerBindings.ml

obj/bindings/Wasmer_ocaml__WasmerBindings.cmx: src/bindings/wasmerBindings.ml obj/bindings/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml__WasmerBindings.cmx \
		-c src/bindings/wasmerBindings.ml

lib/Wasmer_ocaml.cmi: obj/bindings/Wasmer_ocaml.cmx lib/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)cp obj/bindings/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmi
lib/Wasmer_ocaml__WasmerBindings.cmi: obj/bindings/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)cp obj/bindings/Wasmer_ocaml__WasmerBindings.cmi lib/Wasmer_ocaml__WasmerBindings.cmi

obj/bindings/wasmerBindings.o: src/bindings/wasmerBindings.c
	$(SILENCER)$(CC) -c src/bindings/wasmerBindings.c -o obj/bindings/wasmerBindings.o

lib/Wasmer_ocaml.cma: obj/bindings/Wasmer_ocaml.cmo obj/bindings/Wasmer_ocaml__WasmerBindings.cmo obj/bindings/wasmerBindings.o
	@echo "Compiling Wasmer_ocaml.cma"
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-a \
		-o lib/Wasmer_ocaml.cma \
		obj/bindings/wasmerBindings.o \
		-cclib -lwasmer -ccopt -L$(ROOT)lib \
		obj/bindings/Wasmer_ocaml.cmo \
		obj/bindings/Wasmer_ocaml__WasmerBindings.cmo

lib/Wasmer_ocaml.cmxa: obj/bindings/Wasmer_ocaml.cmx obj/bindings/Wasmer_ocaml__WasmerBindings.cmx obj/bindings/wasmerBindings.o
	@echo "Compiling Wasmer_ocaml.cmxa"
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		-a \
		-o lib/Wasmer_ocaml.cmxa \
		obj/bindings/wasmerBindings.o \
		-cclib -lwasmer -ccopt -L$(ROOT)lib \
		obj/bindings/Wasmer_ocaml.cmx \
		obj/bindings/Wasmer_ocaml__WasmerBindings.cmx

lib/Wasmer_ocaml.cmxs: lib/Wasmer_ocaml.cmxa
	@echo "Compiling Wasmer_ocaml.cmxs"
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) -shared -linkall \
		-I src/bindings \
		-o lib/Wasmer_ocaml.cmxs \
		lib/Wasmer_ocaml.cmxa


obj/test/hello/hello.cmo: lib/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmxa test/hello/hello.ml
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-no-alias-deps \
		-o obj/test/hello/hello.cmo \
		-c \
		test/hello/hello.ml
obj/test/hello/hello.cmx: lib/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmxa test/hello/hello.ml
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-no-alias-deps \
		-o obj/test/hello/hello.cmx \
		-c \
		test/hello/hello.ml

test/hello/hello: lib/Wasmer_ocaml.cmxa obj/test/hello/hello.cmx
	@echo "Compiling test 'hello'"
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-o test/hello/hello \
		$(OPAM_SWITCH_PREFIX)/lib/bigarray-compat/bigarray_compat.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/integers/integers.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ctypes/ctypes.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ocaml/unix.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ocaml/threads/threads.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ctypes/ctypes-foreign.cmxa \
		lib/Wasmer_ocaml.cmxa \
		obj/test/hello/hello.cmx

tests: test/hello/hello
	test/hello/hello

clean:
	$(SILENCER)$(RM) src/bindings/wasmerBindings.cmi
	$(SILENCER)$(RM) obj/bindings/*.mli obj/bindings/*.c* obj/bindings/*.o
	$(SILENCER)$(RM) lib/Wasmer_ocaml*.*
	$(SILENCER)$(RM) src/bindings/wasmer_ocaml.a
	$(SILENCER)$(RM) obj/test/hello/hello.*
	$(SILENCER)$(RM) test/hello/hello
