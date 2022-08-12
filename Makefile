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

.depend: src/bindings/wasmerBindings.ml src/bindings/util.ml
	$(SILENCER)$(OCAMLDEP) $(INCLUDES) src/bindings/wasmerBindings.ml src/bindings/util.ml > .depend.tmp
	$(SILENCER)mv .depend.tmp .depend
include .depend


all: wasmer_ocaml test/instance/instance
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

obj/bindings/Wasmer_ocaml__Util.cmi: src/bindings/util.mli src/bindings/wasmerBindings.mli
	$(SILENCER)cp src/bindings/util.mli obj/bindings/Wasmer_ocaml__Util.mli
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		src/bindings/util.mli
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		obj/bindings/Wasmer_ocaml__Util.mli

obj/bindings/Wasmer_ocaml__Util.cmo: src/bindings/util.ml obj/bindings/Wasmer_ocaml__WasmerBindings.cmi obj/bindings/Wasmer_ocaml__Util.cmi
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml__Util.cmo \
		-c src/bindings/util.ml

obj/bindings/Wasmer_ocaml__Util.cmx: src/bindings/util.ml obj/bindings/Wasmer_ocaml__WasmerBindings.cmi obj/bindings/Wasmer_ocaml__Util.cmi
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		-opaque \
		-I obj/bindings \
		$(INCLUDES) \
		-no-alias-deps \
		-o obj/bindings/Wasmer_ocaml__Util.cmx \
		-c src/bindings/util.ml

lib/Wasmer_ocaml.cmi: obj/bindings/Wasmer_ocaml.cmx lib/Wasmer_ocaml__WasmerBindings.cmi lib/Wasmer_ocaml__Util.cmi
	$(SILENCER)cp obj/bindings/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmi
lib/Wasmer_ocaml__WasmerBindings.cmi: obj/bindings/Wasmer_ocaml__WasmerBindings.cmi
	$(SILENCER)cp obj/bindings/Wasmer_ocaml__WasmerBindings.cmi lib/Wasmer_ocaml__WasmerBindings.cmi
lib/Wasmer_ocaml__Util.cmi: obj/bindings/Wasmer_ocaml__Util.cmi
	$(SILENCER)cp obj/bindings/Wasmer_ocaml__Util.cmi lib/Wasmer_ocaml__Util.cmi

obj/bindings/wasmerBindings.o: src/bindings/wasmerBindings.c
	$(SILENCER)$(CC) -c src/bindings/wasmerBindings.c -o obj/bindings/wasmerBindings.o

lib/Wasmer_ocaml.cma: obj/bindings/Wasmer_ocaml.cmo obj/bindings/Wasmer_ocaml__WasmerBindings.cmo obj/bindings/Wasmer_ocaml__Util.cmo obj/bindings/wasmerBindings.o
	@echo "Compiling Wasmer_ocaml.cma"
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		-a \
		-o lib/Wasmer_ocaml.cma \
		obj/bindings/wasmerBindings.o \
		-cclib -lwasmer -ccopt -L$(ROOT)lib \
		obj/bindings/Wasmer_ocaml.cmo \
		obj/bindings/Wasmer_ocaml__WasmerBindings.cmo \
		obj/bindings/Wasmer_ocaml__Util.cmo

lib/Wasmer_ocaml.cmxa: obj/bindings/Wasmer_ocaml.cmx obj/bindings/Wasmer_ocaml__WasmerBindings.cmx obj/bindings/Wasmer_ocaml__Util.cmx obj/bindings/wasmerBindings.o
	@echo "Compiling Wasmer_ocaml.cmxa"
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		-a \
		-o lib/Wasmer_ocaml.cmxa \
		obj/bindings/wasmerBindings.o \
		-cclib -lwasmer -ccopt -L$(ROOT)lib \
		obj/bindings/Wasmer_ocaml.cmx \
		obj/bindings/Wasmer_ocaml__WasmerBindings.cmx \
		obj/bindings/Wasmer_ocaml__Util.cmx

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

obj/test/instance/instance.cmo: lib/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmxa test/instance/instance.ml
	$(SILENCER)$(OCAMLC) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-no-alias-deps \
		-o obj/test/instance/instance.cmo \
		-c \
		test/instance/instance.ml
obj/test/instance/instance.cmx: lib/Wasmer_ocaml.cmi lib/Wasmer_ocaml.cmxa test/instance/instance.ml
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-no-alias-deps \
		-o obj/test/instance/instance.cmx \
		-c \
		test/instance/instance.ml

test/instance/instance: lib/Wasmer_ocaml.cmxa obj/test/instance/instance.cmx
	@echo "Compiling test 'instance'"
	$(SILENCER)$(OCAMLOPT) \
		$(OCAMLFLAGS) \
		$(INCLUDES) \
		-I lib \
		-o test/instance/instance \
		$(OPAM_SWITCH_PREFIX)/lib/bigarray-compat/bigarray_compat.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/integers/integers.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ctypes/ctypes.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ocaml/unix.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ocaml/threads/threads.cmxa \
		$(OPAM_SWITCH_PREFIX)/lib/ctypes/ctypes-foreign.cmxa \
		lib/Wasmer_ocaml.cmxa \
		obj/test/instance/instance.cmx

tests: test/instance/instance
	test/instance/instance

clean:
	$(SILENCER)$(RM) src/bindings/wasmerBindings.cmi
	$(SILENCER)$(RM) obj/bindings/*.mli obj/bindings/*.c* obj/bindings/*.o
	$(SILENCER)$(RM) lib/Wasmer_ocaml*.*
	$(SILENCER)$(RM) src/bindings/wasmer_ocaml.a
	$(SILENCER)$(RM) obj/test/instance/instance.* obj/test/hello/hello.*
	$(SILENCER)$(RM) test/instance/instance test/hello/hello
