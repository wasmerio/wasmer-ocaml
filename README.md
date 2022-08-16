# Wasmer OCaml

This is the (official) OCaml bindings for Wasmer.

# Usage

## Compiling

To compile the library, simply run `make`. An OCaml library
(`Wasmer_ocaml.cmxa`) will be generated in the `lib` folder.
A bytecode archive (`.cma`) and a dynamically-linkable library will also be
generated.

The Wasmer C API library (`libwasmer.so`) must also be accessible by the
linker and all executables.

If you want to use the static library, you must compile using
`make CFLAGS=-DWASM_IMPORT_ALL`, which will generate huge file sizes (all
symbols will be embedded in the executables).

## Checking

To check the library against the provided tests, run `make check`.

Note: using the static Wasmer library will not work, unless you have built the
library with `make CFLAGS=-DWASM_IMPORT_ALL`.

## Running

To run, you need to have the Wasmer C API available for the linker to get.
The library is entirely available in the `lib` folder after compiling.

# Issue

Currently (Tue, Aug 16 2022), OCaml has a bug that makes the program crash on exiting. The issue has been [reported to OCaml](https://github.com/ocaml/ocaml/issues/11489), and (is being fixed for OCaml 4.14)[https://github.com/ocaml/ocaml/pull/11496].
