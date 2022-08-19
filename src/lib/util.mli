open Ctypes;;
open Wasmer_ocaml__WasmerBindings;;

val wat2wasm: string -> Byte.Vec.s -> unit;;
val wasm_of_wat: string -> Byte.Vec.s;;

val (%->): Valkind.ocaml list -> Valkind.ocaml list -> Functype.s;;
