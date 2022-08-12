open Ctypes;;
open Wasmer_ocaml__WasmerBindings;;

val wat2wasm: string -> Byte.Vec.t structure ptr -> unit;;
val wasm_of_wat: string -> Byte.Vec.t structure ptr;;
