open Ctypes;;
open Wasmer__WasmerBindings;;

val wat2wasm: string -> Byte.Vec.s -> unit;;
val wasm_of_wat: string -> Byte.Vec.s;;

val (%->): Valkind.ocaml list -> Valkind.ocaml list -> Functype.s;;

val load_wasm_file: string -> Byte.Vec.s;;
