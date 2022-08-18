open Ctypes;;
open Wasmer_ocaml__WasmerBindings;;

val wat2wasm: string -> Byte.Vec.s -> unit;;
val wasm_of_wat: string -> Byte.Vec.s;;

val functype_new_0_0: unit -> Functype.s;;
val functype_new_args_0: Valtype.V.owning_struct list -> Functype.s;;
val functype_new_0_res: Valtype.V.owning_struct list -> Functype.s;;
val functype_new_args_res:
	Valtype.V.owning_struct list ->
	Valtype.V.owning_struct list -> Functype.s;;
