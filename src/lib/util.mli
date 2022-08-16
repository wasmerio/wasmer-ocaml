open Ctypes;;
open Wasmer_ocaml__WasmerBindings;;

val wat2wasm: string -> Byte.Vec.t structure ptr -> unit;;
val wasm_of_wat: string -> Byte.Vec.t structure ptr;;

val functype_new_0_0: unit -> Functype.t structure ptr;;
val functype_new_args_0: Valtype.Vec.data_type list -> Functype.t structure ptr;;
val functype_new_0_res: Valtype.Vec.data_type list -> Functype.t structure ptr;;
val functype_new_args_res:
	Valtype.Vec.data_type list ->
	Valtype.Vec.data_type list -> Functype.t structure ptr;;
