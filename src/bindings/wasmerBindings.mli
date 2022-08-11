open Ctypes;;
open Foreign;;

module VECTOR(U: sig type data
                     val data: data typ
                     val name: string
                     
                     type coerce_from
                     val coerce: coerce_from -> data end) : sig
  module type T = sig
    type data = U.data
    type t = data structure
    val data: data typ
    val name: string
    val t: t typ
    val tsize: (Unsigned.size_t, t) field
    val tdata: (data ptr, t) field

    val make: unit -> t ptr
    val new_empty: t ptr -> unit
    val new_uninitialized: t ptr -> Unsigned.size_t -> unit
    val copy: t ptr -> t ptr -> unit
    val delete: t ptr -> unit
    
    val create_empty: unit -> t ptr
    val create_uninit: int -> t ptr
    val of_array: U.coerce_from array -> t ptr
    val of_list: U.coerce_from list -> t ptr
    val duplicate: t ptr -> t ptr
  end
end;;
module Vector(T: sig type data
                     val data: data typ
                     val name: string
                     
                     type coerce_from
                     val coerce: coerce_from -> data end) : VECTOR(T).T;;

type engine;;
val engine: engine typ;;
val engine_new: unit -> engine ptr;;
val engine_delete: engine ptr -> unit;;

type store;;
val store: store typ;;
val store_new: engine ptr -> store ptr;;
val store_delete: store ptr -> unit;;

type byte = Unsigned.uint8;;
val byte: byte typ;;
module Byte_vec_chara : sig
  type data = byte
  val data: data typ
  val name: string
  
  type coerce_from = char
  val coerce: coerce_from -> data
end;;
module Byte_vec : VECTOR(Byte_vec_chara).T;;

type module_;;
val module_: module_ typ;;
val module_size: (Unsigned.size_t, module_) field;;

val module_new: store ptr -> Byte_vec.t ptr -> module_ ptr;;
val module_validate: store ptr -> Byte_vec.t ptr -> bool;;
val module_delete: module_ ptr -> unit;;

(* val module_imports: module_ ptr -> ImportType_vec.t ptr -> unit;;
val module_exports: module_ ptr -> ExportType_vec.t ptr -> unit;; *)

val module_serialize: module_ ptr -> Byte_vec.t ptr -> unit;;
val module_deserialize: store ptr -> Byte_vec.t ptr -> module_ ptr;;

(* Valtype *)
type valkind =
  | ValKind_i32
  | ValKind_i64
  | ValKind_f32
  | ValKind_f64
  | ValKind_AnyRef
  | ValKind_FuncRef;;
val valkind: Unsigned.uint8 typ;;
val valkind_C_of_OCaml: valkind -> Unsigned.uint8;;
val valkind_OCaml_of_C: Unsigned.uint8 -> valkind;;

type valtype;;
val valtype: valtype typ;;

val valtype_new: valkind -> valtype ptr;;
val valtype_kind: valtype ptr -> valkind;;
val valtype_delete: valtype ptr -> unit;;

module Valtype_vec_chara : sig
  type data = valtype ptr
  val data: data typ
  val name: string
  
  type coerce_from = data
  val coerce: coerce_from -> data
end;;
module Valtype_vec : VECTOR(Valtype_vec_chara).T;;

(* Functype *)
type functype;;
val functype: functype typ;;
val functype_size: (Unsigned.size_t, functype) field;;

val _functype_new_c: Valtype_vec.t ptr -> Valtype_vec.t ptr -> functype ptr;;
val _functype_params_c: functype ptr -> Valtype_vec.t ptr;;
val _functype_results_c: functype ptr -> Valtype_vec.t ptr;;

val functype_new_0_0: unit -> functype ptr;;
val functype_new_args_0: valtype ptr list -> functype ptr;;
val functype_new_0_res: valtype ptr list -> functype ptr;;
val functype_new_args_res: valtype ptr list -> valtype ptr list -> functype ptr;;

val functype_delete: functype ptr -> unit;;

(* Val *)
type val_;;
val val_: val_ typ;;
val val_kind: (Unsigned.size_t, val_) field;;
(*
(* Trap *)
type trap;;
val trap: trap typ;;
val trap_size: (Unsigned.size_t, trap) field;;

(* Func *)
type func;;
val func: func typ;;
val func_size: (Unsigned.size_t, func) field;;

val func_0_0_callback: (val_ ptr -> val_ ptr -> trap ptr) typ;;

val func_new: store ptr -> functype ptr -> (val_ ptr -> val_ ptr -> trap ptr) -> func ptr;;
val func_delete: func ptr -> unit;;

(* Extern *)
type extern;;
val extern: extern typ;;
val extern_size: (Unsigned.size_t, extern) field;;

val func_as_extern: func ptr -> extern ptr;;
val extern_as_func: extern ptr -> func ptr;;

module Extern_vec_chara : sig
  type data = extern ptr
  val data: data typ
  val name: string
end;;
module Extern_vec : VECTOR(Extern_vec_chara).T;;

(* Instance *)
type instance;;
val instance: instance typ;;
val instance_size: (Unsigned.size_t, instance) field;;

val instance_new: store ptr -> module_ ptr -> extern ptr ptr -> unit ptr -> instance ptr;;
val instance_exports: instance ptr -> Extern_vec.t ptr -> unit;;
val func_call: func ptr -> val_ ptr -> val_ ptr -> unit ptr;;
*)
