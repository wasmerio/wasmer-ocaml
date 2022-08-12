open Ctypes;;
open Foreign;;

module type StructType = sig
  val name: string
end;;
module type VectorType = sig
  type data_type
  val data_type: data_type typ
  val name: string
end;;

module type DECLARE_STRUCT = sig
  val name: string
  type t
  val t: t structure typ
end;;

module type DECLARE_OWN = sig
  include DECLARE_STRUCT
  
  val delete: t structure ptr -> unit
end;;

module DECLARE_VEC(U: VectorType) : sig
  module type T = sig
    type data_type = U.data_type
    val data_type: data_type typ
    
    type t
    val t: t structure typ
    val fsize: (Unsigned.size_t, t structure) field
    val fdata: (data_type ptr, t structure) field
    
    val name: string
    val make: unit -> t structure ptr
    val new_empty: t structure ptr -> unit
    val new_uninitialized: t structure ptr -> Unsigned.size_t -> unit
    val new_: t structure ptr -> Unsigned.size_t -> data_type ptr -> unit
    val new_carray: t structure ptr -> data_type carray -> unit
    val copy: t structure ptr -> t structure ptr -> unit
    val delete: t structure ptr -> unit
    
    val make_empty: unit -> t structure ptr
    val make_uninit: int -> t structure ptr
    val of_list: data_type list -> t structure ptr
    val duplicate: t structure ptr -> t structure ptr
  end
end;;

module type DECLARE_TYPE = sig
  include DECLARE_OWN
  module V : sig
    type data_type = t structure ptr
    val data_type: data_type typ
    val name: string
  end
  module Vec: DECLARE_VEC(V).T
  
  val duplicate: t structure ptr -> t structure ptr
end;;

module type DECLARE_REF_BASE = sig
  include DECLARE_OWN
  
  val duplicate: t structure ptr -> t structure ptr
  val same: t structure ptr -> t structure ptr -> bool
  val get_host_info: t structure ptr -> unit ptr
  val set_host_info: t structure ptr -> unit ptr -> unit
  val set_host_info_with_finalizer: t structure ptr -> unit ptr -> (unit ptr -> unit) -> unit
end;;

module Ref_T: StructType;;
module Ref: DECLARE_STRUCT;;

module type DECLARE_REF = sig
  include DECLARE_REF_BASE
  
  val to_ref: t structure ptr -> Ref.t structure ptr
  val of_ref: Ref.t structure ptr -> t structure ptr
  val to_ref_const: t structure ptr -> Ref.t structure ptr
  val of_ref_const: Ref.t structure ptr -> t structure ptr
end;;


module Byte : sig
  type byte = Unsigned.uint8
  val byte: byte typ
  module V: VectorType with type data_type = byte
  module Vec : sig
    include DECLARE_VEC(V).T
    
    val of_char_list: char list -> t structure ptr
    val of_int_list: int list -> t structure ptr
    val of_bytes: bytes -> t structure ptr
  end
end;;
module Name : sig
  include module type of Byte.Vec
  
  val of_string: string -> t structure ptr
end;;

module Config_T: StructType;;
(** Embedders may provide custom functions for manipulating configs. *)
module Config : sig
  include DECLARE_OWN
  
  val new_: unit -> t structure ptr
end;;

module Engine_T: StructType;;
module Engine : sig
  include DECLARE_OWN
  
  val new_: unit -> t structure ptr
  val new_with_config: Config.t structure ptr -> t structure ptr
end;;

module Store_T: StructType;;
module Store : sig
  include DECLARE_OWN
  
  val new_: Engine.t structure ptr -> t structure ptr
end;;

module Mutability : sig
  type t = Unsigned.uint8
  val t: t typ
end;;
module Mutability_e : sig
  val const: int
  val var: int
end;;

module Limits : sig
  type t
  val t: t structure typ
  val min: (Unsigned.uint32, t structure) field
  val max: (Unsigned.uint32, t structure) field
end;;
val limits_max_default: int

module Valkind : sig
  type valkind_OCaml =
    | ValKind_i32
    | ValKind_i64
    | ValKind_f32
    | ValKind_f64
    | ValKind_AnyRef
    | ValKind_FuncRef
  
  type valkind_C = Unsigned.uint8
  val valkind_C: valkind_C typ
  val valkind_C_of_OCaml: valkind_OCaml -> valkind_C
  val valkind_OCaml_of_C: valkind_C -> valkind_OCaml
end;;

module Valtype_T: StructType;;
module Valtype : sig
  include DECLARE_TYPE
  
  val new_: Valkind.valkind_OCaml -> t structure ptr
  val kind: t structure ptr -> Valkind.valkind_OCaml
end;;

module Functype_T: StructType;;
module Functype : sig
  include DECLARE_TYPE
  
  val new_: Valtype.Vec.t structure ptr -> Valtype.Vec.t structure ptr -> t structure ptr
  val params: t structure ptr -> Valtype.Vec.t structure ptr
  val results: t structure ptr -> Valtype.Vec.t structure ptr
end;;

module Globaltype_T: StructType;;
module Globaltype : sig
  include DECLARE_TYPE
  
  val new_: Valtype.t structure ptr -> Valkind.valkind_C ptr -> t structure ptr
  val content: t structure ptr -> Valtype.t structure ptr
  val mutability: t structure ptr -> Valkind.valkind_C
end;;

module Tabletype_T: StructType;;
module Tabletype : sig
  include DECLARE_TYPE
  
  val new_: Valtype.t structure ptr -> Limits.t structure ptr -> t structure ptr
  val elements: t structure ptr -> Valtype.t structure ptr
  val limits: t structure ptr -> Limits.t structure ptr
end;;

module Memorytype_T: StructType;;
module Memorytype : sig
  include DECLARE_TYPE
  
  val new_: Limits.t structure ptr -> t structure ptr
  val limits: t structure ptr -> Limits.t structure ptr
end;;

module Externkind : sig
  type externkind_OCaml =
    | ExternKind_Func
    | ExternKind_Global
    | ExternKind_Table
    | ExternKind_Memory
  
  type externkind_C = Unsigned.uint8
  val externkind_C: externkind_C typ
  val externkind_C_of_OCaml: externkind_OCaml -> externkind_C
  val externkind_OCaml_of_C: externkind_C -> externkind_OCaml
end;;

module Externtype_T: StructType;;
module Externtype : sig
  include DECLARE_TYPE
  
  val kind: t structure ptr -> Externkind.externkind_OCaml
  
  val of_functype: Functype.t structure ptr -> t structure ptr
  val of_globaltype: Globaltype.t structure ptr -> t structure ptr
  val of_tabletype: Tabletype.t structure ptr -> t structure ptr
  val of_memorytype: Memorytype.t structure ptr -> t structure ptr
  
  val to_functype: t structure ptr -> Functype.t structure ptr
  val to_globaltype: t structure ptr -> Globaltype.t structure ptr
  val to_tabletype: t structure ptr -> Tabletype.t structure ptr
  val to_memorytype: t structure ptr -> Memorytype.t structure ptr
  
  val of_functype_const: Functype.t structure ptr -> t structure ptr
  val of_globaltype_const: Globaltype.t structure ptr -> t structure ptr
  val of_tabletype_const: Tabletype.t structure ptr -> t structure ptr
  val of_memorytype_const: Memorytype.t structure ptr -> t structure ptr
  
  val to_functype_const: t structure ptr -> Functype.t structure ptr
  val to_globaltype_const: t structure ptr -> Globaltype.t structure ptr
  val to_tabletype_const: t structure ptr -> Tabletype.t structure ptr
  val to_memorytype_const: t structure ptr -> Memorytype.t structure ptr
end;;

module Importtype_T: StructType;;
module Importtype : sig
  include DECLARE_TYPE
  
  val new_: Name.t structure ptr -> Name.t structure ptr -> Externtype.t structure ptr -> t structure ptr
  val module_: t structure ptr -> Name.t structure ptr
  val name: t structure ptr -> Name.t structure ptr
  val type_: t structure ptr -> Externtype.t structure ptr
end;;

module Exporttype_T: StructType;;
module Exporttype : sig
  include DECLARE_TYPE
  
  val new_: Name.t structure ptr -> Externtype.t structure ptr -> t structure ptr
  val name_: t structure ptr -> Name.t structure ptr
  val type_: t structure ptr -> Externtype.t structure ptr
end;;

module Val : sig
  type t
  val t: t structure typ
  val fkind: (Valkind.valkind_C, t structure) field
  module Anon0 : sig
    type t
    val t: t union typ
    val fi32: (int32, t union) field
    val fi64: (int64, t union) field
    val ff32: (float, t union) field
    val ff64: (float, t union) field
    val fref: (Ref.t structure ptr, t union) field
  end
  val fof: (Anon0.t union, t structure) field
  
  val of_i32: int32 -> t structure
  val of_i64: int64 -> t structure
  val of_f32: float -> t structure
  val of_f64: float -> t structure
  val of_ref: Ref.t structure ptr -> t structure
  val new_: unit -> t structure
  
  val copy: t structure ptr -> t structure ptr -> unit
  val delete: t structure ptr -> unit
  
  val duplicate: t structure ptr -> t structure
  
  module V: VectorType with type data_type = t structure
  module Vec: DECLARE_VEC(V).T
end;;


module type DECLARE_SHAREABLE_REF = sig
  include DECLARE_REF
  module S: StructType
  module Shared: DECLARE_OWN
  
  val to_shared: t structure ptr -> Shared.t structure ptr
  val of_shared: Store.t structure ptr -> Shared.t structure ptr -> t structure ptr
end;;

(*
(* Module, Extern, Trap *)

module Instance_T: StructType;;

module Instance : sig
  include DECLARE_REF
  
  val new_: Store.t ptr -> Module.t ptr -> Extern.Vec.t ptr -> Trap.t ptr ptr -> t ptr
  
  val exports: t ptr -> Extern.Vec.t ptr -> unit
end;;

module Frame_T: StructType;;

module Frame : sig
  include DECLARE_OWN
  module V : VectorType with type data_type = t structure ptr
  module Vec : DECLARE_VEC(V).T
  val duplicate: t ptr -> t ptr
  
  val instance: t ptr -> Instance.t ptr
  val func_index: t ptr -> uint32_t
  val func_offset: t ptr -> size_t
  val module_offset: t ptr -> size_t
end;;
*)
