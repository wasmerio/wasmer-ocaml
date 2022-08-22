open Ctypes;;

exception Returned_null of string;;
exception Invalid_access of string;;

type 'a dependent_update_func

type object_state =
  | State_Owned
  | State_RW
  | State_Const
  | State_Dependent of ((unit -> object_state) * object_state dependent_update_func)
  | State_PassedAway;;
val get_real_state: object_state -> object_state;;
val max_state_const: object_state -> object_state;;

module type ObjectType = sig
  type t
  type d
  
  val make: unit -> t ptr (** Not part of the public API: DO NOT USE *)
  val delete: t ptr -> unit (** Not part of the public API: DO NOT USE *)
end;;
module type StructType = sig
  val name: string
  type d
end;;
module type VectorType = sig
  type data_type
  val data_type: data_type typ
  
  val name: string
  
  type owning_struct
  val grab_ownership: owning_struct -> data_type
  val to_dependent:
    data_type -> (unit -> object_state) * object_state dependent_update_func -> owning_struct
end;;

module OwnableObject(O: ObjectType) : sig
  type s
  
  val make_new: unit -> s
  val make_from_raise: O.t ptr -> object_state -> s
  val make_from_unsafe: O.t ptr -> object_state -> s
  
  val get_state: s -> object_state
  val lose_ownership: s -> unit
  (** Give the ownership of this to the caller
      Equivalent to (let p = get_ptr <s> in lose_ownership <s>; p) *)
  val grab_ownership: s -> O.t ptr
  (** Delete then give a given-away pointer (own-out parameters in the C API) *)
  val gain_ownership_back: s -> O.t ptr
  
  val is_null: s -> bool
  val get_ptr: s -> O.t ptr
  val get_ptr_const: s -> O.t ptr
  (** Inherently unsafe (fails if the pointer is not given away) *)
  val get_ptr_givenaway: s -> O.t ptr
  
  val delete: s -> unit
end;;

module DeclareOwn(T: StructType) : sig
  val name: string
  type t
  val t: t structure typ
  
  module O: ObjectType with type t = t structure
  include module type of struct include OwnableObject(O) end
end;;

module DeclareVec(U: VectorType) : sig
  type data_type = U.data_type
  val data_type: data_type typ
  
  val name: string
  
  type t
  val t: t structure typ
  val fsize: (Unsigned.size_t, t structure) field
  val fdata: (data_type ptr, t structure) field
  
  module O : ObjectType with type t = t structure
  include module type of struct include OwnableObject(O) end
  
  val make_empty: unit -> s
  val make_empty_null: unit -> s (** Initializes the data field to NULL *)
  val make_uninit: int -> s
  (*val new_empty: s -> unit
  val new_empty_null: s -> unit (** Initializes the data field to NULL *)
  val new_uninit: s -> int -> unit*)
  
  val duplicate: s -> s
  
  (** The vector always get ownership of the data *)
  val of_array: U.owning_struct array -> s
  val of_list: U.owning_struct list -> s
  
  val get_size: s -> int
  val get_element: s -> int -> U.owning_struct
  val get_element_unsafe: s -> int -> U.owning_struct
  val get_element_const: s -> int -> U.owning_struct
  val get_element_const_unsafe: s -> int -> U.owning_struct
  val set_element: s -> int -> U.owning_struct -> unit
  val set_element_unsafe: s -> int -> U.owning_struct -> unit
end;;

module DeclareType(T: StructType) : sig
  include module type of struct include DeclareOwn(T) end
  module V : sig
    type data_type = t structure ptr
    val data_type: data_type typ
    
    val name: string
    
    type owning_struct = s
    val grab_ownership: owning_struct -> data_type
    val to_dependent:
      data_type -> (unit -> object_state) * object_state dependent_update_func -> owning_struct
  end
  module Vec: module type of struct include DeclareVec(V) end
  
  val duplicate: s -> s
  val duplicate_unsafe: s -> s
end;;

module DeclareRefBase(T: StructType) : sig
  include module type of struct include DeclareOwn(T) end
  
  val duplicate: s -> s
  val duplicate_unsafe: s -> s
  val same: s -> s -> bool
  val get_host_info: s -> unit ptr
  val set_host_info: s -> unit ptr -> unit
  val set_host_info_with_finalizer: s -> unit ptr -> (unit ptr -> unit) -> unit
  (** Finalizers will not get GC'd (so long as they are called once) *)
end;;

module Ref_T: StructType;;
module Ref: module type of DeclareRefBase(Ref_T);;

module DeclareRef(T: StructType) : sig
  include module type of struct include DeclareRefBase(T) end
  
  val to_ref: s -> Ref.s
  val of_ref: Ref.s -> s
  val to_ref_const: s -> Ref.s
  val of_ref_const: Ref.s -> s
end;;


module Byte : sig
  type byte = Unsigned.uint8
  val byte: byte typ
  module V: VectorType with type data_type = byte with type owning_struct = byte
  module Vec : sig
    include module type of struct include DeclareVec(V) end
    
    val of_char_list: char list -> s
    val of_int_list: int list -> s
    val of_bytes: bytes -> s
    val to_char_list: s -> char list
    val to_int_list: s -> int list
    val to_bytes: s -> bytes
  end
end;;
module Name : sig
  include module type of struct include Byte.Vec end
  
  val of_string: string -> s
  val to_string: s -> string
end;;
module Message : sig
  include module type of struct include Name end
end;;

module Config_T: StructType;;
(** Embedders may provide custom functions for manipulating configs. *)
module Config : sig
  include module type of struct include DeclareOwn(Config_T) end
  
  val new_: unit -> s
  val new_unsafe: unit -> s
end;;

module Engine_T: StructType;;
module Engine : sig
  include module type of struct include DeclareOwn(Engine_T) end
  
  val new_: unit -> s
  val new_unsafe: unit -> s
  
  val new_with_config: Config.s -> s
  val new_with_config_unsafe: Config.s -> s
end;;

module Store_T: StructType;;
module Store : sig
  include module type of struct include DeclareOwn(Store_T) end
  
  val new_: Engine.s -> s
  val new_unsafe: Engine.s -> s
end;;

module Mutability : sig
  type ocaml = Const | Var
  
  type t = Unsigned.uint8
  val t: t typ
  
  val of_c: t -> ocaml
  val to_c: ocaml -> t
end;;

module Limits : sig
  type t
  val t: t structure typ
  val min: (Unsigned.uint32, t structure) field
  val max: (Unsigned.uint32, t structure) field
  
  val max_default: int
end;;

module Valkind : sig
  type ocaml =
    | I32
    | I64
    | F32
    | F64
    | AnyRef
    | FuncRef
  
  type t = Unsigned.uint8
  val t: t typ
  val to_c: ocaml -> t
  val of_c: t -> ocaml
  
  val is_num: ocaml -> bool
  val is_ref: ocaml -> bool
end;;

module Valtype_T: StructType;;
module Valtype : sig
  include module type of struct include DeclareType(Valtype_T) end
  
  val new_: Valkind.ocaml -> s
  val new_unsafe: Valkind.ocaml -> s
  val kind: s -> Valkind.ocaml
  
  val is_num: s -> bool
  val is_ref: s -> bool
end;;

module Functype_T: StructType;;
module Functype : sig
  include module type of struct include DeclareType(Functype_T) end
  
  val new_: Valtype.Vec.s -> Valtype.Vec.s -> s
  val new_unsafe: Valtype.Vec.s -> Valtype.Vec.s -> s
  
  val params: s -> Valtype.Vec.s
  val results: s -> Valtype.Vec.s
end;;

module Globaltype_T: StructType;;
module Globaltype : sig
  include module type of struct include DeclareType(Globaltype_T) end
  
  val new_: Valtype.s -> Mutability.ocaml -> s
  val new_unsafe: Valtype.s -> Mutability.ocaml -> s
  
  val content: s -> Valtype.s
  val mutability: s -> Mutability.ocaml
end;;

module Tabletype_T: StructType;;
module Tabletype : sig
  include module type of struct include DeclareType(Tabletype_T) end
  
  val new_: Valtype.s -> Limits.t structure ptr -> s
  val new_unsafe: Valtype.s -> Limits.t structure ptr -> s
  
  val elements: s -> Valtype.s
  val limits: s -> Limits.t structure ptr
end;;

module Memorytype_T: StructType;;
module Memorytype : sig
  include module type of struct include DeclareType(Memorytype_T) end
  
  val new_: Limits.t structure ptr -> s
  val new_unsafe: Limits.t structure ptr -> s
  
  val limits: s -> Limits.t structure ptr
end;;

module Externkind : sig
  type ocaml =
    | Func
    | Global
    | Table
    | Memory
  
  type t = Unsigned.uint8
  val t: t typ
  
  val of_c: t -> ocaml
  val to_c: ocaml -> t
end;;

module Externtype_T: StructType;;
module Externtype : sig
  include module type of struct include DeclareType(Externtype_T) end
  
  val kind: s -> Externkind.ocaml
  
  val of_functype: Functype.s -> s
  val of_globaltype: Globaltype.s -> s
  val of_tabletype: Tabletype.s -> s
  val of_memorytype: Memorytype.s -> s
  
  val to_functype: s -> Functype.s
  val to_globaltype: s -> Globaltype.s
  val to_tabletype: s -> Tabletype.s
  val to_memorytype: s -> Memorytype.s
  
  val of_functype_const: Functype.s -> s
  val of_globaltype_const: Globaltype.s -> s
  val of_tabletype_const: Tabletype.s -> s
  val of_memorytype_const: Memorytype.s -> s
  
  val to_functype_const: s -> Functype.s
  val to_globaltype_const: s -> Globaltype.s
  val to_tabletype_const: s -> Tabletype.s
  val to_memorytype_const: s -> Memorytype.s
end;;

module Importtype_T: StructType;;
module Importtype : sig
  include module type of struct include DeclareType(Importtype_T) end
  
  val new_: Name.s -> Name.s -> Externtype.s -> s
  val new_unsafe: Name.s -> Name.s -> Externtype.s -> s
  
  val module_: s -> Name.s
  val name: s -> Name.s
  val type_: s -> Externtype.s
end;;

module Exporttype_T: StructType;;
module Exporttype : sig
  include module type of struct include DeclareType(Exporttype_T) end
  
  val new_: Name.s -> Externtype.s -> s
  val new_unsafe: Name.s -> Externtype.s -> s
  
  val name: s -> Name.s
  val type_: s -> Externtype.s
end;;

module Val : sig
  type t
  val t: t structure typ
  val fkind: (Valkind.t, t structure) field
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
  
  module O: ObjectType with type t = t structure
  include module type of struct include OwnableObject(O) end
  
  val of_i32: int32 -> s
  val of_i64: int64 -> s
  val of_f32: float -> s
  val of_f64: float -> s
  val of_ref: Ref.s -> s
  val new_: unit -> s
  
  type ocaml =
    | I32 of int32
    | I64 of int64
    | F32 of float
    | F64 of float
    | Ref of Ref.s
  
  val get_kind: s -> Valkind.ocaml
  
  val get_i32: s -> int32
  val get_i64: s -> int64
  val get_f32: s -> float
  val get_f64: s -> float
  val get_ref: s -> Ref.s
  
  val of_c: s -> ocaml
  val to_c: ocaml -> s
  
  val duplicate: s -> s
  
  module V: VectorType with type data_type = t structure with type owning_struct = s
  module Vec : sig
    include module type of struct include DeclareVec(V) end
    
    val is_compatible: s -> Valtype.Vec.s -> bool
  end
end;;


module DeclareShareableRef(T: StructType) : sig
  include module type of struct include DeclareRef(T) end
  module S: StructType
  module Shared: module type of struct include DeclareOwn(S) end
  
  val to_shared: s -> Shared.s
  val of_shared: Store.s -> Shared.s -> s
end;;


module Frame_T: StructType;;
module Frame : sig
  include module type of struct include DeclareOwn(Frame_T) end
  module V: VectorType with type data_type = t structure ptr with type owning_struct = s
  module Vec: module type of struct include DeclareVec(V) end
  
  val duplicate: s -> s
  val duplicate_unsafe: s -> s
  
  (* Frame.instance -> see frame_instance (cannot be put here as Instance is not defined) *)
  val func_index: s -> int
  val func_offset: s -> int
  val module_offset: s -> int
end;;

module Trap_T: StructType;;
module Trap : sig
  include module type of struct include DeclareRef(Trap_T) end
  
  val new_: Store.s -> Message.s -> s
  val new_unsafe: Store.s -> Message.s -> s
  
  val message: s -> Message.s -> unit
  val origin: s -> Frame.s
  val trace: s -> Frame.Vec.s -> unit
end;;

module Foreign_T: StructType;;
module Foreign : sig
  include module type of struct include DeclareRef(Foreign_T) end
  
  val new_: Store.s -> s
  val new_unsafe: Store.s -> s
end;;

module Module_T: StructType;;
module Module : sig
  include module type of struct include DeclareRefBase(Module_T) end
  
  val validate: Store.s -> Byte.Vec.s -> bool
  val new_: Store.s -> Byte.Vec.s -> s
  val new_unsafe: Store.s -> Byte.Vec.s -> s
  
  val imports: s -> Importtype.Vec.s -> unit
  val exports: s -> Importtype.Vec.s -> unit
  
  val serialize: s -> Byte.Vec.s -> unit
  val deserialize: Store.s -> Byte.Vec.s -> s
  val deserialize_unsafe: Store.s -> Byte.Vec.s -> s
end;;

module Func_T: StructType;;
module Func : sig
  include module type of struct include DeclareType(Func_T) end
  
  type capi_callback_t =
    Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr
  type capi_callback_with_env_t =
    unit ptr -> Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr
  val capi_callback_t: capi_callback_t typ
  val capi_callback_with_env_t: capi_callback_with_env_t typ
  type callback_t = Store.s -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  type callback_with_env_t = unit ptr -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  
  (** Callbacks need to be stored somewhere so they do not get GC'd *)
  val new_: Store.s -> Functype.s -> callback_t -> s
  val new_unsafe: Store.s -> Functype.s -> callback_t -> s
  val new_with_env:
    Store.s -> Functype.s -> callback_with_env_t -> unit ptr -> (unit ptr -> unit) -> s
  val new_with_env_unsafe:
    Store.s -> Functype.s -> callback_with_env_t -> unit ptr -> (unit ptr -> unit) -> s
  
  val type_: s -> Functype.s
  val param_arity: s -> int
  val result_arity: s -> int
  
  val compatible_vectors: s -> Val.Vec.s -> Val.Vec.s -> (unit, bool) result
  val call: s -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  val call_unsafe: s -> Val.Vec.s -> Val.Vec.s -> Trap.s
end;;

module Global_T: StructType;;
module Global : sig
  include module type of struct include DeclareType(Global_T) end
  
  val new_: Store.s -> Globaltype.s -> Val.s -> s
  val new_unsafe: Store.s -> Globaltype.s -> Val.s -> s
  
  val type_: s -> Globaltype.s
  
  val get: s -> Val.s -> unit
  val set: s -> Val.s -> unit
end;;

module Table_T: StructType;;
module Table : sig
  include module type of struct include DeclareType(Table_T) end
  
  type size_t = Unsigned.uint32
  val size_t: size_t typ
  
  val new_: Store.s -> Tabletype.s -> Ref.s -> s
  val new_unsafe: Store.s -> Tabletype.s -> Ref.s -> s
  
  val type_: s -> Tabletype.s
  
  val get: s -> int -> Ref.s
  val set: s -> int -> Ref.s -> bool
  
  val size: s -> int
  val grow: s -> int -> Ref.s -> bool
end;;

module Memory_T: StructType;;
module Memory : sig
  include module type of struct include DeclareType(Memory_T) end
  
  type pages_t = Unsigned.uint32
  val pages_t: pages_t typ
  
  val page_size: int
  
  val new_: Store.s -> Memorytype.s -> s
  val new_unsafe: Store.s -> Memorytype.s -> s
  
  val type_: s -> Memorytype.s
  
  val data: s -> Byte.byte ptr
  val data_size: s -> int
  val get_data: s -> int -> int -> bytes
  val set_data: s -> int -> int -> bytes -> unit
  
  val size: s -> int
  val grow: s -> int -> bool
end;;

module Extern_T: StructType;;
module Extern : sig
  include module type of struct include DeclareType(Extern_T) end
  module V: VectorType with type data_type = t structure ptr with type owning_struct = s
  module Vec : sig
    include module type of struct include DeclareVec(V) end
    
    val split_kind: s -> Func.s list * Global.s list * Table.s list * Memory.s list
    val split_kind_const: s -> Func.s list * Global.s list * Table.s list * Memory.s list
  end
  
  val kind: s -> Externkind.ocaml
  val type_: s -> Externtype.s
  
  val of_func: Func.s -> s
  val of_global: Global.s -> s
  val of_table: Table.s -> s
  val of_memory: Memory.s -> s
  
  val to_func: s -> Func.s
  val to_global: s -> Global.s
  val to_table: s -> Table.s
  val to_memory: s -> Memory.s
  
  val of_func_const: Func.s -> s
  val of_global_const: Global.s -> s
  val of_table_const: Table.s -> s
  val of_memory_const: Memory.s -> s
  
  val to_func_const: s -> Func.s
  val to_global_const: s -> Global.s
  val to_table_const: s -> Table.s
  val to_memory_const: s -> Memory.s
end;;

module Instance_T: StructType;;
module Instance : sig
  include module type of struct include DeclareRef(Instance_T) end
  
  val new_: Store.s -> Module.s -> Extern.Vec.s -> (s, Trap.s option) result
  val new_unsafe: Store.s -> Module.s -> Extern.Vec.s -> (s, Trap.s) result
  
  val exports: s -> Extern.Vec.s -> unit
end;;
val frame_instance: Frame.s -> Instance.s;;
