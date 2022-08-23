(** This module contains all bindings.
    Currently, only bindings to functions in wasm.h have been implemented. *)

open Ctypes;;

exception Returned_null of string;;
(** A C function returned NULL where a non-null value was required *)
exception Invalid_access of string;;
(** A pointer was accessed but its ownership realtive to the caller was not
    high enough.
    For example, trying to get a pointer using get_ptr while the pointer was
    given away will raise this exception. *)

(**/**)
type 'a dependent_update_func
(**/**)

type object_state =
  | State_Owned (** The object is owned by the caller. *)
  | State_RW (** The object is accessible by the caller. *)
  | State_Const (** The object is accessible by the caller in read-only. *)
  | State_Dependent of ((unit -> object_state) * object_state dependent_update_func)
    (** The object's state depends on another.
        The first function is to get the current state, the second function
        updates it. *)
  | State_PassedAway;; (** The current pointer has been given away. *);;
(** The state of the ownership of the pointed-to object. *)
val get_real_state: object_state -> object_state;;
(** Gets the real state of the object_state.
    Never returns {!const:WasmerBindings.object_state.State_Dependent}. *)
val max_state_const: object_state -> object_state;;
(** Gets the real state of the object_state, and reduces the ownership to
    read-only if required. This is the same as [get_real_state st] if the
    result is not {!const:WasmerBindings.object_state.State_Owned} or
    {!const:WasmerBindings.object_state.State_RW}, and is
    {!const:WasmerBindings.object_state.State_Const} otherwise. *)

module type ObjectType = sig
  type t
  (** The owned type *)
  type d
  (** Additional data that need to hold for the same length as the object *)
  
  (**/**)
  val make: unit -> t ptr (** Not part of the public API: DO NOT USE *)
  val delete: t ptr -> unit (** Not part of the public API: DO NOT USE *)
  (**/**)
end;;
(** The metadata type for ownable objects *)
module type StructType = sig
  val name: string
  (** The structure base name (without the [wasm_] prefix and [_t] suffix) *)
  type d
  (** Additional data that need to hold for the same length as the structure *)
end;;
(** The metadata type for structure-declaring objects *)
module type VectorType = sig
  type data_type
  val data_type: data_type typ
  
  val name: string
  (** The structure base name (without the [wasm_] prefix and [_vec_t] suffix) *)
  
  type owning_struct
  (** Vectors always take ownership of their elements. *)
  val grab_ownership: owning_struct -> data_type
  (** Take away the ownership. *)
  val to_dependent:
    data_type -> (unit -> object_state) * object_state dependent_update_func ->
    owning_struct
  (** Transform an element of the vector back to a pointer,
      which ownership is dependent on this vector.
      @param obj The element
      @param arg The ownership callbacks
      @return The new owning structure *)
end;;
(** The metadata type for vector structures *)

module OwnableObject(O: ObjectType) : sig
  type s
  (** The owning structure type. *)
  
  val make_new: unit -> s
  (** Make a new owning structure from a new pointer. *)
  val make_from_raise: O.t ptr -> object_state -> s
  (** Make a new owning structure from a pointer.
      See also {!val:WasmerBindings.OwnableObject.make_from_unsafe}.
      @raise Returned_null If the pointer is NULL *)
  val make_from_unsafe: O.t ptr -> object_state -> s
  (** Make a new owning structure from a pointer.
      Does not raise an exception if NULL is given.
      See also {!val:WasmerBindings.OwnableObject.make_from_raise}. *)
  val make_from_raise_data: O.t ptr -> object_state -> O.d -> s
  (** Make a new owning structure from a pointer, with an additional data.
      See also {!val:WasmerBindings.OwnableObject.make_from_unsafe_data}.
      @raise Returned_null If the pointer is NULL *)
  val make_from_unsafe_data: O.t ptr -> object_state -> O.d -> s
  (** Make a new owning structure from a pointer, with an additional data.
      Does not raise an exception if NULL is given.
      See also {!val:WasmerBindings.OwnableObject.make_from_raise_data}. *)
  
  val get_state: s -> object_state
  (** Returns the current ownership state. *)
  val lose_ownership: s -> unit
  (** Sets the current ownership state to
      {!const:WasmerBindings.object_state.State_PassedAway}.
      Ignores the current ownership. *)
  val grab_ownership: s -> O.t ptr
  (** [grab_ownership self] gives the ownership of the pointed object to the
      caller code.
      It is equivalent to [let p = get_ptr self in lose_ownership self; p],
      but with additional checks for the current ownership.
      @return The pointer *)
  val gain_ownership_back: s -> O.t ptr
  (** Deletes the object if required, then sets the ownership to
      {!const:WasmerBindings.object_state.State_Owned} and returns the pointer.
      This is only to be used in own-out function arguments
      in the C API or equivalent.
      
      @return A deleted or given-away pointer.
      @raise Invalid_access If the current state is
      {!const:WasmerBindings.object_state.State_Dependent} *)
  
  val is_null: s -> bool
  (** Returns [Ctypes.is_null] on the underlying pointer, ignoring the current
      ownership state. *)
  val get_ptr: s -> O.t ptr
  (** Returns the underlying pointer, requiring a sufficient
      ownership state.
      @raise Invalid_access If the current state is lower than
      {!const:WasmerBindings.object_state.State_RW}  *)
  val get_ptr_const: s -> O.t ptr
  (** Returns the underlying pointer, requiring a sufficient
      ownership state.
      @raise Invalid_access If the current state is lower than
      {!const:WasmerBindings.object_state.State_Const}  *)
  val get_ptr_givenaway: s -> O.t ptr
  (** Inherently unsafe function that returns a given-away pointer.
      @raise Invalid_access If the current state is not
      {!const:WasmerBindings.object_state.State_PassedAway} *)
  
  val delete: s -> unit
  (** Frees the underlying pointer.
      A side effect is to mark the object as given away. *)
end;;
(** Ownable objects are the basic structure that maintains an ownership state
    on C objects. *)

module DeclareOwn(T: StructType) : sig
  val name: string
  (** The C name of the structure without the [_t] suffix. *)
  type t
  (** The C unique (abstract) base type for the structure.
      the structure type is therefore [t structure]. *)
  val t: t structure typ
  (** The Ctypes definition of the C structure. *)
  
  (**/**)
  module O: ObjectType with type t = t structure
  (**/**)
  include module type of struct include OwnableObject(O) end
  (** This structure maintains an ownership status.
      See also {!module:WasmerBindings.OwnableObject}. *)
end;;
(** The base WASM objects, declared in [wasm.h] as [WASM_DECLARE_OWN]. *)

module DeclareVec(U: VectorType) : sig
  type data_type = U.data_type
  (** The vector's data type *)
  val data_type: data_type typ
  (** The vector's Ctypes data type declaration *)
  
  val name: string
  (** The vector name (without the [_t]) *)
  
  type t
  (** The C unique (abstract) base type for the structure.
      the structure type is therefore [t structure]. *)
  val t: t structure typ
  (** The Ctypes definition of the C structure. *)
  val fsize: (Unsigned.size_t, t structure) field
  (** The raw [size] field of the structure *)
  val fdata: (data_type ptr, t structure) field
  (** The raw [data] field of the structure *)
  
  (**/**)
  module O: ObjectType with type t = t structure
  (**/**)
  include module type of struct include OwnableObject(O) end
  (** This structure maintains an ownership status.
      See also {!module:WasmerBindings.OwnableObject}. *)
  
  val make_empty: unit -> s
  (** Makes a new vector, calling the [wasm_*vec*_new] C API function. *)
  val make_empty_null: unit -> s
  (** Makes a new vector, initializing the data field to NULL. *)
  val make_uninit: int -> s
  (** Makes a new uninitialized vector. *)
  
  val duplicate: s -> s
  (** Duplicates the current vector. No guarantee is made on the deep-ness of
      the copy. *)
  
  val of_array: U.owning_struct array -> s
  (** Makes a vector from an array, taking ownership of the elements. *)
  val of_list: U.owning_struct list -> s
  (** Makes a vector from a list, taking ownership of the elements. *)
  
  val get_size: s -> int
  (** Gets the [size] field of the vector. *)
  val get_element: s -> int -> U.owning_struct
  (** [get_element vec i] gets the [i]th element of [vec] with the correct
      {!const:WasmerBindings.object_state.Dependent} ownership state. *)
  val get_element_unsafe: s -> int -> U.owning_struct
  val get_element_const: s -> int -> U.owning_struct
  (** [get_element vec i] gets the [i]th element of [vec] with the correct
      {!const:WasmerBindings.object_state.Dependent} ownership state. *)
  val get_element_const_unsafe: s -> int -> U.owning_struct
  val set_element: s -> int -> U.owning_struct -> unit
  val set_element_unsafe: s -> int -> U.owning_struct -> unit
end;;
(** The vector type, declared in the C API with the [WASM_DECLARE_VEC] macro.
    Vectors always get ownership of their data. *)

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


(* Forward declaration *)
module Wasi_ : sig
  module Env_O_ : sig
    type _t
    type t = _t structure
    type d = unit
    val make: unit -> t ptr
    val delete: t ptr -> unit
  end
  module Env_ : sig
    include module type of struct include OwnableObject(Env_O_) end
  end
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
  type callback_with_env_t = Wasi_.Env_.s -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  
  (** Callbacks need to be stored somewhere so they do not get GC'd *)
  val new_: Store.s -> Functype.s -> callback_t -> s
  val new_unsafe: Store.s -> Functype.s -> callback_t -> s
  val new_with_env:
    Store.s -> Functype.s -> callback_with_env_t -> Wasi_.Env_.s -> (Wasi_.Env_.s -> unit) option -> s
  val new_with_env_unsafe:
    Store.s -> Functype.s -> callback_with_env_t -> Wasi_.Env_.s -> (Wasi_.Env_.s -> unit) option -> s
  
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

module Wasi : sig
  (* Enabled only if WASMER_WASI_ENABLED was defined at the C API library compile time *)
  module Config_T : sig
    val name: string
  end
  module Config : sig
    val name: string
    
    type t
    val t: t structure typ
    
    type t_bis = t
    module O : sig
      type t = t_bis structure
      type d = unit
      
      val make: unit -> t ptr
      val delete: t ptr -> unit
    end
    include module type of struct include OwnableObject(O) end
    
    val new_: string -> s
    val new_unsafe: string -> s
    (* TODO: all other wasi_config_* functions *)
  end
  
  include module type of struct include Wasi_ end
  module Env_T : sig
    type t = Env_O_._t
    val name: string
  end
  module Env : sig
    type t = Env_T.t
    val t: t structure typ
    include module type of struct include Env_ end
    
    val new_: Store.s -> Config.s -> s
    val new_unsafe: Store.s -> Config.s -> s
    val get_imports: Store.s -> s -> Module.s -> Extern.Vec.s option
  end
end;;
