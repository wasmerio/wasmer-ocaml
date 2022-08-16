open Ctypes;;
open Foreign;;

let () =
  foreign "assertions" (void @-> returning void) ();;

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

module DeclareStruct(T: StructType) : DECLARE_STRUCT = struct
  let name = "wasm_" ^ T.name
  type t
  let t = structure name
end;;

module type DECLARE_OWN = sig
  include DECLARE_STRUCT
  
  val delete: t structure ptr -> unit
end;;

module DeclareOwn(T: StructType) : DECLARE_OWN = struct
  include DeclareStruct(T)
  
  let delete = foreign ?stub:(Some true) (name ^ "_delete") (ptr t @-> returning void)
end;;

module DECLARE_VEC(U: VectorType) = struct
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
    val make_empty_null: unit -> t structure ptr
    val make_uninit: int -> t structure ptr
    val of_carray: data_type carray -> t structure ptr
    val of_list: data_type list -> t structure ptr
    val duplicate: t structure ptr -> t structure ptr
  end
end;;

module DeclareVec(T: VectorType) : DECLARE_VEC(T).T = struct
  type data_type = T.data_type
  let data_type = T.data_type
  
  let name = "wasm_" ^ T.name ^ "_vec"
  type t
  let t: t structure typ = structure (name ^ "_t")
  let fsize = field t "size" size_t
  let fdata = field t "data" (ptr data_type)
  let () = seal t
  
  let make () = allocate_n t ~count:1
  let new_empty = foreign ?stub:(Some true) (name ^ "_new_empty") (ptr t @-> returning void)
  let new_uninitialized =
    foreign ?stub:(Some true) (name ^ "_new_uninitialized") (ptr t @-> size_t @-> returning void)
  let new_ = foreign ?stub:(Some true) (name ^ "_new") (ptr t @-> size_t @-> ptr data_type @-> returning void)
  let new_carray self data =
    new_ self (Unsigned.Size_t.of_int (CArray.length data)) (CArray.start data)
  let copy = foreign ?stub:(Some true) (name ^ "_copy") (ptr t @-> ptr t @-> returning void)
  let delete = foreign ?stub:(Some true) (name ^ "_delete") (ptr t @-> returning void)
  
  let make_empty () = let ret = make () in new_empty ret; ret
  let make_empty_null () =
    let ret = make () in
    ret |-> fsize <-@ Unsigned.Size_t.of_int 0;
    ret |-> fdata <-@ from_voidp data_type null;
    ret
  let make_uninit n =
    let ret = make () in new_uninitialized ret (Unsigned.Size_t.of_int n); ret
  let of_carray a =
    let ret = make () in
    ret |-> fsize <-@ Unsigned.Size_t.of_int (CArray.length a);
    ret |-> fdata <-@ CArray.start a;
    ret
  let of_list l = of_carray (CArray.of_list data_type l)
  let duplicate self =
    let cop = make () in
    copy cop self;
    cop
end;;

module type DECLARE_TYPE = sig
  include DECLARE_OWN
  module V : sig
    type data_type = t structure ptr
    val data_type: data_type typ
    val name: string
  end
  module Vec : DECLARE_VEC(V).T
  
  val duplicate: t structure ptr -> t structure ptr
end;;

module DeclareType(T: StructType) : DECLARE_TYPE = struct
  include DeclareOwn(T)
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    let name = T.name
  end
  module Vec = DeclareVec(V)
  
  let duplicate = foreign ?stub:(Some true) (name ^ "_copy") (ptr t @-> returning (ptr t))
end;;

module type DECLARE_REF_BASE = sig
  include DECLARE_OWN
  
  val duplicate: t structure ptr -> t structure ptr
  val same: t structure ptr -> t structure ptr -> bool
  val get_host_info: t structure ptr -> unit ptr
  val set_host_info: t structure ptr -> unit ptr -> unit
  val set_host_info_with_finalizer: t structure ptr -> unit ptr -> (unit ptr -> unit) -> unit
end;;

module DeclareRefBase(T: StructType) : DECLARE_REF_BASE = struct
  include DeclareOwn(T)
  
  let duplicate =
    foreign ?stub:(Some true) (name ^ "_copy") (ptr t @-> returning (ptr t))
  let same =
    foreign ?stub:(Some true) (name ^ "_same") (ptr t @-> ptr t @-> returning bool)
  let get_host_info =
    foreign ?stub:(Some true) (name ^ "_get_host_info") (ptr t @-> returning (ptr void))
  let set_host_info =
    foreign ?stub:(Some true) (name ^ "_set_host_info") (ptr t @-> ptr void @-> returning void)
  let set_host_info_with_finalizer =
    let mem = ref [] in (* Finalizers may be GC'd if not stored here *)
    let idx = ref 0 in
    let f = foreign ?stub:(Some true)
      (name ^ "_set_host_info_with_finalizer")
      (ptr t @-> ptr void @-> (funptr ~thread_registration:true (ptr void @-> returning void)) @-> returning void) in
    fun r d cb -> (* Make sure the callback gets GC'd *)
      let i = !idx in
      let real_callback data =
        cb data;
        let rec remove_i l acc = match l with
        | [] -> acc
        | (j, v) :: tl ->
          if j = i then List.rev_append tl acc
          else remove_i tl ((j, v) :: acc) in
        mem := remove_i !mem [] in
      mem := (i, real_callback) :: !mem;
      f r d real_callback
end;;

module Ref_T = struct let name = "ref" end;;
module Ref = DeclareStruct(Ref_T);; (* .h indicates REF_BASE but no symbol is present *)

module type DECLARE_REF = sig
  include DECLARE_REF_BASE
  
  val to_ref: t structure ptr -> Ref.t structure ptr
  val of_ref: Ref.t structure ptr -> t structure ptr
  val to_ref_const: t structure ptr -> Ref.t structure ptr
  val of_ref_const: Ref.t structure ptr -> t structure ptr
end;;

module DeclareRef(T: StructType) : DECLARE_REF = struct
  include DeclareRefBase(T)
  
  let to_ref =
    foreign ?stub:(Some true) (name ^ "_as_ref") (ptr t @-> returning (ptr Ref.t))
  let of_ref =
    foreign ?stub:(Some true) ("wasm_ref_as_" ^ T.name) (ptr Ref.t @-> returning (ptr t))
  let to_ref_const =
    foreign ?stub:(Some true) (name ^ "_as_ref_const") (ptr t @-> returning (ptr Ref.t))
  let of_ref_const =
    foreign ?stub:(Some true) ("wasm_ref_as_" ^ T.name ^ "_const") (ptr Ref.t @-> returning (ptr t))
end;;


module Byte = struct
  type byte = Unsigned.uint8
  let byte = uint8_t
  module V = struct
    type data_type = byte
    let data_type = byte
    let name = "byte"
  end
  module Vec = struct
    include DeclareVec(V)
    
    let of_char_list l = of_list (List.map (fun i -> Unsigned.UInt8.of_int (Char.code i)) l)
    let of_int_list l = of_list (List.map (fun i -> Unsigned.UInt8.of_int i) l)
    let of_bytes b = of_char_list (List.of_seq (Bytes.to_seq b))
  end
end;;
module Name = struct
  include Byte.Vec
  
  let of_string s = of_bytes (Bytes.of_string s)
end;;
module Message = struct
  include Name
end;;

module Config_T = struct
  let name = "config"
end;;
(** Embedders may provide custom functions for manipulating configs. *)
module Config = struct
  include DeclareOwn(Config_T)
  
  let new_ = foreign ?stub:(Some true) "wasm_config_new" (void @-> returning (ptr t))
end;;

module Engine_T = struct
  let name = "engine"
end;;
module Engine = struct
  include DeclareOwn(Engine_T)
  
  let new_ = foreign ?stub:(Some true) "wasm_engine_new" (void @-> returning (ptr t))
  let new_with_config =
    foreign ?stub:(Some true) "wasm_engine_new_with_config" (ptr Config.t @-> returning (ptr t))
end;;

module Store_T = struct
  let name = "store"
end;;
module Store = struct
  include DeclareOwn(Store_T)
  
  let new_ = foreign ?stub:(Some true) "wasm_store_new" (ptr Engine.t @-> returning (ptr t))
end;;

module Mutability = struct
  type t = Unsigned.uint8
  let t = typedef uint8_t "wasm_mutability_t"
end;;
module Mutability_e = struct
  let const = 0
  let var = 1
end;;

module Limits = struct
  type t
  let t: t structure typ = structure "wasm_limits_t"
  let min = field t "min" uint32_t
  let max = field t "max" uint32_t
  let () = seal t
  
  let max_default = 0xFFFFFFFF (* limits_max_default *)
end;;

module Valkind = struct
  type valkind_OCaml =
    | ValKind_i32
    | ValKind_i64
    | ValKind_f32
    | ValKind_f64
    | ValKind_AnyRef
    | ValKind_FuncRef
  
  type valkind_C = Unsigned.uint8
  let valkind_C = uint8_t
  let valkind_C_of_OCaml vk: valkind_C = Unsigned.UInt8.of_int (match vk with
    | ValKind_i32 -> 0
    | ValKind_i64 -> 1
    | ValKind_f32 -> 2
    | ValKind_f64 -> 3
    | ValKind_AnyRef -> 128
    | ValKind_FuncRef -> 129)
  let valkind_OCaml_of_C (vk: valkind_C) = match Unsigned.UInt8.to_int vk with
    | 0 -> ValKind_i32
    | 1 -> ValKind_i64
    | 2 -> ValKind_f32
    | 3 -> ValKind_f64
    | 128 -> ValKind_AnyRef
    | 129 -> ValKind_FuncRef
    | i -> failwith ("Invalid C valkind " ^ (string_of_int i))
end;;

module Valtype_T = struct
  let name = "valtype"
end;;
module Valtype = struct
  include DeclareType(Valtype_T)
  
  let new_ =
    let f = foreign ?stub:(Some true) "wasm_valtype_new" (Valkind.valkind_C @-> returning (ptr t)) in
    fun v -> f (Valkind.valkind_C_of_OCaml v)
  let kind =
    let f = foreign ?stub:(Some true) "wasm_valtype_kind" (ptr t @-> returning Valkind.valkind_C) in
    fun v -> Valkind.valkind_OCaml_of_C (f v)
end;;

module Functype_T = struct
  let name = "functype"
end;;
module Functype = struct
  include DeclareType(Functype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_functype_new"
      (ptr Valtype.Vec.t @-> ptr Valtype.Vec.t @-> returning (ptr t))
  let params =
    foreign ?stub:(Some true) "wasm_functype_params" (ptr t @-> returning (ptr Valtype.Vec.t))
  let results =
    foreign ?stub:(Some true) "wasm_functype_results" (ptr t @-> returning (ptr Valtype.Vec.t))
end;;

module Globaltype_T = struct
  let name = "globaltype"
end;;
module Globaltype = struct
  include DeclareType(Globaltype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_globaltype_new"
      (ptr Valtype.t @-> ptr Mutability.t @-> returning (ptr t))
  let content =
    foreign ?stub:(Some true) "wasm_globaltype_content" (ptr t @-> returning (ptr Valtype.t))
  let mutability =
    foreign ?stub:(Some true) "wasm_globaltype_mutability" (ptr t @-> returning Mutability.t)
end;;

module Tabletype_T = struct
  let name = "tabletype"
end;;
module Tabletype = struct
  include DeclareType(Tabletype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_tabletype_new"
      (ptr Valtype.t @-> ptr Limits.t @-> returning (ptr t))
  let elements =
    foreign ?stub:(Some true) "wasm_tabletype_elements" (ptr t @-> returning (ptr Valtype.t))
  let limits =
    foreign ?stub:(Some true) "wasm_tabletype_limits" (ptr t @-> returning (ptr Limits.t))
end;;

module Memorytype_T = struct
  let name = "memorytype"
end;;
module Memorytype = struct
  include DeclareType(Memorytype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_memorytype_new"
      (ptr Limits.t @-> returning (ptr t))
  let limits =
    foreign ?stub:(Some true) "wasm_memorytype_limits" (ptr t @-> returning (ptr Limits.t))
end;;

module Externkind = struct
  type externkind_OCaml =
    | ExternKind_Func
    | ExternKind_Global
    | ExternKind_Table
    | ExternKind_Memory
  
  type externkind_C = Unsigned.uint8
  let externkind_C = uint8_t
  let externkind_C_of_OCaml vk: externkind_C = Unsigned.UInt8.of_int (match vk with
    | ExternKind_Func -> 0
    | ExternKind_Global -> 1
    | ExternKind_Table -> 2
    | ExternKind_Memory -> 3)
  let externkind_OCaml_of_C (vk: externkind_C) = match Unsigned.UInt8.to_int vk with
    | 0 -> ExternKind_Func
    | 1 -> ExternKind_Global
    | 2 -> ExternKind_Table
    | 3 -> ExternKind_Memory
    | i -> failwith ("Invalid C externkind " ^ (string_of_int i))
end;;

module Externtype_T = struct
  let name = "externtype"
end;;
module Externtype = struct
  include DeclareType(Externtype_T)
  
  let kind =
    let f =
      foreign ?stub:(Some true)
        "wasm_externtype_kind"
        (ptr t @-> returning Externkind.externkind_C) in
    fun v -> Externkind.externkind_OCaml_of_C (f v)
  
  let of_functype =
    foreign ?stub:(Some true) "wasm_functype_as_externtype" (ptr Functype.t @-> returning (ptr t))
  let of_globaltype =
    foreign ?stub:(Some true) "wasm_globaltype_as_externtype" (ptr Globaltype.t @-> returning (ptr t))
  let of_tabletype =
    foreign ?stub:(Some true) "wasm_tabletype_as_externtype" (ptr Tabletype.t @-> returning (ptr t))
  let of_memorytype =
    foreign ?stub:(Some true) "wasm_memorytype_as_externtype" (ptr Memorytype.t @-> returning (ptr t))
  
  
  let to_functype =
    foreign ?stub:(Some true) "wasm_functype_as_externtype" (ptr t @-> returning (ptr Functype.t))
  let to_globaltype =
    foreign ?stub:(Some true) "wasm_globaltype_as_externtype" (ptr t @-> returning (ptr Globaltype.t))
  let to_tabletype =
    foreign ?stub:(Some true) "wasm_tabletype_as_externtype" (ptr t @-> returning (ptr Tabletype.t))
  let to_memorytype =
    foreign ?stub:(Some true) "wasm_memorytype_as_externtype" (ptr t @-> returning (ptr Memorytype.t))
  
  let of_functype_const =
    foreign ?stub:(Some true) "wasm_functype_as_externtype_const" (ptr Functype.t @-> returning (ptr t))
  let of_globaltype_const =
    foreign ?stub:(Some true) "wasm_globaltype_as_externtype_const" (ptr Globaltype.t @-> returning (ptr t))
  let of_tabletype_const =
    foreign ?stub:(Some true) "wasm_tabletype_as_externtype_const" (ptr Tabletype.t @-> returning (ptr t))
  let of_memorytype_const =
    foreign ?stub:(Some true) "wasm_memorytype_as_externtype_const" (ptr Memorytype.t @-> returning (ptr t))
  
  
  let to_functype_const =
    foreign ?stub:(Some true) "wasm_functype_as_externtype_const" (ptr t @-> returning (ptr Functype.t))
  let to_globaltype_const =
    foreign ?stub:(Some true) "wasm_globaltype_as_externtype_const" (ptr t @-> returning (ptr Globaltype.t))
  let to_tabletype_const =
    foreign ?stub:(Some true) "wasm_tabletype_as_externtype_const" (ptr t @-> returning (ptr Tabletype.t))
  let to_memorytype_const =
    foreign ?stub:(Some true) "wasm_memorytype_as_externtype_const" (ptr t @-> returning (ptr Memorytype.t))
end;;

module Importtype_T = struct
  let name = "importtype"
end;;
module Importtype = struct
  include DeclareType(Importtype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_importtype_new"
      (ptr Name.t @-> ptr Name.t @-> ptr Externtype.t @-> returning (ptr t))
  let module_ =
    foreign ?stub:(Some true) "wasm_importtype_module" (ptr t @-> returning (ptr Name.t))
  let name =
    foreign ?stub:(Some true) "wasm_importtype_name" (ptr t @-> returning (ptr Name.t))
  let type_ =
    foreign ?stub:(Some true) "wasm_importtype_type" (ptr t @-> returning (ptr Externtype.t))
end;;

module Exporttype_T = struct
  let name = "exporttype"
end;;
module Exporttype = struct
  include DeclareType(Exporttype_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_exporttype_new"
      (ptr Name.t @-> ptr Externtype.t @-> returning (ptr t))
  let name_ =
    foreign ?stub:(Some true) "wasm_exporttype_name" (ptr t @-> returning (ptr Name.t))
  let type_ =
    foreign ?stub:(Some true) "wasm_exporttype_type" (ptr t @-> returning (ptr Externtype.t))
end;;

module Val = struct
  type t
  let t: t structure typ = structure "wasm_val_t"
  let fkind = field t "kind" Valkind.valkind_C
  module Anon0 = struct
    type t
    let t: t union typ = union "wasm_val_t.<union0>"
    let fi32 = field t "i32" int32_t
    let fi64 = field t "i64" int64_t
    let ff32 = field t "f32" float
    let ff64 = field t "f64" double
    let fref = field t "ref" (ptr Ref.t)
    let () = seal t
  end
  let fof = field t "of" Anon0.t
  let () = seal t
  
  let of_i32 i32 =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_i32);
    let tmp = make Anon0.t in
    setf tmp Anon0.fi32 i32;
    setf ret fof tmp;
    ret
  let of_i64 i64 =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_i64);
    let tmp = make Anon0.t in
    setf tmp Anon0.fi64 i64;
    setf ret fof tmp;
    ret
  let of_f32 f32 =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_f32);
    let tmp = make Anon0.t in
    setf tmp Anon0.ff32 f32;
    setf ret fof tmp;
    ret
  let of_f64 f64 =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_f64);
    let tmp = make Anon0.t in
    setf tmp Anon0.ff64 f64;
    setf ret fof tmp;
    ret
  let of_ref ref =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_AnyRef);
    let tmp = make Anon0.t in
    setf tmp Anon0.fref ref;
    setf ret fof tmp;
    ret
  let new_ () =
    let ret = make t in
    setf ret fkind (Valkind.valkind_C_of_OCaml Valkind.ValKind_AnyRef);
    let tmp = make Anon0.t in
    setf tmp Anon0.fref (from_voidp Ref.t null);
    setf ret fof tmp;
    ret
  
  let copy =
    foreign ?stub:(Some true) "wasm_val_copy" (ptr t @-> ptr t @-> returning void)
  let delete =
    foreign ?stub:(Some true) "wasm_val_delete" (ptr t @-> returning void)
  
  let duplicate self =
    let cop = new_ () in
    copy (addr cop) self;
    cop
  
  module V = struct
    type data_type = t structure
    let data_type = t
    let name = "val"
  end
  module Vec = DeclareVec(V)
end;;


module type DECLARE_SHAREABLE_REF = sig
  include DECLARE_REF
  module S : sig
    val name: string
  end
  module Shared : DECLARE_OWN 
  
  val to_shared: t structure ptr -> Shared.t structure ptr
  val of_shared: Store.t structure ptr -> Shared.t structure ptr -> t structure ptr
end;;

module DeclareShareableRef(T: StructType) : DECLARE_SHAREABLE_REF = struct
  include DeclareRef(T)
  module S = struct
    let name = "shared_" ^ T.name
  end
  module Shared = DeclareOwn(S)
  
  let to_shared =
    foreign ?stub:(Some true) (name ^ "_share") (ptr t @-> returning (ptr Shared.t))
  let of_shared =
    foreign ?stub:(Some true) (name ^ "_obtain") (ptr Store.t @-> ptr Shared.t @-> returning (ptr t))
end;;


module Frame_T = struct
  let name = "frame"
end
module Frame = struct
  include DeclareOwn(Frame_T)
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    let name = Frame_T.name
  end
  module Vec = DeclareVec(V)
  let duplicate = foreign ?stub:(Some true) (name ^ "_copy") (ptr t @-> returning (ptr t))
  
  let instance = foreign ?stub:(Some true) (name ^ "_instance") (ptr t @-> returning (ptr void))
  let func_index = foreign ?stub:(Some true) (name ^ "_func_index") (ptr t @-> returning uint32_t)
  let func_offset = foreign ?stub:(Some true) (name ^ "_func_offset") (ptr t @-> returning size_t)
  let module_offset = foreign ?stub:(Some true) (name ^ "_module_offset") (ptr t @-> returning size_t)
end;;

module Trap_T = struct
  let name = "trap"
end;;
module Trap = struct
  include DeclareRef(Trap_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_trap_new"
      (ptr Store.t @-> ptr Message.t @-> returning (ptr t))
  
  let message =
    foreign ?stub:(Some true)
      "wasm_trap_message"
      (ptr t @-> ptr Message.t @-> returning void)
  let origin =
    foreign ?stub:(Some true)
      "wasm_trap_origin"
      (ptr t @-> returning (ptr Frame.t))
  let trace =
    foreign ?stub:(Some true)
      "wasm_trap_trace"
      (ptr t @-> ptr Frame.Vec.t @-> returning void)
end;;

module Foreign_T = struct
  let name = "foreign"
end;;
module Foreign = struct
  include DeclareRef(Foreign_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_foreign_new"
      (ptr Store.t @-> returning (ptr t))
end;;

module Module_T = struct
  let name = "module"
end;;
module Module = struct
  include DeclareShareableRef(Module_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_module_new"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning (ptr t))
  
  let validate =
    foreign ?stub:(Some true)
      "wasm_module_validate"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning bool)
  
  let imports =
    foreign ?stub:(Some true)
      "wasm_module_imports"
      (ptr t @-> ptr Importtype.Vec.t @-> returning void)
  let exports =
    foreign ?stub:(Some true)
      "wasm_module_exports"
      (ptr t @-> ptr Importtype.Vec.t @-> returning void)
  
  let serialize =
    foreign ?stub:(Some true)
      "wasm_module_serialize"
      (ptr t @-> ptr Byte.Vec.t @-> returning void)
  let deserialize =
    foreign ?stub:(Some true)
      "wasm_module_deserialize"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning (ptr t))
end;;

module Func_T = struct
  let name = "func"
end;;
module Func = struct
  include DeclareType(Func_T)
  
  type callback_t =
    Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr
  type callback_with_env_t =
    unit ptr -> Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr
  let callback_t =
    typedef
      (funptr ~thread_registration:true (ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t)))
      "wasm_func_callback_t"
  let callback_with_env_t =
    typedef
      (funptr ~thread_registration:true (ptr void @-> ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t)))
      "wasm_func_callback_with_env_t"
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_func_new"
      (ptr Store.t @-> ptr Functype.t @-> callback_t @-> returning (ptr t))
  let new_with_env =
    foreign ?stub:(Some true)
      "wasm_func_new_with_env"
      (ptr Store.t @-> ptr Functype.t @-> callback_with_env_t @-> ptr void @->
        (funptr ~thread_registration:true (ptr void @-> returning void)) @->
        returning (ptr t))
  
  let type_ =
    foreign ?stub:(Some true) "wasm_func_type" (ptr t @-> returning (ptr Functype.t))
  let param_arity =
    foreign ?stub:(Some true) "wasm_func_param_arity" (ptr t @-> returning size_t)
  let result_arity =
    foreign ?stub:(Some true) "wasm_func_result_arity" (ptr t @-> returning size_t)
  
  let call =
    foreign ?stub:(Some true)
      "wasm_func_call"
      (ptr t @-> ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t))
end;;

module Global_T = struct
  let name = "global"
end;;
module Global = struct
  include DeclareType(Global_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_global_new"
      (ptr Store.t @-> ptr Globaltype.t @-> ptr Val.t @-> returning (ptr t))
  
  let type_ =
    foreign ?stub:(Some true) "wasm_global_type" (ptr t @-> returning (ptr Globaltype.t))
  
  let get =
    foreign ?stub:(Some true) "wasm_global_get" (ptr t @-> ptr Val.t @-> returning void)
  let set =
    foreign ?stub:(Some true) "wasm_global_set" (ptr t @-> ptr Val.t @-> returning void)
end;;

module Table_T = struct
  let name = "table"
end;;
module Table = struct
  include DeclareType(Table_T)
  
  type size_t = Unsigned.uint32
  let size_t = typedef uint32_t "wasm_table_size_t"
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_table_new"
      (ptr Store.t @-> ptr Tabletype.t @-> ptr Ref.t @-> returning (ptr t))
  
  let type_ =
    foreign ?stub:(Some true) "wasm_table_type" (ptr t @-> returning (ptr Tabletype.t))
  
  let get =
    foreign ?stub:(Some true) "wasm_table_get" (ptr t @-> size_t @-> returning (ptr Ref.t))
  let set =
    foreign ?stub:(Some true)
      "wasm_table_set"
      (ptr t @-> size_t @-> ptr Ref.t @-> returning bool)
  
  let size =
    foreign ?stub:(Some true) "wasm_table_size" (ptr t @-> returning size_t)
  let grow =
    foreign ?stub:(Some true)
      "wasm_table_grow"
      (ptr t @-> size_t @-> ptr Ref.t @-> returning bool)
end;;

module Memory_T = struct
  let name = "memory"
end;;
module Memory = struct
  include DeclareType(Memory_T)
  
  type pages_t = Unsigned.uint32
  let pages_t = typedef uint32_t "wasm_table_pages_t"
  
  let page_size = 0x10000 (* MEMORY_PAGE_SIZE *)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_memoy_new"
      (ptr Store.t @-> ptr Memorytype.t @-> returning (ptr t))
  
  let type_ =
    foreign ?stub:(Some true) "wasm_memoy_type" (ptr t @-> returning (ptr Memorytype.t))
  
  let data =
    foreign ?stub:(Some true) "wasm_memoy_data" (ptr t @-> returning (ptr Byte.byte))
  let data_size =
    foreign ?stub:(Some true) "wasm_memoy_data_size" (ptr t @-> returning size_t)
  
  let size =
    foreign ?stub:(Some true) "wasm_memoy_size" (ptr t @-> returning pages_t)
  let grow =
    foreign ?stub:(Some true)
      "wasm_memoy_grow"
      (ptr t @-> pages_t @-> returning bool)
end;;

module Extern_T = struct
  let name = "extern"
end;;
module Extern = struct
  include DeclareType(Extern_T)
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    let name = Extern_T.name
  end
  module Vec = DeclareVec(V)
  
  let kind =
    let f =
      foreign ?stub:(Some true)
        "wasm_extern_kind"
        (ptr t @-> returning Externkind.externkind_C) in
    fun v -> Externkind.externkind_OCaml_of_C (f v)
  let type_ =
    foreign ?stub:(Some true)
      "wasm_extern_type"
      (ptr t @-> returning (ptr Externtype.t))
  
  let of_func =
    foreign ?stub:(Some true) "wasm_func_as_extern" (ptr Func.t @-> returning (ptr t))
  let of_global =
    foreign ?stub:(Some true) "wasm_global_as_extern" (ptr Global.t @-> returning (ptr t))
  let of_table =
    foreign ?stub:(Some true) "wasm_table_as_extern" (ptr Table.t @-> returning (ptr t))
  let of_memory =
    foreign ?stub:(Some true) "wasm_memory_as_extern" (ptr Memory.t @-> returning (ptr t))
  
  
  let to_func =
    foreign ?stub:(Some true) "wasm_func_as_extern" (ptr t @-> returning (ptr Func.t))
  let to_global =
    foreign ?stub:(Some true) "wasm_global_as_extern" (ptr t @-> returning (ptr Global.t))
  let to_table =
    foreign ?stub:(Some true) "wasm_table_as_extern" (ptr t @-> returning (ptr Table.t))
  let to_memory =
    foreign ?stub:(Some true) "wasm_memory_as_extern" (ptr t @-> returning (ptr Memory.t))
  
  let of_func_const =
    foreign ?stub:(Some true) "wasm_func_as_extern_const" (ptr Func.t @-> returning (ptr t))
  let of_global_const =
    foreign ?stub:(Some true) "wasm_global_as_extern_const" (ptr Global.t @-> returning (ptr t))
  let of_table_const =
    foreign ?stub:(Some true) "wasm_table_as_extern_const" (ptr Table.t @-> returning (ptr t))
  let of_memory_const =
    foreign ?stub:(Some true) "wasm_memory_as_extern_const" (ptr Memory.t @-> returning (ptr t))
  
  
  let to_func_const =
    foreign ?stub:(Some true) "wasm_func_as_extern_const" (ptr t @-> returning (ptr Func.t))
  let to_global_const =
    foreign ?stub:(Some true) "wasm_global_as_extern_const" (ptr t @-> returning (ptr Global.t))
  let to_table_const =
    foreign ?stub:(Some true) "wasm_table_as_extern_const" (ptr t @-> returning (ptr Table.t))
  let to_memory_const =
    foreign ?stub:(Some true) "wasm_memory_as_extern_const" (ptr t @-> returning (ptr Memory.t))
end;;

module Instance_T = struct
  let name = "instance"
end;;
module Instance = struct
  include DeclareRef(Instance_T)
  
  let new_ =
    foreign ?stub:(Some true)
      "wasm_instance_new"
      (ptr Store.t @-> ptr Module.t @-> ptr Extern.Vec.t @-> ptr (ptr Trap.t)
       @-> returning (ptr t))
  
  let exports =
    foreign ?stub:(Some true)
      "wasm_instance_exports"
      (ptr t @-> ptr Extern.Vec.t @-> returning void)
end;;
let frame_instance f =
  from_voidp Instance.t (to_voidp (Frame.instance f));;
