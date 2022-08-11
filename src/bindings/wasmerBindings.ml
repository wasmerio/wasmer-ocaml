open Ctypes;;
open Foreign;;

module VECTOR(U: sig type data
                     val data: data typ
                     val name: string
                     
                     type coerce_from
                     val coerce: coerce_from -> data end) = struct
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
                     val coerce: coerce_from -> data end) : VECTOR(T).T =
struct
  type data = T.data
  type t = data structure
  let data = T.data
  let name = T.name ^ "_vec"
  let t: t typ = structure (name ^ "_t")
  let tsize = field t "size" size_t
  let tdata = field t "data" (ptr data)
  let () = seal t

  let make () = Ctypes.allocate_n t ~count:1
  let new_empty =
    foreign
      (name ^ "_new_empty")
      (ptr t @-> returning void)
  let new_uninitialized =
    foreign
      (name ^ "_new_uninitialized")
      (ptr t @-> size_t @-> returning void)
  let copy =
    foreign
      (name ^ "_copy")
      (ptr t @-> ptr t @-> returning void)
  let delete = foreign (name ^ "_delete") (ptr t @-> returning void)
  
  let create_empty () = let ret = make () in new_empty ret; ret
  let create_uninit n =
    let ret = make () in new_uninitialized ret (Unsigned.Size_t.of_int n); ret
  let of_array a =
    let ret = create_uninit (Array.length a) in
    for i = 0 to (Array.length a) - 1 do
      ((!@ (ret |-> tdata)) +@ i) <-@ (T.coerce a.(i))
    done;
    ret
  let of_list l = of_array (Array.of_list l)
  let duplicate self =
    let cop = make () in
    copy cop self;
    cop
end;;

type engine (*= [`Engine] structure *);;
let engine: engine typ = structure "wasm_engine_t";;
let engine_new = foreign "wasm_engine_new" (void @-> returning (ptr engine));;
let engine_delete = foreign "wasm_engine_delete" (ptr engine @-> returning void);;

type store (*= [`Store] structure *);;
let store: store typ = structure "wasm_store_t";;
let store_new = foreign "wasm_store_new" (ptr engine @-> returning (ptr store));;
let store_delete = foreign "wasm_store_delete" (ptr store @-> returning void);;

type byte = Unsigned.uint8;;
let byte = uint8_t;;
module Byte_vec_chara = struct
  type data = byte
  let data = byte
  let name = "wasm_byte"
  
  type coerce_from = char
  let coerce v = Unsigned.UInt8.of_int (Char.code v);;
end;;
module Byte_vec = Vector(Byte_vec_chara);;

type module_ (*= [`Module] structure *);;
let module_: module_ typ = structure "wasm_module_t";;
let module_size = field module_ "size" size_t;;
let () = seal module_;;

let module_new =
  foreign
    "wasm_module_new"
    (ptr store @-> ptr Byte_vec.t @-> returning (ptr module_));;
let module_validate =
  foreign
    "wasm_module_validate"
    (ptr store @-> ptr Byte_vec.t @-> returning bool);;
let module_delete = foreign "wasm_module_delete" (ptr module_ @-> returning void);;

(* let module_imports =
  foreign
    "wasm_module_imports"
    (ptr module_ @-> ptr ImportType_vec.t @-> returning void);;
let module_exports =
  foreign
    "wasm_module_exports"
    (ptr module_ @-> ptr ExportType_vec.t @-> returning void);; *)

let module_serialize =
  foreign
    "wasm_module_serialize"
    (ptr module_ @-> ptr Byte_vec.t @-> returning void);;
let module_deserialize =
  foreign
    "wasm_module_deserialize"
    (ptr store @-> ptr Byte_vec.t @-> returning (ptr module_));;

(* Valtype *)
type valkind =
  | ValKind_i32
  | ValKind_i64
  | ValKind_f32
  | ValKind_f64
  | ValKind_AnyRef
  | ValKind_FuncRef;;
let valkind = uint8_t;;
let valkind_C_of_OCaml vk = Unsigned.UInt8.of_int (match vk with
  | ValKind_i32 -> 0
  | ValKind_i64 -> 1
  | ValKind_f32 -> 2
  | ValKind_f64 -> 3
  | ValKind_AnyRef -> 128
  | ValKind_FuncRef -> 129);;
let valkind_OCaml_of_C vk = match Unsigned.UInt8.to_int vk with
  | 0 -> ValKind_i32
  | 1 -> ValKind_i64
  | 2 -> ValKind_f32
  | 3 -> ValKind_f64
  | 128 -> ValKind_AnyRef
  | 129 -> ValKind_FuncRef
  | i -> failwith ("Invalid integer valkind " ^ (string_of_int i));;

type valtype (*= [`Valtype] structure *);;
let valtype: valtype typ = structure "wasm_valtype_t";;

let valtype_new v =
  (foreign "wasm_valtype_new" (valkind @-> returning (ptr valtype)))
  (valkind_C_of_OCaml v);;
let valtype_kind v =
  valkind_OCaml_of_C
    (foreign "wasm_valtype_kind" (ptr valtype @-> returning valkind) v);;
let valtype_delete = foreign "wasm_valtype_delete" (ptr valtype @-> returning void);;

module Valtype_vec_chara = struct
  type data = valtype ptr
  let data = ptr valtype
  let name = "wasm_valtype"
  
  type coerce_from = data
  let coerce v = v
end;;
module Valtype_vec = Vector(Valtype_vec_chara);;

(* Val *)
type ref (*= [`Ref] structure *);;
let ref: ref typ = structure "wasm_ref_t";;

type val_ (*= [`Val] structure *);;
let val_: val_ typ = structure "wasm_val_t";;
let val_kind = field val_ "kind" valkind;;

type val_anon0 (*= [`Val_anon0] structure *);;
let val_anon0: val_anon0 typ = union "wasm_val_t.<union0>";;
let val_of_i32 = field val_anon0 "i32" int32_t;;
let val_of_i64 = field val_anon0 "i64" int64_t;;
let val_of_f32 = field val_anon0 "f32" float;;
let val_of_f64 = field val_anon0 "f64" double;;
let val_of_ref = field val_anon0 "ref" (ptr ref);;
let val_of = field val_ "of" val_anon0;;
let () = seal val_;;

let val_of_i32 i32 =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_i32);
  let tmp = make val_anon0 in
  setf tmp val_of_i32 i32;
  setf ret val_of tmp;
  ret;;
let val_of_i64 i64 =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_i64);
  let tmp = make val_anon0 in
  setf tmp val_of_i64 i64;
  setf ret val_of tmp;
  ret;;
let val_of_f32 f32 =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_f32);
  let tmp = make val_anon0 in
  setf tmp val_of_f32 f32;
  setf ret val_of tmp;
  ret;;
let val_of_f64 f64 =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_f64);
  let tmp = make val_anon0 in
  setf tmp val_of_f64 f64;
  setf ret val_of tmp;
  ret;;
let val_of_ref ref =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_AnyRef);
  let tmp = make val_anon0 in
  setf tmp val_of_ref ref;
  setf ret val_of tmp;
  ret;;
let val_new () =
  let ret = make val_ in
  setf ret val_kind (valkind_C_of_OCaml ValKind_AnyRef);
  let tmp = make val_anon0 in
  setf tmp val_of_ref (from_voidp (ptr ref) null);
  setf ret val_of tmp;
  ret;;
  
let val_copy =
  foreign "wasm_val_copy" (ptr val_ @-> ptr val_ @-> returning void);;
let val_delete =
  foreign "wasm_val_delete" (ptr val_ @-> returning void);;

(* Functype *)
type functype (*= [`Functype] structure *);;
let functype: functype typ = structure "wasm_functype_t";;
let functype_size = field functype "size" size_t;;
let () = seal functype;;

let _functype_new_c =
  foreign "wasm_functype_new"
    (ptr Valtype_vec.t @-> ptr Valtype_vec.t @-> returning (ptr functype));;
let _functype_params_c =
  foreign "wasm_functype_params" (ptr functype @-> returning (ptr Valtype_vec.t));;
let _functype_results_c =
  foreign "wasm_functype_results" (ptr functype @-> returning (ptr Valtype_vec.t));;

let functype_new_0_0 () =
  _functype_new_c (Valtype_vec.create_empty ()) (Valtype_vec.create_empty ());;
let functype_new_args_0 args =
  _functype_new_c (Valtype_vec.of_list args) (Valtype_vec.create_empty ());;
let functype_new_0_res res =
  _functype_new_c (Valtype_vec.create_empty ()) (Valtype_vec.of_list res);;
let functype_new_args_res args res =
  _functype_new_c (Valtype_vec.of_list args) (Valtype_vec.of_list res);;

let functype_delete =
  foreign "wasm_functype_delete" (ptr functype @-> returning void);;

(* Trap *)
let message = typedef Byte_vec.t "wasm_message_t";;

type trap (*= [`Trap] structure *);;
let trap: trap typ = structure "wasm_trap_t";;

(*
WASM_API_EXTERN own wasm_trap_t* wasm_trap_copy(const wasm_trap_t* );
WASM_API_EXTERN bool wasm_trap_same(const wasm_trap_t*, const wasm_trap_t* );

WASM_API_EXTERN void* wasm_trap_get_host_info(const wasm_trap_t* );
WASM_API_EXTERN void wasm_trap_set_host_info(wasm_trap_t*, void* );
WASM_API_EXTERN void wasm_trap_set_host_info_with_finalizer( wasm_trap_t*, void*, void (*)(void*));

WASM_API_EXTERN wasm_ref_t* wasm_trap_as_ref(wasm_trap_t* );
WASM_API_EXTERN wasm_trap_t* wasm_ref_as_trap(wasm_ref_t* );
WASM_API_EXTERN const wasm_ref_t* wasm_trap_as_ref_const(const wasm_trap_t* );
WASM_API_EXTERN const wasm_trap_t* wasm_ref_as_trap_const(const wasm_ref_t* );

WASM_API_EXTERN void wasm_trap_delete(own wasm_trap_t* );


WASM_API_EXTERN own wasm_trap_t* wasm_trap_new(wasm_store_t* store, const wasm_message_t* );

WASM_API_EXTERN void wasm_trap_message(const wasm_trap_t*, own wasm_message_t* out);
WASM_API_EXTERN own wasm_frame_t* wasm_trap_origin(const wasm_trap_t* );
WASM_API_EXTERN void wasm_trap_trace(const wasm_trap_t*, own wasm_frame_vec_t* out);
*)
let trap_new =
  foreign "wasm_trap_new" (ptr store @-> ptr message @-> returning (ptr trap));;
let trap_copy =
  foreign "wasm_trap_copy" (ptr trap @-> returning (ptr trap));;

let trap_same =
  foreign "wasm_trap_same" (ptr trap @-> ptr trap @-> returning bool);;

let trap_message =
  foreign "wasm_trap_message" (ptr trap @-> ptr message @-> returning void);;
(*let trap_origin =
  foreign "wasm_trap_origin" (ptr trap @-> returning (ptr frame));;
let trap_trace =
  foreign "wasm_trap_trace" (ptr trap @-> ptr Frame_vec.t @-> returning void);;*)

let trap_delete =
  foreign "wasm_trap_delete" (ptr trap @-> returning void);;

(* Func *)
type func (*= [`Func] structure *);;
let func: func typ = structure "wasm_func_t";;
let func_size = field func "size" size_t;;
let () = seal func;;

let func_callback =
  typedef
    (funptr (ptr val_ @-> ptr val_ @-> returning (ptr trap)))
    "wasm_func_callback_t";;

let func_new =
  foreign
    "wasm_func_new"
    (ptr store @-> ptr functype @-> func_callback @-> returning (ptr func));;
let func_delete = foreign "wasm_func_delete" (ptr func @-> returning void);;

(* Extern *)
type extern (*= [`Extern] structure *);;
let extern: extern typ = structure "wasm_extern_t";;
let extern_size = field extern "size" size_t;;
let () = seal extern;;

let func_as_extern =
  foreign "wasm_func_as_extern" (ptr func @-> returning (ptr extern));;
let extern_as_func =
  foreign "wasm_extern_as_func" (ptr extern @-> returning (ptr func));;

module Extern_vec_chara = struct
  type data = extern ptr
  let data = ptr extern
  let name = "wasm_extern"
  
  type coerce_from = data
  let coerce v = v
end;;
module Extern_vec = Vector(Extern_vec_chara);;

(* Instance *)
type instance (*= [`Instance] structure *);;
let instance: instance typ = structure "wasm_instance_t";;
let instance_size = field instance "size" size_t;;
let () = seal instance;;

let instance_new =
  foreign
    "wasm_instance_new"
    (ptr store @-> ptr module_ @-> ptr (ptr extern) @-> ptr void @-> returning (ptr instance));;

let instance_exports =
  foreign
    "wasm_instance_exports"
    (ptr instance @-> ptr Extern_vec.t @-> returning void);;

let func_call =
  foreign
    "wasm_func_call"
    (ptr func @-> ptr val_ @-> ptr val_ @-> returning (ptr void));;
*)
