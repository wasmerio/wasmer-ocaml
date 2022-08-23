open Ctypes;;
open Foreign;;

let () =
  foreign "assertions" (void @-> returning void) ();;

exception Returned_null of string;;
exception Invalid_access of string;;

type 'a dependent_update_func = 'a -> unit;; (* Not part of the public API *)

type object_state =
  | State_Owned
  | State_RW
  | State_Const
  | State_Dependent of ((unit -> object_state) * object_state dependent_update_func)
  | State_PassedAway;;

let rec get_real_state st = match st with
  | State_Dependent (f, _) -> get_real_state (f ())
  | State_Owned
  | State_RW
  | State_Const
  | State_PassedAway -> st;;
let rec max_state_const st = match st with
  | State_Dependent (f, _) -> max_state_const (f ())
  | State_Owned
  | State_RW
  | State_Const -> State_Const
  | State_PassedAway -> State_PassedAway;;

module type ObjectType = sig
  type t
  type d
  (** Creates a new pointer.
      @see {!val:OwnableObject.make_new} *)
  val make: unit -> t ptr (* Not part of the public API *)
  (** Deletes a pointer.
      The ownership is guaranteed to be [State_Owned]. *)
  val delete: t ptr -> unit (* Not part of the public API *)
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

module OWNABLE_OBJECT(O: ObjectType) = struct
  module type T = sig
    type s
    
    val make_new_st: object_state -> s (* Not part of the public API *)
    val make_new: unit -> s
    val make_from_raise: O.t ptr -> object_state -> s
    val make_from_unsafe: O.t ptr -> object_state -> s
    val make_from_raise_data: O.t ptr -> object_state -> O.d -> s
    val make_from_unsafe_data: O.t ptr -> object_state -> O.d -> s
    
    (* Set -- discards Dependent, ignore ownership hierarchy
       Update -- updates Dependent, crash if ownership increases *)
    (** Overrides the current state of the pointer.
        Ignores the updator if the current state is [State_Dependent _]. *)
    val set_state: s -> object_state -> unit (* Not part of the public API *)
    (** Updates the current state of the pointer.
        Updates through the updator if the current state is [State_Dependent _]. *)
    val update_state: s -> object_state -> unit (* Not part of the public API *)
    
    val get_state: s -> object_state
    val lose_ownership: s -> unit
    val grab_ownership: s -> O.t ptr
    val gain_ownership_back: s -> O.t ptr
    
    val is_null: s -> bool
    val get_ptr: s -> O.t ptr
    val get_ptr_const: s -> O.t ptr
    val get_ptr_givenaway: s -> O.t ptr
    
    val delete: s -> unit
  end
end;;

module OwnableObject(O: ObjectType) : OWNABLE_OBJECT(O).T = struct
  type s = { p: O.t ptr; mutable state: object_state; d: O.d option }
  
  let delete self = match self.state with
    | State_Dependent _ -> () (* The object doesn't own this *)
    | State_Owned -> O.delete self.p; self.state <- State_PassedAway
    | State_RW -> () (* The C API is unclear here, so do nothing to be safe *)
    | State_Const -> () (* The C API is unclear here, so do nothing to be safe *)
    | State_PassedAway -> ()
  
  (** Registers [self] to be deleted *)
  let register self = Gc.finalise delete self; self
  let make_new_st st = register { p = O.make (); state = st; d = None }
  let make_new () = register { p = O.make (); state = State_PassedAway; d = None }
  let make_from_raise p st =
    if is_null p then invalid_arg "cannot create an owned object from NULL"
    else register { p = p; state = st; d = None }
  let make_from_unsafe p st = register { p = p; state = st; d = None }
  let make_from_raise_data p st d =
    if is_null p then invalid_arg "cannot create an owned object from NULL"
    else register { p = p; state = st; d = Some d }
  let make_from_unsafe_data p st d = register { p = p; state = st; d = Some d }
  
  let set_state self st = self.state <- st
  let update_state self st = match self.state with
    | State_Dependent (_, up) -> up st
    | State_Owned -> self.state <- st
    | State_RW ->
      if st = State_Owned then invalid_arg "update_state does not increase ownership"
      else self.state <- st
    | State_Const ->
      if (st = State_Owned) || (st = State_RW) then
        invalid_arg "update_state does not increase ownership"
      else self.state <- st
    | State_PassedAway ->
      if st = State_PassedAway then self.state <- st
      else invalid_arg "update_state does not increase ownership"
  
  let get_state { state; _ } = state
  let lose_ownership self = self.state <- State_PassedAway
  let grab_ownership self = match self.state with
    | State_Owned -> self.state <- State_PassedAway; self.p
    | State_RW -> raise (Invalid_access "grab_ownership { state = State_RW }")
    | State_Const -> raise (Invalid_access "grab_ownership { state = State_Const }")
    | State_PassedAway ->
      raise (Invalid_access "grab_ownership { state = State_PassedAway }")
    | State_Dependent (_, up) -> (* TODO: what shold be done here? *)
      (* FIXME: if this is dependent on a vector, the vector never gets deleted
         since one element makes it completely given away.
         Furthermore, no special check is made to ensure that the real state is
         not PassedAway. *)
      up State_PassedAway; self.p
  let gain_ownership_back self = match self.state with
      | State_Dependent _ ->
        raise (Invalid_access "gain_ownership_back { state = State_Dependent }")
      | State_Owned -> O.delete self.p; self.p
      | _ -> self.state <- State_Owned; self.p
  
  let is_null { p; _ } = is_null p
  let get_ptr { p; state; _ } = match get_real_state state with
    | State_Owned -> p
    | State_RW -> p
    | State_Const -> raise (Invalid_access "get_ptr { state = ..State_Const }")
    | State_PassedAway -> raise (Invalid_access "get_ptr { state = ..State_PassedAway }")
    | State_Dependent _ -> failwith "unreachable (real_state = Dependent)"
  let get_ptr_const { p; state; _ } = match get_real_state state with
    | State_Owned -> p
    | State_RW -> p
    | State_Const -> p
    | State_PassedAway ->
      raise (Invalid_access "get_ptr_const { state = ..State_PassedAway }")
    | State_Dependent _ -> failwith "unreachable (real_state = Dependent)"
  let get_ptr_givenaway { p; state; _ } = match get_real_state state with
    | State_PassedAway -> p
    | State_Owned ->
      raise (Invalid_access "get_ptr_givenaway { state = ..State_Owned }")
    | State_RW ->
      raise (Invalid_access "get_ptr_givenaway { state = ..State_RW }")
    | State_Const ->
      raise (Invalid_access "get_ptr_givenaway { state = ..State_Const }")
    | State_Dependent _ -> failwith "unreachable (real_state = Dependent)"
end;;

module DECLARE_OWN(S: StructType) = struct
  module type T = sig
    val name: string
    type t
    val t: t structure typ
    
    module O: ObjectType with type t = t structure with type d = S.d
    include module type of struct include OwnableObject(O) end
  end
end;;

module DeclareOwn(T: StructType) : DECLARE_OWN(T).T = struct
  let name = "wasm_" ^ T.name
  
  type t
  let t: t structure typ = structure name
  
  type t_bis = t
  module O = struct
    type t = t_bis structure
    type d = T.d
    
    let make () = addr (make t)
    let delete = foreign ~stub:true (name ^ "_delete") (ptr t @-> returning void)
  end
  include OwnableObject(O)
end;;

module DECLARE_VEC(U: VectorType) = struct
  module type T = sig
    type data_type = U.data_type
    val data_type: data_type typ
    
    val name: string
    
    type t
    val t: t structure typ
    val fsize: (Unsigned.size_t, t structure) field
    val fdata: (data_type ptr, t structure) field
    
    module O : ObjectType with type t = t structure with type d = unit
    include module type of struct include OwnableObject(O) end
    
    val make_empty: unit -> s
    val make_empty_null: unit -> s (* Initializes the data field to NULL *)
    val make_uninit: int -> s
    
    val duplicate: s -> s
    
    (* The vector always get ownership of the data *)
    val of_array: U.owning_struct array -> s
    val of_list: U.owning_struct list -> s
    
    val get_size: s -> int
    val get_element: s -> int -> U.owning_struct
    val get_element_unsafe: s -> int -> U.owning_struct
    val get_element_const: s -> int -> U.owning_struct
    val get_element_const_unsafe: s -> int -> U.owning_struct
    val set_element: s -> int -> U.owning_struct -> unit
    val set_element_unsafe: s -> int -> U.owning_struct -> unit
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
  
  type t_bis = t
  module O = struct
    type t = t_bis structure
    type d = unit
    
    let make () = addr (make t)
    let delete = foreign ~stub:true (name ^ "_delete") (ptr t @-> returning void)
  end
  include OwnableObject(O)
  
  let capi_new_empty = foreign ~stub:true (name ^ "_new_empty") (ptr t @-> returning void)
  let capi_new_uninitialized =
    foreign ~stub:true (name ^ "_new_uninitialized") (ptr t @-> size_t @-> returning void)
  let capi_new = foreign ~stub:true (name ^ "_new") (ptr t @-> size_t @-> ptr data_type @-> returning void)
  let capi_copy = foreign ~stub:true (name ^ "_copy") (ptr t @-> ptr t @-> returning void)
  
  (* Helper functions *)
  let make_empty () =
    let ret = make_new_st State_Owned in capi_new_empty (get_ptr ret); ret
  let make_empty_null () =
    let ret = make_new_st State_Owned in
    let p = get_ptr ret in
    p |-> fsize <-@ Unsigned.Size_t.of_int 0;
    p |-> fdata <-@ from_voidp data_type null;
    ret
  let make_uninit n =
    let ret = make_new_st State_Owned in
    capi_new_uninitialized (get_ptr ret) (Unsigned.Size_t.of_int n); ret
  
  let duplicate self =
    let p = get_ptr_const self in
    let cop = make_new_st State_Owned in
    capi_copy (get_ptr cop) p;
    cop
  
  let of_array a =
    let ret = make_uninit (Array.length a) in
    Array.iteri (fun i v -> !@ ((get_ptr ret) |-> fdata) +@ i <-@ T.grab_ownership v) a;
    ret
  let of_list l = of_array (Array.of_list l)
  
  let get_size self =
    Unsigned.Size_t.to_int (!@ ((get_ptr_const self) |-> fsize))
  let get_element_unsafe self i =
    T.to_dependent
      (!@ ((!@ ((get_ptr self) |-> fdata)) +@ i))
      ((fun () -> get_state self), (fun newstate -> update_state self newstate))
  let get_element self i =
    if (i < 0) || (i >= get_size self) then
      raise (Invalid_argument "index out of bounds")
    else get_element_unsafe self i
  let get_element_const_unsafe self i =
    T.to_dependent
      (!@ ((!@ ((get_ptr_const self) |-> fdata)) +@ i))
      ((fun () -> max_state_const (get_state self)), (fun newstate -> update_state self newstate))
  let get_element_const self i =
    if (i < 0) || (i >= get_size self) then
      raise (Invalid_argument "index out of bounds")
    else get_element_const_unsafe self i
  let set_element_unsafe self i v =
    (!@ ((get_ptr self) |-> fdata)) +@ i <-@ T.grab_ownership v
  let set_element self i v =
    if (i < 0) || (i >= get_size self) then
      raise (Invalid_argument "index out of bounds")
    else set_element_unsafe self i v
end;;

module DECLARE_TYPE(T: StructType) = struct
  module type T = sig
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
    module Vec: module type of DeclareVec(V)
    
    val duplicate: s -> s
    val duplicate_unsafe: s -> s
  end
end;;

module DeclareType(T: StructType) : DECLARE_TYPE(T).T = struct
  include DeclareOwn(T)
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    
    let name = T.name
    
    type owning_struct = s
    let grab_ownership s = grab_ownership s
    let to_dependent p f = make_from_unsafe p (State_Dependent f)
  end
  module Vec = DeclareVec(V)
  
  let capi_duplicate =
    foreign ~stub:true (name ^ "_copy") (ptr t @-> returning (ptr t))
  
  let duplicate self =
    let p = capi_duplicate (get_ptr_const self) in
    if Ctypes.is_null p then raise (Returned_null (name ^ "_copy"))
    else make_from_unsafe p State_Owned
  let duplicate_unsafe self =
    make_from_unsafe (capi_duplicate (get_ptr_const self)) State_Owned
end;;

module DECLARE_REF_BASE(T: StructType) = struct
  module type T = sig
    include module type of DeclareOwn(T)
    
    val duplicate: s -> s
    val duplicate_unsafe: s -> s
    val same: s -> s -> bool
    val get_host_info: s -> unit ptr
    val set_host_info: s -> unit ptr -> unit
    val set_host_info_with_finalizer: s -> unit ptr -> (unit ptr -> unit) -> unit
  end
end;;

module DeclareRefBase(T: StructType) : DECLARE_REF_BASE(T).T = struct
  include DeclareOwn(T)
  
  let capi_duplicate =
    foreign ~stub:true (name ^ "_copy") (ptr t @-> returning (ptr t))
  let capi_same =
    foreign ~stub:true (name ^ "_same") (ptr t @-> ptr t @-> returning bool)
  let capi_get_host_info =
    foreign ~stub:true (name ^ "_get_host_info") (ptr t @-> returning (ptr void))
  let capi_set_host_info =
    foreign ~stub:true (name ^ "_set_host_info") (ptr t @-> ptr void @-> returning void)
  let capi_set_host_info_with_finalizer =
    let mem = ref [] in (* Finalizers may be GC'd if not stored here *)
    let idx = ref 0 in
    let f = foreign ~stub:true
      (name ^ "_set_host_info_with_finalizer")
      (ptr t @-> ptr void @-> (funptr (ptr void @-> returning void)) @-> returning void) in
    fun r hinfo cb -> (* Make sure the callback gets GC'd *)
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
      f r hinfo real_callback
  
  let duplicate self =
    let p = capi_duplicate (get_ptr self) in
    if Ctypes.is_null p then raise (Returned_null (name ^ "_copy"))
    else make_from_unsafe p State_Owned
  let duplicate_unsafe self = make_from_unsafe (capi_duplicate (get_ptr self)) State_Owned
  let same self other = capi_same (get_ptr_const self) (get_ptr_const other)
  let get_host_info self = capi_get_host_info (get_ptr_const self)
  let set_host_info self hinfo = capi_set_host_info (get_ptr self) hinfo
  let set_host_info_with_finalizer self hinfo cb =
    capi_set_host_info_with_finalizer (get_ptr self) hinfo cb
end;;

module Ref_T = struct
  let name = "ref"
  type d = unit
end;;
module Ref = DeclareRefBase(Ref_T);;

module DECLARE_REF(T: StructType) = struct
  module type T = sig
    include module type of DeclareRefBase(T)
    
    val to_ref: s -> Ref.s
    val of_ref: Ref.s -> s
    val to_ref_const: s -> Ref.s
    val of_ref_const: Ref.s -> s
  end
end;;

module DeclareRef(T: StructType) : DECLARE_REF(T).T = struct
  include DeclareRefBase(T)
  
  let capi_to_ref =
    foreign ~stub:true (name ^ "_as_ref") (ptr t @-> returning (ptr Ref.t))
  let capi_of_ref =
    foreign ~stub:true ("wasm_ref_as_" ^ T.name) (ptr Ref.t @-> returning (ptr t))
  let capi_to_ref_const =
    foreign ~stub:true (name ^ "_as_ref_const") (ptr t @-> returning (ptr Ref.t))
  let capi_of_ref_const =
    foreign ~stub:true ("wasm_ref_as_" ^ T.name ^ "_const") (ptr Ref.t @-> returning (ptr t))
  
  let to_ref self = Ref.make_from_unsafe (capi_to_ref (get_ptr self)) State_Owned
  let of_ref ref = make_from_unsafe (capi_of_ref (Ref.get_ptr ref)) State_Owned
  let to_ref_const self =
    Ref.make_from_unsafe (capi_to_ref_const (get_ptr_const self)) State_Const
  let of_ref_const ref =
    make_from_unsafe (capi_of_ref_const (Ref.get_ptr_const ref)) State_Const
end;;


(* Forward declaration *)
module Wasi_ = struct
  module Env_O_ = struct
    type _t
    type t = _t structure
    type d = unit
    let t: t typ = structure "wasi_env"
    let make () = addr (make t)
    let delete = foreign ~stub:true "wasi_env_delete" (ptr t @-> returning void)
  end
  module Env_ = struct
    include OwnableObject(Env_O_)
  end
end;;

module Byte = struct
  type byte = Unsigned.uint8
  let byte = uint8_t
  module V = struct
    type data_type = byte
    let data_type = byte
    let name = "byte"
    
    type owning_struct = byte
    let grab_ownership (b: owning_struct): data_type = b
    let to_dependent (b: data_type) _: owning_struct = b
  end
  module Vec = struct
    include DeclareVec(V)
    
    let of_char_list l = of_list (List.map (fun i -> Unsigned.UInt8.of_int (Char.code i)) l)
    let of_int_list l = of_list (List.map (fun i -> Unsigned.UInt8.of_int i) l)
    let of_bytes b = of_char_list (List.of_seq (Bytes.to_seq b))
    let to_char_list self =
      List.init (get_size self)
      (fun i -> Char.chr (Unsigned.UInt8.to_int (get_element_const_unsafe self i)))
    let to_int_list self =
      List.init (get_size self)
      (fun i -> Unsigned.UInt8.to_int (get_element_const_unsafe self i))
    let to_bytes self =
      Bytes.init (get_size self)
        (fun i -> Char.chr (Unsigned.UInt8.to_int (get_element_const_unsafe self i)))
  end
end;;
module Name = struct
  include Byte.Vec
  
  let of_string s = of_bytes (Bytes.of_string s)
  let to_string self = Bytes.unsafe_to_string (to_bytes self)
end;;
module Message = struct
  include Name
end;;

module Config_T = struct
  let name = "config"
  type d = unit
end;;
(** Embedders may provide custom functions for manipulating configs. *)
module Config = struct
  include DeclareOwn(Config_T)
  
  let capi_new = foreign ~stub:true "wasm_config_new" (void @-> returning (ptr t))
  
  let new_ () =
    make_from_raise (capi_new ()) State_Owned
  let new_unsafe () =
    make_from_unsafe (capi_new ()) State_Owned
  
  (* TODO: wasi_config_* *)
end;;

module Engine_T = struct
  let name = "engine"
  type d = unit
end;;
module Engine = struct
  include DeclareOwn(Engine_T)
  
  let capi_new = foreign ~stub:true "wasm_engine_new" (void @-> returning (ptr t))
  let capi_new_with_config =
    foreign ~stub:true "wasm_engine_new_with_config" (ptr Config.t @-> returning (ptr t))
  
  let new_ () = make_from_raise (capi_new ()) State_Owned
  let new_unsafe () = make_from_unsafe (capi_new ()) State_Owned
  
  let new_with_config config =
    make_from_raise (capi_new_with_config (Config.grab_ownership config)) State_Owned
  let new_with_config_unsafe config =
    make_from_unsafe (capi_new_with_config (Config.grab_ownership config)) State_Owned
end;;

module Store_T = struct
  let name = "store"
  type d = unit
end;;
module Store = struct
  include DeclareOwn(Store_T)
  
  let capi_new =
    foreign ~stub:true "wasm_store_new" (ptr Engine.t @-> returning (ptr t))
  
  let new_ engine =
    make_from_raise (capi_new (Engine.get_ptr engine)) State_Owned
  let new_unsafe engine =
    make_from_unsafe (capi_new (Engine.get_ptr engine)) State_Owned
end;;

module Mutability = struct
  type ocaml = Const | Var
  
  type t = Unsigned.uint8
  let t: t typ = typedef uint8_t "wasm_mutability_t"
  
  let of_c (cval: t) = match Unsigned.UInt8.to_int cval with
    | 0 -> Const
    | 1 -> Var
    | i -> invalid_arg ("Invalid C mutability " ^ (string_of_int i))
  let to_c ocval: t = match ocval with
    | Const -> Unsigned.UInt8.zero
    | Var -> Unsigned.UInt8.one
end;;

module Limits = struct
  type t
  let t: t structure typ = structure "wasm_limits_t"
  let min = field t "min" uint32_t
  let max = field t "max" uint32_t
  let () = seal t
  
  let max_default = 0xFFFFFFFF (* limits_max_default *)
  
  (* TODO: can limits_t be owned? *)
end;;

module Valkind = struct
  type ocaml =
    | I32
    | I64
    | F32
    | F64
    | AnyRef
    | FuncRef
  
  type t = Unsigned.uint8
  let t: t typ = typedef uint8_t "wasm_valkind_t"
  
  let to_c vk: t = Unsigned.UInt8.of_int (match vk with
    | I32 -> 0
    | I64 -> 1
    | F32 -> 2
    | F64 -> 3
    | AnyRef -> 128
    | FuncRef -> 129)
  let of_c (vk: t) = match Unsigned.UInt8.to_int vk with
    | 0 -> I32
    | 1 -> I64
    | 2 -> F32
    | 3 -> F64
    | 128 -> AnyRef
    | 129 -> FuncRef
    | i -> invalid_arg ("Invalid C valkind " ^ (string_of_int i))
  
  let is_num = function
    | I32 | I64 | F32 | F64 -> true
    | AnyRef | FuncRef -> false
  let is_ref = function
    | I32 | I64 | F32 | F64 -> false
    | AnyRef | FuncRef -> true
end;;

module Valtype_T = struct
  let name = "valtype"
  type d = unit
end;;
module Valtype = struct
  include DeclareType(Valtype_T)
  
  let capi_new =
    let f = foreign ~stub:true "wasm_valtype_new" (Valkind.t @-> returning (ptr t)) in
    fun v -> f (Valkind.to_c v)
  let capi_kind =
    let f = foreign ~stub:true "wasm_valtype_kind" (ptr t @-> returning Valkind.t) in
    fun v -> Valkind.of_c (f v)
  
  let new_ vk = make_from_raise (capi_new vk) State_Owned
  let new_unsafe vk = make_from_unsafe (capi_new vk) State_Owned
  let kind self = capi_kind (get_ptr_const self)
  
  let is_num self = Valkind.is_num (kind self)
  let is_ref self = Valkind.is_ref (kind self)
end;;

module Functype_T = struct
  let name = "functype"
  type d = unit
end;;
module Functype = struct
  include DeclareType(Functype_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_functype_new"
      (ptr Valtype.Vec.t @-> ptr Valtype.Vec.t @-> returning (ptr t))
  let capi_params =
    foreign ~stub:true "wasm_functype_params" (ptr t @-> returning (ptr Valtype.Vec.t))
  let capi_results =
    foreign ~stub:true "wasm_functype_results" (ptr t @-> returning (ptr Valtype.Vec.t))
  
  let new_ params results =
    make_from_raise (capi_new
     (Valtype.Vec.grab_ownership params)
     (Valtype.Vec.grab_ownership results)) State_Owned
  let new_unsafe params results = make_from_unsafe (capi_new
     (Valtype.Vec.grab_ownership params)
     (Valtype.Vec.grab_ownership results)) State_Owned
  
  let params self =
    Valtype.Vec.make_from_raise (capi_params (get_ptr_const self)) State_Const
  let results self =
    Valtype.Vec.make_from_raise (capi_results (get_ptr_const self)) State_Const
end;;

module Globaltype_T = struct
  let name = "globaltype"
  type d = unit
end;;
module Globaltype = struct
  include DeclareType(Globaltype_T)
  
  let capi_new =
    let f = foreign ~stub:true
      "wasm_globaltype_new"
      (ptr Valtype.t @-> Mutability.t @-> returning (ptr t)) in
    fun v1 v2 -> f v1 (Mutability.to_c v2)
  let capi_content =
    foreign ~stub:true "wasm_globaltype_content" (ptr t @-> returning (ptr Valtype.t))
  let capi_mutability =
    let f = foreign ~stub:true "wasm_globaltype_mutability"
      (ptr t @-> returning Mutability.t) in
    fun v -> Mutability.of_c (f v)
  
  let new_ content mutability =
    make_from_raise (capi_new
     (Valtype.grab_ownership content) mutability) State_Owned
  let new_unsafe content mutability = make_from_unsafe (capi_new
     (Valtype.grab_ownership content) mutability) State_Owned
  
  let content self =
    Valtype.make_from_raise (capi_content (get_ptr_const self)) State_Const
  let mutability self = capi_mutability (get_ptr_const self)
end;;

module Tabletype_T = struct
  let name = "tabletype"
  type d = unit
end;;
module Tabletype = struct
  include DeclareType(Tabletype_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_tabletype_new"
      (ptr Valtype.t @-> ptr Limits.t @-> returning (ptr t))
  let capi_elements =
    foreign ~stub:true "wasm_tabletype_elements" (ptr t @-> returning (ptr Valtype.t))
  let capi_limits =
    foreign ~stub:true "wasm_tabletype_limits" (ptr t @-> returning (ptr Limits.t))
  
  let new_ elements limits = make_from_raise (capi_new
     (Valtype.grab_ownership elements) limits) State_Owned
  let new_unsafe elements limits = make_from_unsafe (capi_new
     (Valtype.grab_ownership elements) limits) State_Owned
  
  let elements self =
    Valtype.make_from_raise (capi_elements (get_ptr_const self)) State_Const
  let limits self =
    capi_limits (get_ptr_const self)
end;;

module Memorytype_T = struct
  let name = "memorytype"
  type d = unit
end;;
module Memorytype = struct
  include DeclareType(Memorytype_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_memorytype_new"
      (ptr Limits.t @-> returning (ptr t))
  let capi_limits =
    foreign ~stub:true "wasm_memorytype_limits" (ptr t @-> returning (ptr Limits.t))
  
  let new_ limits = make_from_raise (capi_new limits) State_Owned
  let new_unsafe limits = make_from_unsafe (capi_new limits) State_Owned
  
  let limits self =
    capi_limits (get_ptr_const self)
end;;

module Externkind = struct
  type ocaml =
    | Func
    | Global
    | Table
    | Memory
  
  type t = Unsigned.uint8
  let t: t typ = typedef uint8_t "wasm_externkind_t"
  
  let of_c (vk: t) = match Unsigned.UInt8.to_int vk with
    | 0 -> Func
    | 1 -> Global
    | 2 -> Table
    | 3 -> Memory
    | i -> invalid_arg ("Invalid C externkind " ^ (string_of_int i))
  let to_c vk: t = Unsigned.UInt8.of_int (match vk with
    | Func -> 0
    | Global -> 1
    | Table -> 2
    | Memory -> 3)
end;;

module Externtype_T = struct
  let name = "externtype"
  type d = unit
end;;
module Externtype = struct
  include DeclareType(Externtype_T)
  
  let capi_kind =
    let f =
      foreign ~stub:true
        "wasm_externtype_kind"
        (ptr t @-> returning Externkind.t) in
    fun v -> Externkind.of_c (f v)
  
  let capi_of_functype =
    foreign ~stub:true "wasm_functype_as_externtype" (ptr Functype.t @-> returning (ptr t))
  let capi_of_globaltype =
    foreign ~stub:true "wasm_globaltype_as_externtype" (ptr Globaltype.t @-> returning (ptr t))
  let capi_of_tabletype =
    foreign ~stub:true "wasm_tabletype_as_externtype" (ptr Tabletype.t @-> returning (ptr t))
  let capi_of_memorytype =
    foreign ~stub:true "wasm_memorytype_as_externtype" (ptr Memorytype.t @-> returning (ptr t))
  
  let capi_to_functype =
    foreign ~stub:true "wasm_functype_as_externtype" (ptr t @-> returning (ptr Functype.t))
  let capi_to_globaltype =
    foreign ~stub:true "wasm_globaltype_as_externtype" (ptr t @-> returning (ptr Globaltype.t))
  let capi_to_tabletype =
    foreign ~stub:true "wasm_tabletype_as_externtype" (ptr t @-> returning (ptr Tabletype.t))
  let capi_to_memorytype =
    foreign ~stub:true "wasm_memorytype_as_externtype" (ptr t @-> returning (ptr Memorytype.t))
  
  let capi_of_functype_const =
    foreign ~stub:true "wasm_functype_as_externtype_const" (ptr Functype.t @-> returning (ptr t))
  let capi_of_globaltype_const =
    foreign ~stub:true "wasm_globaltype_as_externtype_const" (ptr Globaltype.t @-> returning (ptr t))
  let capi_of_tabletype_const =
    foreign ~stub:true "wasm_tabletype_as_externtype_const" (ptr Tabletype.t @-> returning (ptr t))
  let capi_of_memorytype_const =
    foreign ~stub:true "wasm_memorytype_as_externtype_const" (ptr Memorytype.t @-> returning (ptr t))
  
  let capi_to_functype_const =
    foreign ~stub:true "wasm_functype_as_externtype_const" (ptr t @-> returning (ptr Functype.t))
  let capi_to_globaltype_const =
    foreign ~stub:true "wasm_globaltype_as_externtype_const" (ptr t @-> returning (ptr Globaltype.t))
  let capi_to_tabletype_const =
    foreign ~stub:true "wasm_tabletype_as_externtype_const" (ptr t @-> returning (ptr Tabletype.t))
  let capi_to_memorytype_const =
    foreign ~stub:true "wasm_memorytype_as_externtype_const" (ptr t @-> returning (ptr Memorytype.t))
  
  let kind self =
    capi_kind (get_ptr_const self)
  let of_functype other =
    make_from_raise (capi_of_functype (Functype.get_ptr other))
      (State_Dependent
        ((fun () -> Functype.get_state other),
         (fun st -> Functype.update_state other st)))
  let of_globaltype other =
    make_from_raise (capi_of_globaltype (Globaltype.get_ptr other))
      (State_Dependent
        ((fun () -> Globaltype.get_state other),
         (fun st -> Globaltype.update_state other st)))
  let of_tabletype other =
    make_from_raise (capi_of_tabletype (Tabletype.get_ptr other))
      (State_Dependent
        ((fun () -> Tabletype.get_state other),
         (fun st -> Tabletype.update_state other st)))
  let of_memorytype other =
    make_from_raise (capi_of_memorytype (Memorytype.get_ptr other))
      (State_Dependent
        ((fun () -> Memorytype.get_state other),
         (fun st -> Memorytype.update_state other st)))
  let to_functype self =
    Functype.make_from_raise (capi_to_functype (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_globaltype self =
    Globaltype.make_from_raise (capi_to_globaltype (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_tabletype self =
    Tabletype.make_from_raise (capi_to_tabletype (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_memorytype self =
    Memorytype.make_from_raise (capi_to_memorytype (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let of_functype_const other =
    make_from_raise (capi_of_functype_const (Functype.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Functype.get_state other)),
         (fun st -> Functype.update_state other st)))
  let of_globaltype_const other =
    make_from_raise (capi_of_globaltype_const (Globaltype.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Globaltype.get_state other)),
         (fun st -> Globaltype.update_state other st)))
  let of_tabletype_const other =
    make_from_raise (capi_of_tabletype_const (Tabletype.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Tabletype.get_state other)),
         (fun st -> Tabletype.update_state other st)))
  let of_memorytype_const other =
    make_from_raise (capi_of_memorytype_const (Memorytype.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Memorytype.get_state other)),
         (fun st -> Memorytype.update_state other st)))
  let to_functype_const self =
    Functype.make_from_raise (capi_to_functype_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_globaltype_const self =
    Globaltype.make_from_raise (capi_to_globaltype_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_tabletype_const self =
    Tabletype.make_from_raise (capi_to_tabletype_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_memorytype_const self =
    Memorytype.make_from_raise (capi_to_memorytype_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
end;;

module Importtype_T = struct
  let name = "importtype"
  type d = unit
end;;
module Importtype = struct
  include DeclareType(Importtype_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_importtype_new"
      (ptr Name.t @-> ptr Name.t @-> ptr Externtype.t @-> returning (ptr t))
  let capi_module =
    foreign ~stub:true "wasm_importtype_module" (ptr t @-> returning (ptr Name.t))
  let capi_name =
    foreign ~stub:true "wasm_importtype_name" (ptr t @-> returning (ptr Name.t))
  let capi_type =
    foreign ~stub:true "wasm_importtype_type" (ptr t @-> returning (ptr Externtype.t))
  
  let new_ module_ name type_ =
    make_from_raise (capi_new
      (Name.grab_ownership module_) (Name.grab_ownership name)
      (Externtype.grab_ownership type_)) State_Owned
  let new_unsafe module_ name type_ =
    make_from_unsafe (capi_new
      (Name.grab_ownership module_) (Name.grab_ownership name)
      (Externtype.grab_ownership type_)) State_Owned
  
  let module_ self =
    Name.make_from_raise (capi_module (get_ptr_const self)) State_Const
  let name self =
    Name.make_from_raise (capi_name (get_ptr_const self)) State_Const
  let type_ self =
    Externtype.make_from_raise (capi_type (get_ptr_const self)) State_Const
end;;

module Exporttype_T = struct
  let name = "exporttype"
  type d = unit
end;;
module Exporttype = struct
  include DeclareType(Exporttype_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_exporttype_new"
      (ptr Name.t @-> ptr Externtype.t @-> returning (ptr t))
  let capi_name =
    foreign ~stub:true "wasm_exporttype_name" (ptr t @-> returning (ptr Name.t))
  let capi_type =
    foreign ~stub:true "wasm_exporttype_type" (ptr t @-> returning (ptr Externtype.t))
  
  let new_ name typ =
    make_from_raise
      (capi_new (Name.grab_ownership name) (Externtype.grab_ownership typ)) State_Const
  let new_unsafe name typ =
    make_from_unsafe
      (capi_new (Name.grab_ownership name) (Externtype.grab_ownership typ)) State_Const
  
  let name self =
    Name.make_from_raise (capi_name (get_ptr_const self)) State_Const
  let type_ self =
    Externtype.make_from_raise (capi_type (get_ptr_const self)) State_Const
end;;

module Val = struct
  type t
  let t: t structure typ = structure "wasm_val_t"
  let fkind = field t "kind" Valkind.t
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
  
  type t_bis = t
  module O = struct
    type t = t_bis structure
    type d = unit
    
    let make () = addr (make t)
    let delete = foreign ~stub:true "wasm_val_delete" (ptr t @-> returning void)
  end
  include OwnableObject(O)
  
  let of_i32 i32 =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.I32);
    let tmp = make Anon0.t in
    setf tmp Anon0.fi32 i32;
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  let of_i64 i64 =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.I64);
    let tmp = make Anon0.t in
    setf tmp Anon0.fi64 i64;
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  let of_f32 f32 =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.F32);
    let tmp = make Anon0.t in
    setf tmp Anon0.ff32 f32;
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  let of_f64 f64 =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.F64);
    let tmp = make Anon0.t in
    setf tmp Anon0.ff64 f64;
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  let of_ref ref =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.AnyRef);
    let tmp = make Anon0.t in
    setf tmp Anon0.fref (Ref.get_ptr ref);
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  let new_ () =
    let ret = make t in
    setf ret fkind (Valkind.to_c Valkind.AnyRef);
    let tmp = make Anon0.t in
    setf tmp Anon0.fref (from_voidp Ref.t null);
    setf ret fof tmp;
    make_from_unsafe (addr ret) State_Owned
  
  let capi_copy =
    foreign ~stub:true "wasm_val_copy" (ptr t @-> ptr t @-> returning void)
  
  type ocaml =
    | I32 of int32
    | I64 of int64
    | F32 of float
    | F64 of float
    | Ref of Ref.s
  
  let get_kind self =
    let p = get_ptr_const self in
    Valkind.of_c (!@ (p |-> fkind))
  
  let get_i32 self =
    let p = get_ptr_const self in
    match get_kind self with
    | Valkind.I32 -> !@ (p |-> fof |-> Anon0.fi32)
    | k -> invalid_arg "Val.get_i32"
  let get_i64 self =
    let p = get_ptr_const self in
    match get_kind self with
    | Valkind.I64 -> !@ (p |-> fof |-> Anon0.fi64)
    | k -> invalid_arg "Val.get_i64"
  let get_f32 self =
    let p = get_ptr_const self in
    match get_kind self with
    | Valkind.F32 -> !@ (p |-> fof |-> Anon0.ff32)
    | k -> invalid_arg "Val.get_f32"
  let get_f64 self =
    let p = get_ptr_const self in
    match get_kind self with
    | Valkind.F64 -> !@ (p |-> fof |-> Anon0.ff64)
    | k -> invalid_arg "Val.get_f64"
  let get_ref self =
    let p = get_ptr_const self in
    match get_kind self with
    | Valkind.AnyRef | Valkind.FuncRef ->
        Ref.make_from_unsafe (!@ (p |-> fof |-> Anon0.fref))
          (State_Dependent ((fun () -> get_state self), (fun st -> update_state self st)))
    | k -> invalid_arg "Val.get_ref"
  
  let of_c self = match get_kind self with
    | Valkind.I32 -> I32 (get_i32 self)
    | Valkind.I64 -> I64 (get_i64 self)
    | Valkind.F32 -> F32 (get_f32 self)
    | Valkind.F64 -> F64 (get_f64 self)
    | Valkind.AnyRef | Valkind.FuncRef -> Ref (get_ref self)
  let to_c self = match self with
    | I32 i -> of_i32 i
    | I64 i -> of_i64 i
    | F32 f -> of_f32 f
    | F64 f -> of_f64 f
    | Ref r -> of_ref r
  
  let duplicate self =
    let p = get_ptr_const self in
    let cop = make_new_st State_Owned in
    capi_copy (get_ptr cop) p;
    cop
  
  module V = struct
    type data_type = t structure
    let data_type = t
    let name = "val"
    
    type owning_struct = s
    let grab_ownership s = !@ (grab_ownership s)
    let to_dependent v f = make_from_unsafe (addr v) (State_Dependent f)
  end
  module Vec = struct
    include DeclareVec(V)
    
    let is_compatible self valtypevec =
      let len = get_size self in
      if len <> Valtype.Vec.get_size valtypevec then false
      else
        let rec inner ptrself ptrvt i =
          if i >= len then true
          else if Valkind.of_c (!@ (ptrself |-> fkind)) <> Valtype.capi_kind (!@ ptrvt)
            then false
          else inner (ptrself +@ 1) (ptrvt +@ 1) (i + 1)
        in inner (!@ ((get_ptr_const self) |-> fdata))
          (!@ ((Valtype.Vec.get_ptr_const valtypevec) |-> Valtype.Vec.fdata)) 0
  end
end;;


module DECLARE_SHAREABLE_REF(T: StructType) = struct
  module type T = sig
    include module type of DeclareRef(T)
    module S : sig
      val name: string
      type d = unit
    end
    module Shared : module type of DeclareOwn(S)
    
    val to_shared: s -> Shared.s
    val of_shared: Store.s -> Shared.s -> s
  end
end;;

module DeclareShareableRef(T: StructType) : DECLARE_SHAREABLE_REF(T).T = struct
  include DeclareRef(T)
  module S = struct
    let name = "shared_" ^ T.name
    type d = unit
  end
  module Shared = DeclareOwn(S)
  
  let capi_to_shared =
    foreign ~stub:true (name ^ "_share") (ptr t @-> returning (ptr Shared.t))
  let capi_of_shared =
    foreign ~stub:true (name ^ "_obtain") (ptr Store.t @-> ptr Shared.t @-> returning (ptr t))
  
  let to_shared self =
    Shared.make_from_unsafe
     (capi_to_shared (get_ptr_const self)) State_Owned
  let of_shared store shared =
    make_from_unsafe (capi_of_shared
      (Store.get_ptr store)
      (Shared.get_ptr_const shared)) State_Owned
end;;


module Frame_T = struct
  let name = "frame"
  type d = unit
end
module Frame = struct
  include DeclareOwn(Frame_T)
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    let name = Frame_T.name
    
    type owning_struct = s
    let grab_ownership s = grab_ownership s
    let to_dependent p f = make_from_unsafe p (State_Dependent f)
  end
  module Vec = DeclareVec(V)
  
  let capi_duplicate =
    foreign ~stub:true "wasm_frame_copy" (ptr t @-> returning (ptr t))
  
  let capi_instance =
    foreign ~stub:true "wasm_frame_instance" (ptr t @-> returning (ptr void))
  let capi_func_index =
    foreign ~stub:true "wasm_frame_func_index" (ptr t @-> returning uint32_t)
  let capi_func_offset =
    foreign ~stub:true "wasm_frame_func_offset" (ptr t @-> returning size_t)
  let capi_module_offset =
    foreign ~stub:true "wasm_frame_module_offset" (ptr t @-> returning size_t)
  
  let duplicate self =
    let p = capi_duplicate (get_ptr_const self) in
    if Ctypes.is_null p then raise (Returned_null "wasm_frame_copy")
    else make_from_unsafe p State_Owned
  let duplicate_unsafe self =
    make_from_unsafe (capi_duplicate (get_ptr_const self)) State_Owned
  
  (* Frame.instance -> see frame_instance (cannot be put here as Instance is not defined) *)
  let func_index self = Unsigned.UInt32.to_int (capi_func_index (get_ptr_const self))
  let func_offset self =
    Unsigned.Size_t.to_int (capi_func_offset (get_ptr_const self))
  let module_offset self =
    Unsigned.Size_t.to_int (capi_module_offset (get_ptr_const self))
end;;

module Trap_T = struct
  let name = "trap"
  type d = unit
end;;
module Trap = struct
  include DeclareRef(Trap_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_trap_new"
      (ptr Store.t @-> ptr Message.t @-> returning (ptr t))
  
  let capi_message =
    foreign ~stub:true
      "wasm_trap_message"
      (ptr t @-> ptr Message.t @-> returning void)
  let capi_origin =
    foreign ~stub:true
      "wasm_trap_origin"
      (ptr t @-> returning (ptr Frame.t))
  let capi_trace =
    foreign ~stub:true
      "wasm_trap_trace"
      (ptr t @-> ptr Frame.Vec.t @-> returning void)
  
  let new_ store message =
    make_from_raise
      (capi_new (Store.get_ptr store) (Message.get_ptr_const message)) State_Owned
  let new_unsafe store message =
    make_from_unsafe
      (capi_new (Store.get_ptr store) (Message.get_ptr_const message)) State_Owned
  
  let message self message =
    capi_message (get_ptr_const self) (Message.gain_ownership_back message)
  let origin self =
    Frame.make_from_raise (capi_origin (get_ptr_const self)) State_Owned
  let trace self trace =
    capi_trace (get_ptr_const self) (Frame.Vec.gain_ownership_back trace)
end;;

module Foreign_T = struct
  let name = "foreign"
  type d = unit
end;;
module Foreign = struct
  include DeclareRef(Foreign_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_foreign_new"
      (ptr Store.t @-> returning (ptr t))
  
  let new_ store = make_from_raise (capi_new (Store.get_ptr store)) State_Owned
  let new_unsafe store = make_from_unsafe (capi_new (Store.get_ptr store)) State_Owned
end;;

module Module_T = struct
  let name = "module"
  type d = unit
end;;
module Module = struct
  include DeclareShareableRef(Module_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_module_new"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning (ptr t))
  
  let capi_validate =
    foreign ~stub:true
      "wasm_module_validate"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning bool)
  
  let capi_imports =
    foreign ~stub:true
      "wasm_module_imports"
      (ptr t @-> ptr Importtype.Vec.t @-> returning void)
  let capi_exports =
    foreign ~stub:true
      "wasm_module_exports"
      (ptr t @-> ptr Importtype.Vec.t @-> returning void)
  
  let capi_serialize =
    foreign ~stub:true
      "wasm_module_serialize"
      (ptr t @-> ptr Byte.Vec.t @-> returning void)
  let capi_deserialize =
    foreign ~stub:true
      "wasm_module_deserialize"
      (ptr Store.t @-> ptr Byte.Vec.t @-> returning (ptr t))
  
  let validate store binary =
    capi_validate (Store.get_ptr store) (Byte.Vec.get_ptr_const binary)
  let new_ store binary =
    make_from_raise
      (capi_new (Store.get_ptr store) (Byte.Vec.get_ptr_const binary)) State_Owned
  let new_unsafe store binary =
    make_from_unsafe
      (capi_new (Store.get_ptr store) (Byte.Vec.get_ptr_const binary)) State_Owned
  
  let imports self other =
    capi_imports (get_ptr_const self) (Importtype.Vec.gain_ownership_back other)
  let exports self other =
    capi_exports (get_ptr_const self) (Importtype.Vec.gain_ownership_back other)
  
  let serialize self other =
    capi_serialize (get_ptr_const self) (Byte.Vec.gain_ownership_back other)
  let deserialize store binary =
    make_from_raise
      (capi_deserialize (Store.get_ptr store) (Byte.Vec.grab_ownership binary))
      State_Owned
  let deserialize_unsafe store binary =
    make_from_unsafe
      (capi_deserialize (Store.get_ptr store) (Byte.Vec.grab_ownership binary))
      State_Owned
end;;

module Func_T = struct
  let name = "func"
  type d =
    | CallbackFunc of
      (Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr)
    | CallbackEnvFunc of
      ((unit ptr ->
        Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr) *
      (unit ptr -> unit) option)
end;;
module Func = struct
  include DeclareType(Func_T)
  
  type capi_callback_t =
    Val.Vec.t structure ptr -> Val.Vec.t structure ptr -> Trap.t structure ptr
  type capi_callback_with_env_t =
    unit ptr -> Val.Vec.t structure ptr
     -> Val.Vec.t structure ptr -> Trap.t structure ptr
  let capi_callback_t =
    typedef
      (funptr (ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t)))
      "wasm_func_callback_t"
  let capi_callback_with_env_t =
    typedef
      (funptr (ptr void @-> ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t)))
      "wasm_func_callback_with_env_t"
  type callback_t = Store.s -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  type callback_with_env_t = Wasi_.Env_.s -> Val.Vec.s -> Val.Vec.s -> Trap.s option
  
  let capi_new =
    foreign ~stub:true
      "wasm_func_new"
      (ptr Store.t @-> ptr Functype.t @-> capi_callback_t @-> returning (ptr t))
  let capi_new_with_env =
    foreign ~stub:true
      "wasm_func_new_with_env"
      (ptr Store.t @-> ptr Functype.t @-> capi_callback_with_env_t @-> ptr void @->
        (funptr_opt (ptr void @-> returning void)) @-> returning (ptr t))
  
  let capi_type =
    foreign ~stub:true "wasm_func_type" (ptr t @-> returning (ptr Functype.t))
  let capi_param_arity =
    foreign ~stub:true "wasm_func_param_arity" (ptr t @-> returning size_t)
  let capi_result_arity =
    foreign ~stub:true "wasm_func_result_arity" (ptr t @-> returning size_t)
  
  let capi_call =
    foreign ~stub:true
      "wasm_func_call"
      (ptr t @-> ptr Val.Vec.t @-> ptr Val.Vec.t @-> returning (ptr Trap.t))
  
  let generate_callback store cb = fun args rets ->
    match cb store (Val.Vec.make_from_unsafe args State_Const)
      (Val.Vec.make_from_unsafe rets State_RW) with
    | None -> from_voidp Trap.t null
    | Some trap -> Trap.grab_ownership trap
  let generate_callback_env cb = fun env args rets ->
    match cb
      (Wasi_.Env_.make_from_unsafe (from_voidp Wasi_.Env_O_.t env) State_RW)
      (Val.Vec.make_from_unsafe args State_Const)
      (Val.Vec.make_from_unsafe rets State_RW) with
    | None -> from_voidp Trap.t null
    | Some trap -> Trap.grab_ownership trap
  let generate_finalizer_env optfin = Option.map (fun fin -> fun env ->
    fin (Wasi_.Env_.make_from_unsafe (from_voidp Wasi_.Env_O_.t env) State_RW)) optfin
  
  let new_ store typ cb =
    let newcb = generate_callback store cb in
    make_from_raise_data
      (capi_new (Store.get_ptr store) (Functype.get_ptr typ) newcb) State_Owned
      (Func_T.CallbackFunc newcb)
  let new_unsafe store typ cb =
    let newcb = generate_callback store cb in
    make_from_unsafe_data
      (capi_new (Store.get_ptr store) (Functype.get_ptr typ) newcb) State_Owned
      (Func_T.CallbackFunc newcb)
  let new_with_env store typ cb env finalizer =
    let newcb = generate_callback_env cb in
    let newfin = generate_finalizer_env finalizer in
    make_from_raise_data (capi_new_with_env
        (Store.get_ptr store) (Functype.get_ptr typ) newcb
        (to_voidp (Wasi_.Env_.get_ptr env)) newfin) State_Owned
      (Func_T.CallbackEnvFunc (newcb, newfin))
  let new_with_env_unsafe store typ cb env finalizer =
    let newcb = generate_callback_env cb in
    let newfin = generate_finalizer_env finalizer in
    make_from_unsafe_data (capi_new_with_env
        (Store.get_ptr store) (Functype.get_ptr typ) newcb
        (to_voidp (Wasi_.Env_.get_ptr env)) newfin) State_Owned
      (Func_T.CallbackEnvFunc (newcb, newfin))
  
  let type_ self =
    Functype.make_from_raise (capi_type (get_ptr_const self)) State_Owned
  let param_arity self =
    Unsigned.Size_t.to_int (capi_param_arity (get_ptr_const self))
  let result_arity self =
    Unsigned.Size_t.to_int (capi_result_arity (get_ptr_const self))
  
  let call_unsafe self args results =
    Trap.make_from_unsafe (capi_call
      (get_ptr_const self) (Val.Vec.get_ptr_const args) (Val.Vec.get_ptr results))
      State_Owned
  let compatible_vectors self args results =
    let fparms, frets =
      let funt = type_ self in
      Functype.params funt, Functype.results funt in
    if not (Val.Vec.is_compatible args fparms) then Error false
    else if
      let retl = Val.Vec.get_size results in
      let reql = Valtype.Vec.get_size frets in
      retl <> reql then Error true
    else Ok ()
  let call self args results =
    match compatible_vectors self args results with
    | Error b -> invalid_arg
        (if b then "Incompatible return vector" else "Incompatible arguments vector")
    | Ok () -> let ret = call_unsafe self args results in
      if Trap.is_null ret then None
      else Some ret
end;;

module Global_T = struct
  let name = "global"
  type d = unit
end;;
module Global = struct
  include DeclareType(Global_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_global_new"
      (ptr Store.t @-> ptr Globaltype.t @-> ptr Val.t @-> returning (ptr t))
  
  let capi_type =
    foreign ~stub:true "wasm_global_type" (ptr t @-> returning (ptr Globaltype.t))
  
  let capi_get =
    foreign ~stub:true "wasm_global_get" (ptr t @-> ptr Val.t @-> returning void)
  let capi_set =
    foreign ~stub:true "wasm_global_set" (ptr t @-> ptr Val.t @-> returning void)
  
  let new_ store typ value =
    make_from_raise (capi_new
      (Store.get_ptr store) (Globaltype.get_ptr_const typ) (Val.get_ptr_const value))
      State_Owned
  let new_unsafe store typ value =
    make_from_unsafe (capi_new
      (Store.get_ptr store) (Globaltype.get_ptr_const typ) (Val.get_ptr_const value))
      State_Owned
  
  let type_ self =
    Globaltype.make_from_raise (capi_type (get_ptr_const self)) State_Owned
  
  let get self out = capi_get (get_ptr_const self) (Val.gain_ownership_back out)
  let set self value = capi_set (get_ptr self) (Val.get_ptr_const value)
end;;

module Table_T = struct
  let name = "table"
  type d = unit
end;;
module Table = struct
  include DeclareType(Table_T)
  
  type size_t = Unsigned.uint32
  let size_t = typedef uint32_t "wasm_table_size_t"
  
  let capi_new =
    foreign ~stub:true
      "wasm_table_new"
      (ptr Store.t @-> ptr Tabletype.t @-> ptr Ref.t @-> returning (ptr t))
  
  let capi_type =
    foreign ~stub:true "wasm_table_type" (ptr t @-> returning (ptr Tabletype.t))
  
  let capi_get =
    foreign ~stub:true "wasm_table_get" (ptr t @-> size_t @-> returning (ptr Ref.t))
  let capi_set =
    foreign ~stub:true
      "wasm_table_set"
      (ptr t @-> size_t @-> ptr Ref.t @-> returning bool)
  
  let capi_size =
    foreign ~stub:true "wasm_table_size" (ptr t @-> returning size_t)
  let capi_grow =
    foreign ~stub:true
      "wasm_table_grow"
      (ptr t @-> size_t @-> ptr Ref.t @-> returning bool)
  
  let new_ store typ ref =
    make_from_raise (capi_new
      (Store.get_ptr store) (Tabletype.get_ptr_const typ) (Ref.get_ptr_const ref))
      State_Owned
  let new_unsafe store typ ref =
    make_from_unsafe (capi_new
      (Store.get_ptr store) (Tabletype.get_ptr_const typ) (Ref.get_ptr_const ref))
      State_Owned
  
  let type_ self = Tabletype.make_from_raise (capi_type (get_ptr_const self)) State_Owned
  
  let get self idx =
    Ref.make_from_raise
      (capi_get (get_ptr_const self) (Unsigned.UInt32.of_int idx)) State_Owned
  let set self idx ref =
    capi_set (get_ptr self) (Unsigned.UInt32.of_int idx) (Ref.get_ptr ref)
  
  let size self = Unsigned.UInt32.to_int (capi_size (get_ptr_const self))
  let grow self delta init =
    capi_grow (get_ptr self) (Unsigned.UInt32.of_int delta) (Ref.get_ptr init)
end;;

module Memory_T = struct
  let name = "memory"
  type d = unit
end;;
module Memory = struct
  include DeclareType(Memory_T)
  
  type pages_t = Unsigned.uint32
  let pages_t = typedef uint32_t "wasm_table_pages_t"
  
  let page_size = 0x10000 (* MEMORY_PAGE_SIZE *)
  
  let capi_new =
    foreign ~stub:true
      "wasm_memory_new"
      (ptr Store.t @-> ptr Memorytype.t @-> returning (ptr t))
  
  let capi_type =
    foreign ~stub:true "wasm_memory_type" (ptr t @-> returning (ptr Memorytype.t))
  
  let capi_data =
    foreign ~stub:true "wasm_memory_data" (ptr t @-> returning (ptr Byte.byte))
  let capi_data_size =
    foreign ~stub:true "wasm_memory_data_size" (ptr t @-> returning size_t)
  
  let capi_size =
    foreign ~stub:true "wasm_memory_size" (ptr t @-> returning pages_t)
  let capi_grow =
    foreign ~stub:true
      "wasm_memory_grow"
      (ptr t @-> pages_t @-> returning bool)
  
  let new_ store typ =
    make_from_raise (capi_new (Store.get_ptr store) (Memorytype.get_ptr_const typ))
      State_Owned
  let new_unsafe store typ =
    make_from_unsafe (capi_new (Store.get_ptr store) (Memorytype.get_ptr_const typ))
      State_Owned
  
  let type_ self =
    Memorytype.make_from_raise (capi_type (get_ptr_const self)) State_Owned
  
  let data self = capi_data (get_ptr self)
  let data_size self = Unsigned.Size_t.to_int (capi_data_size (get_ptr_const self))
  let get_data self offset size =
    if offset < 0 then invalid_arg "Offset is negative"
    else if offset + size > page_size * (data_size self) then
      invalid_arg "Offset + size is too big"
    else
      let dat = (data self) +@ offset in
      Bytes.init size (fun i -> Char.chr (Unsigned.UInt8.to_int (!@ (dat +@ i))))
  let set_data self offset size newdata =
    if offset < 0 then invalid_arg "Offset is negative"
    else if offset + size > page_size * (data_size self) then
      invalid_arg "Offset + size is too big"
    else
      let dat = (data self) +@ offset in
      Bytes.iteri (fun i c -> (dat +@ i) <-@ Unsigned.UInt8.of_int (Char.code c)) newdata
  
  let size self = Unsigned.UInt32.to_int (capi_size (get_ptr_const self))
  let grow self delta = capi_grow (get_ptr self) (Unsigned.UInt32.of_int delta)
end;;

module Extern_T = struct
  let name = "extern"
  type d = unit
end;;
module Extern = struct
  include DeclareType(Extern_T)
  
  let capi_kind =
    let f =
      foreign ~stub:true
        "wasm_extern_kind"
        (ptr t @-> returning Externkind.t) in
    fun v -> Externkind.of_c (f v)
  let capi_type =
    foreign ~stub:true
      "wasm_extern_type"
      (ptr t @-> returning (ptr Externtype.t))
  
  let kind self = capi_kind (get_ptr_const self)
  let type_ self = Externtype.make_from_raise (capi_type (get_ptr_const self)) State_Owned
  
  let capi_of_func =
    foreign ~stub:true "wasm_func_as_extern" (ptr Func.t @-> returning (ptr t))
  let capi_of_global =
    foreign ~stub:true "wasm_global_as_extern" (ptr Global.t @-> returning (ptr t))
  let capi_of_table =
    foreign ~stub:true "wasm_table_as_extern" (ptr Table.t @-> returning (ptr t))
  let capi_of_memory =
    foreign ~stub:true "wasm_memory_as_extern" (ptr Memory.t @-> returning (ptr t))
  
  let capi_to_func =
    foreign ~stub:true "wasm_func_as_extern" (ptr t @-> returning (ptr Func.t))
  let capi_to_global =
    foreign ~stub:true "wasm_global_as_extern" (ptr t @-> returning (ptr Global.t))
  let capi_to_table =
    foreign ~stub:true "wasm_table_as_extern" (ptr t @-> returning (ptr Table.t))
  let capi_to_memory =
    foreign ~stub:true "wasm_memory_as_extern" (ptr t @-> returning (ptr Memory.t))
  
  let capi_of_func_const =
    foreign ~stub:true "wasm_func_as_extern_const" (ptr Func.t @-> returning (ptr t))
  let capi_of_global_const =
    foreign ~stub:true "wasm_global_as_extern_const" (ptr Global.t @-> returning (ptr t))
  let capi_of_table_const =
    foreign ~stub:true "wasm_table_as_extern_const" (ptr Table.t @-> returning (ptr t))
  let capi_of_memory_const =
    foreign ~stub:true "wasm_memory_as_extern_const" (ptr Memory.t @-> returning (ptr t))
  
  let capi_to_func_const =
    foreign ~stub:true "wasm_func_as_extern_const" (ptr t @-> returning (ptr Func.t))
  let capi_to_global_const =
    foreign ~stub:true "wasm_global_as_extern_const" (ptr t @-> returning (ptr Global.t))
  let capi_to_table_const =
    foreign ~stub:true "wasm_table_as_extern_const" (ptr t @-> returning (ptr Table.t))
  let capi_to_memory_const =
    foreign ~stub:true "wasm_memory_as_extern_const" (ptr t @-> returning (ptr Memory.t))
  
  let of_func other =
    make_from_raise (capi_of_func (Func.get_ptr other))
      (State_Dependent
        ((fun () -> Func.get_state other),
         (fun st -> Func.update_state other st)))
  let of_global other =
    make_from_raise (capi_of_global (Global.get_ptr other))
      (State_Dependent
        ((fun () -> Global.get_state other),
         (fun st -> Global.update_state other st)))
  let of_table other =
    make_from_raise (capi_of_table (Table.get_ptr other))
      (State_Dependent
        ((fun () -> Table.get_state other),
         (fun st -> Table.update_state other st)))
  let of_memory other =
    make_from_raise (capi_of_memory (Memory.get_ptr other))
      (State_Dependent
        ((fun () -> Memory.get_state other),
         (fun st -> Memory.update_state other st)))
  let to_func self =
    Func.make_from_raise (capi_to_func (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_global self =
    Global.make_from_raise (capi_to_global (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_table self =
    Table.make_from_raise (capi_to_table (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let to_memory self =
    Memory.make_from_raise (capi_to_memory (get_ptr self))
      (State_Dependent
        ((fun () -> get_state self),
         (fun st -> update_state self st)))
  let of_func_const other =
    make_from_raise (capi_of_func_const (Func.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Func.get_state other)),
         (fun st -> Func.update_state other st)))
  let of_global_const other =
    make_from_raise (capi_of_global_const (Global.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Global.get_state other)),
         (fun st -> Global.update_state other st)))
  let of_table_const other =
    make_from_raise (capi_of_table_const (Table.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Table.get_state other)),
         (fun st -> Table.update_state other st)))
  let of_memory_const other =
    make_from_raise (capi_of_memory_const (Memory.get_ptr_const other))
      (State_Dependent
        ((fun () -> max_state_const (Memory.get_state other)),
         (fun st -> Memory.update_state other st)))
  let to_func_const self =
    Func.make_from_raise (capi_to_func_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_global_const self =
    Global.make_from_raise (capi_to_global_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_table_const self =
    Table.make_from_raise (capi_to_table_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  let to_memory_const self =
    Memory.make_from_raise (capi_to_memory_const (get_ptr_const self))
      (State_Dependent
        ((fun () -> max_state_const (get_state self)),
         (fun st -> update_state self st)))
  
  module V = struct
    type data_type = t structure ptr
    let data_type = ptr t
    let name = Extern_T.name
    
    type owning_struct = s
    let grab_ownership s = grab_ownership s
    let to_dependent p f = make_from_unsafe p (State_Dependent f)
  end
  module Vec = struct
    include DeclareVec(V)
    
    let split_kind self =
      let sz = get_size self in
      let rec inner funcs gbls tbls mems i =
        if i = sz then List.rev funcs, List.rev gbls, List.rev tbls, List.rev mems
        else begin let elt = get_element self i in
          match kind elt with
          | Externkind.Func -> inner ((to_func elt) :: funcs) gbls tbls mems (i + 1)
          | Externkind.Global -> inner funcs ((to_global elt) :: gbls) tbls mems (i + 1)
          | Externkind.Table -> inner funcs gbls ((to_table elt) :: tbls) mems (i + 1)
          | Externkind.Memory -> inner funcs gbls tbls ((to_memory elt) :: mems) (i + 1)
        end
      in inner [] [] [] [] 0
    let split_kind_const self =
      let sz = get_size self in
      let rec inner funcs gbls tbls mems i =
        if i = sz then List.rev funcs, List.rev gbls, List.rev tbls, List.rev mems
        else begin let elt = get_element_const self i in
          match kind elt with
          | Externkind.Func -> inner ((to_func elt) :: funcs) gbls tbls mems (i + 1)
          | Externkind.Global -> inner funcs ((to_global elt) :: gbls) tbls mems (i + 1)
          | Externkind.Table -> inner funcs gbls ((to_table elt) :: tbls) mems (i + 1)
          | Externkind.Memory -> inner funcs gbls tbls ((to_memory elt) :: mems) (i + 1)
        end
      in inner [] [] [] [] 0
  end
end;;

module Instance_T = struct
  let name = "instance"
  type d = unit
end;;
module Instance = struct
  include DeclareRef(Instance_T)
  
  let capi_new =
    foreign ~stub:true
      "wasm_instance_new"
      (ptr Store.t @-> ptr Module.t @-> ptr Extern.Vec.t @-> ptr (ptr Trap.t)
       @-> returning (ptr t))
  
  let capi_exports =
    foreign ~stub:true
      "wasm_instance_exports"
      (ptr t @-> ptr Extern.Vec.t @-> returning void)
  
  let new_unsafe store module_ imports =
    let trap = allocate (ptr Trap.t) (from_voidp Trap.t null) in
    let instance = capi_new
      (Store.get_ptr store) (Module.get_ptr_const module_)
      (Extern.Vec.get_ptr_const imports) trap in
    if Ctypes.is_null instance then Error (Trap.make_from_unsafe (!@ trap) State_Owned)
    else Ok (make_from_unsafe instance State_Owned)
  let new_ store module_ imports =
    match new_unsafe store module_ imports with
    | Ok instance -> Ok instance
    | Error trap ->
      if Trap.is_null trap then Error None
      else Error (Some trap)
  
  let exports self out =
    capi_exports (get_ptr_const self) (Extern.Vec.gain_ownership_back out)
end;;
let frame_instance f =
  Instance.make_from_raise
    (from_voidp Instance.t (to_voidp
     (Frame.capi_instance (Frame.get_ptr_const f)))) State_Owned;;

module Wasi = struct
  (* Enabled only if WASMER_WASI_ENABLED was defined at the C API library compile time *)
  module Config_T = struct
    let name = "config"
  end
  module Config = struct
    let name = "wasi_config"
    
    type t
    let t: t structure typ = structure name
    
    type t_bis = t
    module O = struct
      type t = t_bis structure
      type d = unit
      
      let make () = addr (make t)
      let delete ptr =
        (* TODO: panic? (There is no wasi_config_new) *) ()
    end
    include OwnableObject(O)
    
    let capi_new = foreign ~stub:true "wasi_config_new" (string @-> returning (ptr t))
    let new_ name = make_from_raise (capi_new name) State_Owned
    let new_unsafe name = make_from_unsafe (capi_new name) State_Owned
    (* TODO: all other wasi_config_* functions *)
  end
  
  include Wasi_
  module Env_T = struct
    type t = Env_O_._t
    let name = "env"
  end
  module Env = struct
    type t = Env_T.t
    let t = Env_O_.t
    include Env_
    
    let capi_new =
      foreign ~stub:true "wasi_env_new"
        (ptr Store.t @-> ptr Config.t @-> returning (ptr t))
    let capi_get_imports =
      foreign ~stub:true "wasi_get_imports"
        (ptr Store.t @-> ptr t @-> ptr Module.t @-> ptr Extern.Vec.t @-> returning bool)
    
    (* Ownership status is assumed, as there are no wasi_config_delete *)
    let new_ store config = make_from_raise
      (capi_new (Store.get_ptr store) (Config.grab_ownership config)) State_Owned
    let new_unsafe store config = make_from_unsafe
      (capi_new (Store.get_ptr store) (Config.grab_ownership config)) State_Owned
    let get_imports store env module_ =
      let ret = Extern.Vec.make_new () in
      if capi_get_imports
        (Store.get_ptr_const store)
        (get_ptr env)
        (Module.get_ptr_const module_)
        (Extern.Vec.get_ptr ret) then Some ret
      else None
  end
end;;

module Util = struct
  let wat2wasm =
    let f = foreign "wat2wasm" (ptr Byte.Vec.t @-> ptr Byte.Vec.t @-> returning void) in
    fun s dst ->
      let bytevec = Byte.Vec.of_bytes (Bytes.of_string s)
      in f (Byte.Vec.get_ptr_const bytevec) (Byte.Vec.gain_ownership_back dst);
      Byte.Vec.delete bytevec;;
  let wasm_of_wat wat =
    let dst = Byte.Vec.make_new () in
    wat2wasm wat dst;
    dst;;
  
  let (%->) args res =
    Functype.new_
      (Valtype.Vec.of_list (List.map Valtype.new_ args))
      (Valtype.Vec.of_list (List.map Valtype.new_ res));;
  
  let load_wasm_file filename =
    let f = open_in_bin filename in try
      let flen = in_channel_length f in
      let b = Bytes.create flen in
      really_input f b 0 flen;
      let ret = Byte.Vec.of_bytes b in
      close_in f; ret
    with e ->
      close_in_noerr f;
      raise e;;
end
