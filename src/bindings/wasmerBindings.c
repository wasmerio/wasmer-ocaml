#ifndef WASM_API_EXTERN
#ifdef _WIN32
#define WASM_API_EXTERN(l,n,r) __declspec(dllimport) l n r
#else
#define WASM_API_EXTERN(l,n,r) l n r
#endif
#endif
#define TYPEDEF(...) typedef __VA_ARGS__
#define ENUM(...) enum __VA_ARGS__
#include "wasmerBindings.h"

void assertions() {
  static_assert(sizeof(float) == sizeof(uint32_t), "incompatible float type");
  static_assert(sizeof(double) == sizeof(uint64_t), "incompatible double type");
  static_assert(sizeof(intptr_t) == sizeof(uint32_t) ||
                sizeof(intptr_t) == sizeof(uint64_t),
                "incompatible pointer type");
}

#undef WASM_H
#undef WASM_API_EXTERN
#define WASM_API_EXTERN(l,n,r) ptr = &n;
#undef TYPEDEF
#define TYPEDEF(...)
#undef ENUM
#define ENUM(...)
void _instanciate_all() {
//#include "wasmerBindings.h"
	void(*ptr)() = &wasm_byte_vec_new_empty;
	ptr = &wasm_valtype_vec_new_empty;
}
