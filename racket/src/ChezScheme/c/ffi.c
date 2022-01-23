#include "system.h"

#ifdef PORTABLE_BYTECODE
#ifdef ENABLE_LIBFFI

#include "ffi.h"

/* 
   Encoding of a function type:

     #(cached abi fixed-arg-count return-type ret-is-arg? arg-type ...)

   where `cached` is filled with a bytevector that starts as a
   `ffi_cif*` and has all of its associated data, fix-arg-count is 0
   for a non-varrags function, and a type is one of

     - a fixnum for an atomic: ffi_typerep_void, ffi_typerep_uint8, ...
     - a boxed fixnum representing a pointer to an atomic
     - a vector representing a struct (passed by copying)
     - a list of types representing a union
     - a (non-list) pair of a type and a count for an array
*/

# define RET_TYPE_INDEX 3
# define RET_IS_ARG_INDEX 4
# define ARG_TYPE_START_INDEX 5

typedef struct alloc_state {
  /* to allocate exactly as much as needed in a single bytevector,
     we'll decode in two passes, where the first pass result is discarded
     except for the size */
  iptr alloc_size;
  ptr bv;
} alloc_state;

static ffi_type *decode_type(alloc_state *alloc, ptr type, ffi_abi abi, IBOOL *all_float);
static void *alloc_for_ffi(alloc_state *alloc, iptr sz);
static void closure_callback(ffi_cif *cif, void *ret, void **args, void *user_data);

ffi_type *decode_type(alloc_state *alloc, ptr type, ffi_abi abi, IBOOL *all_float) {

  if (Sboxp(type))
    type = Sunbox(type);

  if (Sfixnump(type)) {
    ffi_type *out;
    IBOOL is_float = 0;
    switch(UNFIX(type)) {
    case ffi_typerep_void:
      out = &ffi_type_void;
      break;
    case ffi_typerep_uint8:
      out = &ffi_type_uint8;
      break;
    case ffi_typerep_sint8:
      out = &ffi_type_sint8;
      break;
    case ffi_typerep_uint16:
      out = &ffi_type_uint16;
      break;
    case ffi_typerep_sint16:
      out = &ffi_type_sint16;
      break;
    case ffi_typerep_uint32:
      out = &ffi_type_uint32;
      break;
    case ffi_typerep_sint32:
      out = &ffi_type_sint32;
      break;
    case ffi_typerep_uint64:
      out = &ffi_type_uint64;
      break;
    case ffi_typerep_sint64:
      out = &ffi_type_sint64;
      break;
    case ffi_typerep_float:
      is_float = 1;
      out = &ffi_type_float;
      break;
    case ffi_typerep_double:
      is_float = 1;
      out = &ffi_type_double;
      break;
    default:
      out = &ffi_type_pointer;
      break;
    }
    if (!is_float)
      *all_float = 0;
    return out;
  } else if (Svectorp(type)) {
    /* struct */
    iptr i, len = Svector_length(type);
    ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type)), *elem_out;
    ffi_type **elements = (ffi_type **)alloc_for_ffi(alloc, (len+1) * sizeof(ffi_type*));

    for (i = 0; i < len; i++) {
      ptr a = Svector_ref(type, i);
      elem_out = decode_type(alloc, a, abi, all_float);
      elements[i] = elem_out;
    }
    elements[len] = NULL;

    out->size = 0;
    out->alignment = 0;
    out->type = FFI_TYPE_STRUCT;
    out->elements = elements;

    return out;
  } else if (Spairp(type)) {
    ptr rest = Scdr(type);
    if (Spairp(rest) || (rest == Snil)) {
      /* union */

      /* libffi doesn't support union types, so we try to make a
         reasonable approximation. The calling convention of a union type
         mostly likely depends on of the maximum size of all alternatives
         and whether it's floating-point or not. Synthesize a struct that
         is big enough and composed of only floats if the union
         alternative are only floats or integers otherwise. This is not
         guaranteed to be right, but it has a chance at working. */
      IBOOL union_all_float = 1;
      int align = 1;
      size_t sz = 0;
      iptr count;
      ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type));
      ffi_type *elem_out, **elements;
      ffi_cif cif;
      
      /* find max required alignment and size: */
      while (type != Snil) {
        elem_out = decode_type(alloc, Scar(type), abi, &union_all_float);

        ffi_prep_cif(&cif, abi, 0, elem_out, NULL);
        if (elem_out->alignment > align)
          align = elem_out->alignment;
        if (elem_out->size > sz)
          sz = elem_out->size;
        
        type = Scdr(type);
      }

      if (!union_all_float)
        *all_float = 0;

      /* round size up to alignment: */
      if ((sz % align) != 0) {
        sz += (align - (sz % align));
      }

      /* Synthesize element list */
      count = 0;
      elements = NULL;
      while (!elements) { /* iterates exactly 2 times */
        if (count)
          elements = (ffi_type **)alloc_for_ffi(alloc, (count+1) * sizeof(ffi_type*));
        count = 0;

        if (!union_all_float) {
          /* build a struct out of integers */
          size_t remain_sz = sz;
          while (remain_sz >= 8) {
            if (elements)
              elements[count] = &ffi_type_sint64;
            remain_sz -= 8;
            count++;
          }
          while (remain_sz >= 4) {
            if (elements)
              elements[count] = &ffi_type_sint32;
            remain_sz -= 4;
            count++;
          }
          while (remain_sz >= 2) {
            if (elements)
              elements[count] = &ffi_type_sint16;
            remain_sz -= 2;
            count++;
          }
          while (remain_sz) {
            if (elements)
              elements[count] = &ffi_type_sint8;
            remain_sz -= 1;
            count++;
          }
          /* remain_sz should be 0 at this point */
        } else {
          /* build a struct out of doubles and floats */
          size_t remain_sz = sz;
          while (remain_sz >= sizeof(double)) {
            if (elements)
              elements[count] = &ffi_type_double;
            remain_sz -= sizeof(double);
            count++;
          }
          while (remain_sz >= sizeof(float)) {
            if (elements)
              elements[count] = &ffi_type_float;
            remain_sz -= sizeof(float);
            count++;
          }
          /* remain_sz should be 0 at this point */
        }
      }

      elements[count] = NULL;

      out->size = sz;
      out->alignment = align;
      out->type = FFI_TYPE_STRUCT;
      out->elements = elements;

      return out;
    } else {
      /* array */
#     if defined(__aarch64__)
#       define SMALL_ARRAY_THRESHOLD 64
#     else
#       define SMALL_ARRAY_THRESHOLD 32
#     endif

      /* libffi doesn't seem to support array types, but we try to make
         libffi work anyway by making a structure type that is used when
         an array appears as a struct field. If the array size is 4 or
         less, or if the total size is SMALL_ARRAY_THRESHOLD bytes or
         less, then we make a full `elements' array, because the x86_64
         ABI always shifts to memory mode after 32 bytes and the AArch64
         ABI shifts after 64 bytes.

         For a non-small element, we still put FFI_TYPE_STRUCT in
         out->type but make an elements array that contains a single
         instance of the element type, which seems to work ok. */
      ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type));
      ffi_type *elem_out, **elements;
      ffi_cif cif;
      iptr len;
 
      elem_out = decode_type(alloc, Scar(type), abi, all_float);
      len = UNFIX(Scdr(type));

      ffi_prep_cif(&cif, abi, 0, elem_out, NULL);

      out->size = elem_out->size * len;
      out->alignment = elem_out->alignment;
      out->type = FFI_TYPE_STRUCT;

      if ((out->size <= SMALL_ARRAY_THRESHOLD) || (len <= 4)) {
        iptr i;
        elements = alloc_for_ffi(alloc, (len + 1) * sizeof(ffi_type*));
        for (i = 0; i < len; i++)
          elements[i] = elem_out;
        elements[len] = NULL;
      } else {
        elements = alloc_for_ffi(alloc, 2 * sizeof(ffi_type*));
        elements[0] = elem_out;
        elements[1] = NULL;
      }
      out->elements  = elements;

      return out;
    }
  } else {
    return &ffi_type_pointer;
  }
}

static void *alloc_for_ffi(alloc_state *alloc, iptr sz) {
  void *result;

  sz = ptr_align(sz);

  if (alloc->alloc_size + sz > Sbytevector_length(alloc->bv))
    alloc->bv = S_bytevector((Sbytevector_length(alloc->bv) + sz) * 2);
  
  result = TO_VOIDP((uptr)TO_PTR(Sbytevector_data(alloc->bv)) + alloc->alloc_size);
  alloc->alloc_size += sz;
  return result;
}

ffi_cif *make_cif(ptr types) {
  ptr cached;
  ffi_abi abi;
  int n_var_req;
  alloc_state alloc;
  ffi_cif *cif;
  ffi_type *ret, **args;
  IBOOL all_float;
  iptr i, len, n_args;

  /* `types` is #(cached abi fixed-arg-count return-type arg-type ...) */

  cached = Svector_ref(types, 0);
  if (cached != Sfalse)
    return (ffi_cif *)Sbytevector_data(cached);

  len = Svector_length(types);
  n_args = len - ARG_TYPE_START_INDEX;

  abi = UNFIX(Svector_ref(types, 1));
  if (abi == ffi_default_abi)
    abi = FFI_DEFAULT_ABI;
  n_var_req = UNFIX(Svector_ref(types, 2));

  /* first pass to get exact allocation size: */

  alloc.alloc_size = 0;
  alloc.bv = S_bytevector(sizeof(ffi_cif));

  (void)alloc_for_ffi(&alloc, sizeof(ffi_cif));
  (void)alloc_for_ffi(&alloc, n_args * sizeof(ffi_type*));

  all_float = 1;
  (void)decode_type(&alloc, Svector_ref(types, RET_TYPE_INDEX), abi, &all_float);
  
  for (i = 0; i < n_args; i++) {
    all_float = 1;
    (void)decode_type(&alloc, Svector_ref(types, i+ARG_TYPE_START_INDEX), abi, &all_float);
  }

  /* now we know the right size, to allocate as immobile */
  cached = S_bytevector2(get_thread_context(), alloc.alloc_size, space_immobile_data);
  S_immobilize_object(cached);

  alloc.alloc_size = 0;
  alloc.bv = cached;

  cif = alloc_for_ffi(&alloc, sizeof(ffi_cif));
  args = alloc_for_ffi(&alloc, n_args * sizeof(ffi_type*));

  all_float = 1;
  ret = decode_type(&alloc, Svector_ref(types, RET_TYPE_INDEX), abi, &all_float);

  for (i = 0; i < n_args; i++) {
    all_float = 1;
    args[i] = decode_type(&alloc, Svector_ref(types, i+ARG_TYPE_START_INDEX), abi, &all_float);
  }

  if (n_var_req > 0)
    ffi_prep_cif_var(cif, abi, n_var_req, n_args, ret, args);
  else
    ffi_prep_cif(cif, abi, n_args, ret, args);

  Svector_set(types, 0, cached);

  return cif;
}

void S_ffi_call(ptr types, ptr proc, ptr *stack) {
  ptr *stack_start = stack;
  ffi_cif *cif = make_cif(types);
  iptr len = Svector_length(types), i;
  iptr n_args = len - ARG_TYPE_START_INDEX;
  void *rvalue, **args = TO_VOIDP((uptr)TO_PTR(stack) + (n_args * 8));

  if (Svector_ref(types, RET_IS_ARG_INDEX) != Sfalse) {
    rvalue = TO_VOIDP(*stack);
    stack++;
  } else
    rvalue = stack;

  for (i = 0; i < n_args; i++) {
    ptr type = Svector_ref(types, i + ARG_TYPE_START_INDEX);
    if (Sfixnump(type)) {
      args[i] = stack;
      /* adjust arguments that are not ptr-sized or not encoded as doubles/iptrs */
      switch(UNFIX(type)) {
#   ifdef PORTABLE_BYTECODE_BIGENDIAN
      case ffi_typered_uint8:
      case ffi_typered_sint8:
        {
          U8 s;
          s = *stack;
          memcpy(stack, &s, sizeof(U8));
        }
        break;
      case ffi_typered_uint16:
      case ffi_typered_sint16:
        {
          U16 s;
          s = *stack;
          memcpy(stack, &s, sizeof(U16));
        }
        break;
      case ffi_typered_uint32:
      case ffi_typered_sint32:
        {
          U32 s;
          s = *stack;
          memcpy(stack, &s, sizeof(U32));
        }
        break;
#   endif
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
#         ifdef PORTABLE_BYTECODE_BIGENDIAN
          {
            ptr lo = stack[0];
            stack[0] = stack[1];
            stack[1] = lo;
          }
#         endif
          stack += (sizeof(I64) - sizeof(ptr)) >> log2_ptr_bytes;
        }
        break;
      case ffi_typerep_double:
        stack += (sizeof(double) - sizeof(ptr)) >> log2_ptr_bytes;
        break;
      case ffi_typerep_float:
        {
          float f;
          double d;
          memcpy(&d, stack, sizeof(double));
          f = d;
          memcpy(stack, &f, sizeof(float));
          *(float *)stack = *(double *)stack;
        }
        stack += (sizeof(double) - sizeof(ptr)) >> log2_ptr_bytes;
        break;
      }
    } else
      args[i] = *(void **)stack;
    stack++;
  }

  ffi_call(cif, TO_VOIDP(proc), rvalue, args);

  /* fix up result for certain types: */
  {
    ptr ret_type = Svector_ref(types, RET_TYPE_INDEX);
    if (Sfixnump(ret_type)) {
      /* adjust arguments that are not ptr-sized or not encoded as doubles/iptrs */
      switch(UNFIX(ret_type)) {
#   ifdef PORTABLE_BYTECODE_BIGENDIAN
      case ffi_typered_uint8:
      case ffi_typered_sint8:
        {
          U8 s;
          memcpy(&s, stack_start, &s, sizeof(U8));
          *stack = (ptr)s;
        }
        break;
      case ffi_typered_uint16:
      case ffi_typered_sint16:
        {
          U16 s;
          memcpy(&s, stack_start, &s, sizeof(U16));
          *stack = (ptr)s;
        }
        break;
      case ffi_typered_uint32:
      case ffi_typered_sint32:
        {
          U32 s;
          memcpy(&s, stack_start, &s, sizeof(U32));
          *stack = (ptr)s;
        }
        break;
#   endif
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
#         ifdef PORTABLE_BYTECODE_BIGENDIAN
          {
            ptr lo = stack[0];
            stack[0] = stack[1];
            stack[1] = lo;
          }
#         endif
        }
        break;
      case ffi_typerep_float:
        {
          float f;
          double d;
          memcpy(&f, stack_start, sizeof(float));
          d = f;
          memcpy(stack_start, &d, sizeof(double));
        }
        break;
      }
    }
  }
}

ptr S_ffi_closure(ptr types, ptr proc) {
  ffi_cif *cif = make_cif(types);
  ffi_closure *closure;
  ptr vec;
  void *code;
  
  closure = ffi_closure_alloc(sizeof(ffi_closure), &code);

  vec = S_vector_in(get_thread_context(), space_immobile_impure, 0, 3);
  S_immobilize_object(vec);

  Svector_set(vec, 0, proc);
  Svector_set(vec, 1, types);
  Svector_set(vec, 2, Sunsigned64((uptr)TO_PTR(code)));

  ffi_prep_closure_loc(closure, cif, closure_callback, TO_VOIDP(vec), code);
    
  return vec;
}

static void closure_callback(UNUSED ffi_cif *cif, void *ret, void **args, void *user_data) {
  ptr vec = (ptr)user_data;
  ptr types = Svector_ref(vec, 1), type;
  ptr tc = get_thread_context();
  ptr *stack_start = S_get_argres(), *stack = stack_start;
  iptr len = Svector_length(types), i;
  iptr n_args = len - ARG_TYPE_START_INDEX;
  IBOOL ret_is_arg;

  if (Svector_ref(types, RET_IS_ARG_INDEX) != Sfalse) {
    *stack = TO_PTR(ret);
    stack++;
    ret_is_arg = 1;
  } else
    ret_is_arg = 0;
 
  /* Move args in `args` to "stack" space */
  for (i = 0; i < n_args; i++) {
    type = Svector_ref(types, i + ARG_TYPE_START_INDEX);
    if (Sfixnump(type)) {
      switch(UNFIX(type)) {
      case ffi_typerep_uint8:
        *stack = (ptr)*(U8 *)args[i];
        break;
      case ffi_typerep_sint8:
        *stack = (ptr)*(I8 *)args[i];
        break;
      case ffi_typerep_uint16:
        *stack = (ptr)*(U16 *)args[i];
        break;
      case ffi_typerep_sint16:
        *stack = (ptr)*(I16 *)args[i];
        break;
      case ffi_typerep_uint32:
        *stack = (ptr)*(U32 *)args[i];
        break;
      case ffi_typerep_sint32:
        *stack = (ptr)*(I32 *)args[i];
        break;
      case ffi_typerep_uint64:
        if (sizeof(U64) > sizeof(ptr)) {
          stack[0] = (ptr)((*(U64 *)args[i]) >> 32);
          stack[1] = (ptr)*(U64 *)args[i];
          stack++;
        }
        break;
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
          stack[0] = (ptr)((*(I64 *)args[i]) >> 32);
          stack[1] = (ptr)*(I64 *)args[i];
          stack++;
        }
        break;
      case ffi_typerep_float:
        *(double *)stack = *(float *)args[i];
        if (sizeof(double) > sizeof(ptr))
          stack++;
        break;
      case ffi_typerep_double:
        *(double *)stack = *(double *)args[i];
        if (sizeof(double) > sizeof(ptr))
          stack++;
        break;
      default:
        *stack = *(ptr *)args[i];
        break;
      }
    } else {
      /* all boxed or compound values are passed as an address */
      *stack = TO_PTR(args[i]);
    }
    stack++;
  }

  S_generic_invoke(tc, Svector_ref(vec, 0));

  if (!ret_is_arg) {
    /* move result to "stack" */
    type = Svector_ref(types, RET_IS_ARG_INDEX);
  
    if (Sfixnump(type)) {
      switch(UNFIX(type)) {
      case ffi_typerep_uint8:
        *(U8 *)ret = *stack_start;
        break;
      case ffi_typerep_sint8:
        *(I8 *)ret = *stack_start;
        break;
      case ffi_typerep_uint16:
        *(U16 *)ret = *stack_start;
        break;
      case ffi_typerep_sint16:
        *(I16 *)ret = *stack_start;
        break;
      case ffi_typerep_uint32:
        *(U32 *)ret = *stack_start;
        break;
      case ffi_typerep_sint32:
        *(I32 *)ret = *stack_start;
        break;
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(U64) > sizeof(ptr)) {
#        ifdef PORTABLE_BYTECODE_BIGENDIAN
          ((U32 *)ret)[0] = stack_start[0];
          ((U32 *)ret)[1] = stack_start[1];
#        else
          ((U32 *)ret)[1] = stack_start[0];
          ((U32 *)ret)[0] = stack_start[1];
#        endif
        } else {
          *(U64 *)ret = *stack_start;
        }
        break;
      case ffi_typerep_float:
        *(float *)ret = *(double *)stack_start;
        break;
      case ffi_typerep_double:
        *(double *)ret = *(double *)stack_start;
        break;
      default:
        *(ptr *)ret = *stack_start;
        break;
      }
    } else {
      *(ptr *)ret = *stack_start;
    }
  }
}

#else

/* libffi disabled */

void S_ffi_call(UNUSED ptr types, UNUSED ptr proc, UNUSED ptr *stack) {
  S_error("foreign-procedure", "protocol not supported (libffi unavailable)");
}

ptr S_ffi_closure(UNUSED ptr types, UNUSED ptr proc) {
  S_error("foreign-callable", "not supported (libffi unavailable)");
}

#endif
#endif
