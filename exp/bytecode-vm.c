#include <stdint.h>
#include "bytecode.h"

#include <math.h>
#include <string.h>
// XXX intel
#include "emmintrin.h"
#include "immintrin.h"
#include <stdio.h>

#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <time.h>

typedef union {
  uint8_t  ui8;
  uint16_t ui16;
  uint32_t ui32;
  int8_t   si8;
  int16_t  si16;
  int32_t  si32;
  float    f32;
} reg;

// XXX intel -> ymm
typedef union {
  uint8_t  b8[32];
  uint16_t b16[16];
  uint32_t b32[8];
} buf;

typedef buf bufset[4];

typedef OP_T PROM[];

typedef bufset *MEM;

#define dprintf(...)

uint32_t vm_run( PROM prom, size_t prom_size,
                 MEM rom, MEM ram, MEM stack, MEM sys, MEM debug,
                 uint32_t ipc, uint32_t *pc_stack ) {
  uint32_t pc = ipc;
  uint8_t status = 0;
  bufset bs = { 0 };
  reg rs[8] = { 0 };
  uint32_t isp = 0;

  static void *ops[] = { &&vm_add, &&vm_sub, &&vm_mul, &&vm_div, &&vm_rem, &&vm_fprim, &&vm_shift, &&vm_bitwise, &&vm_conv, &&vm_cmp, &&vm_control, &&vm_return, &&vm_halt, &&vm_control_imm, &&vm_control_immx, &&vm_load_imm, &&vm_load_immx, &&vm_select, &&vm_buf_mem, &&vm_mem_fence, &&vm_buf_set, &&vm_buf_read, &&vm_buf_all, &&vm_stack_mov };
  OP_T op;

#define VM_TOP { op = prom[pc++]; goto *(ops[OP_CODE(op)]); }

  VM_TOP;
  while (1) {
 vm_add: {
      dprintf("ADD\n");
      uint8_t dst = OP_ADD_DST(op);
      uint8_t lhs = OP_ADD_LHS(op);
      uint8_t rhs = OP_ADD_RHS(op);
      if ( OP_ADD_MODE(op) == ARITH_MODE_INT ) {
        rs[dst].ui32 = rs[lhs].ui32 + rs[rhs].ui32; VM_TOP;
      } else {
        rs[dst].f32  = rs[lhs].f32  + rs[rhs].f32;  VM_TOP;
      }
    }

  vm_sub: {
      dprintf("SUB\n");
      uint8_t dst = OP_SUB_DST(op);
      uint8_t lhs = OP_SUB_LHS(op);
      uint8_t rhs = OP_SUB_RHS(op);
      if ( OP_SUB_MODE(op) == ARITH_MODE_INT ) {
        rs[dst].ui32 = rs[lhs].ui32 - rs[rhs].ui32; VM_TOP;
      } else {
        rs[dst].f32  = rs[lhs].f32  - rs[rhs].f32;  VM_TOP;
      }
    }

  vm_mul: {
      dprintf("MUL\n");
      uint8_t dst = OP_MUL_DST(op);
      uint8_t lhs = OP_MUL_LHS(op);
      uint8_t rhs = OP_MUL_RHS(op);
      if ( OP_MUL_MODE(op) == ARITH_MODE_INT ) {
        rs[dst].ui32 = rs[lhs].ui32 * rs[rhs].ui32; VM_TOP;
      } else {
        rs[dst].f32  = rs[lhs].f32  * rs[rhs].f32;  VM_TOP;
      }
    }

  vm_div: {
      switch ( OP_DIV_MODE(op) ) {
      case  DIV_MODE_UINT: rs[OP_DIV_DST(op)].ui32 = rs[OP_DIV_LHS(op)].ui32 / rs[OP_DIV_RHS(op)].ui32; VM_TOP;
      case  DIV_MODE_SINT: rs[OP_DIV_DST(op)].si32 = rs[OP_DIV_LHS(op)].si32 / rs[OP_DIV_RHS(op)].si32; VM_TOP;
      case DIV_MODE_FLOAT: rs[OP_DIV_DST(op)].f32  = rs[OP_DIV_LHS(op)].f32  / rs[OP_DIV_RHS(op)].f32;  VM_TOP;
      }
    }

  vm_rem: {
      switch ( OP_REM_MODE(op) ) {
      case  DIV_MODE_UINT: rs[OP_REM_DST(op)].ui32 = rs[OP_REM_LHS(op)].ui32 % rs[OP_REM_RHS(op)].ui32; VM_TOP;
      case  DIV_MODE_SINT: rs[OP_REM_DST(op)].si32 = rs[OP_REM_LHS(op)].si32 % rs[OP_REM_RHS(op)].si32; VM_TOP;
      case DIV_MODE_FLOAT: rs[OP_REM_DST(op)].f32  = fmodf( rs[OP_REM_LHS(op)].f32, rs[OP_REM_RHS(op)].f32 ); VM_TOP;
      }
    }

  vm_fprim: {
      switch ( OP_FPRIM_MODE(op) ) {
      case  FPRIM_MODE_SQRT: rs[OP_FPRIM_DST(op)].f32 = sqrtf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case   FPRIM_MODE_SIN: rs[OP_FPRIM_DST(op)].f32 = sinf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case   FPRIM_MODE_COS: rs[OP_FPRIM_DST(op)].f32 = cosf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case   FPRIM_MODE_EXP: rs[OP_FPRIM_DST(op)].f32 = expf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case  FPRIM_MODE_EXP2: rs[OP_FPRIM_DST(op)].f32 = exp2f( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case   FPRIM_MODE_LOG: rs[OP_FPRIM_DST(op)].f32 = logf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case FPRIM_MODE_LOG10: rs[OP_FPRIM_DST(op)].f32 = log10f( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case  FPRIM_MODE_LOG2: rs[OP_FPRIM_DST(op)].f32 = log2f( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case   FPRIM_MODE_ABS: rs[OP_FPRIM_DST(op)].f32 = fabs( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case FPRIM_MODE_FLOOR: rs[OP_FPRIM_DST(op)].f32 = floorf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case  FPRIM_MODE_CEIL: rs[OP_FPRIM_DST(op)].f32 = ceilf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case FPRIM_MODE_TRUNC: rs[OP_FPRIM_DST(op)].f32 = truncf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case  FPRIM_MODE_RINT: rs[OP_FPRIM_DST(op)].f32 = rintf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case FPRIM_MODE_NEARBYINT: rs[OP_FPRIM_DST(op)].f32 = nearbyintf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      case FPRIM_MODE_ROUND: rs[OP_FPRIM_DST(op)].f32 = roundf( rs[OP_FPRIM_SRC(op)].f32 ); VM_TOP;
      }
    }

  vm_shift: {
      switch ( OP_SHIFT_MODE(op) ) {
      case             SHIFT_MODE_LEFT: rs[OP_SHIFT_DST(op)].ui32 = rs[OP_SHIFT_LHS(op)].ui32 << rs[OP_SHIFT_RHS(op)].ui32; VM_TOP;
      case    SHIFT_MODE_RIGHT_LOGICAL: rs[OP_SHIFT_DST(op)].ui32 = rs[OP_SHIFT_LHS(op)].ui32 >> rs[OP_SHIFT_RHS(op)].ui32; VM_TOP;
      case SHIFT_MODE_RIGHT_ARITHMETIC: rs[OP_SHIFT_DST(op)].si32 = rs[OP_SHIFT_LHS(op)].si32 >> rs[OP_SHIFT_RHS(op)].si32; VM_TOP;
      }
    }

  vm_bitwise: {
      switch ( OP_BITWISE_MODE(op) ) {
      case  BIT_MODE_AND: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 & rs[OP_BITWISE_RHS(op)].ui32; VM_TOP;
      case  BIT_MODE_IOR: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 | rs[OP_BITWISE_RHS(op)].ui32; VM_TOP;
      case  BIT_MODE_XOR: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 ^ rs[OP_BITWISE_RHS(op)].ui32; VM_TOP;
      case BIT_MODE_NAND: rs[OP_BITWISE_DST(op)].ui32 = ~ ( rs[OP_BITWISE_LHS(op)].ui32 & rs[OP_BITWISE_RHS(op)].ui32 ); VM_TOP;
      }
    }

  vm_conv: {
      switch ( OP_CONV_TY(op) ) {
      case    CONV_TY_U8_TO_U16: rs[OP_CONV_DST(op)].ui16 = rs[OP_CONV_SRC(op)].ui8; VM_TOP;
      case    CONV_TY_U8_TO_U32: rs[OP_CONV_DST(op)].ui32 = rs[OP_CONV_SRC(op)].ui8; VM_TOP;
      case  CONV_TY_U8_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui8; VM_TOP;
      case    CONV_TY_S8_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].si8; VM_TOP;
      case    CONV_TY_S8_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].si8; VM_TOP;
      case  CONV_TY_S8_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si8; VM_TOP;
      case    CONV_TY_U16_TO_U8: rs[OP_CONV_DST(op)].ui8  = rs[OP_CONV_SRC(op)].ui16; VM_TOP;
      case   CONV_TY_U16_TO_U32: rs[OP_CONV_DST(op)].ui32 = rs[OP_CONV_SRC(op)].ui16; VM_TOP;
      case CONV_TY_U16_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui16; VM_TOP;
      case    CONV_TY_S16_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].si16; VM_TOP;
      case   CONV_TY_S16_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].si16; VM_TOP;
      case CONV_TY_S16_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si16; VM_TOP;
      case    CONV_TY_U32_TO_U8: rs[OP_CONV_DST(op)].ui8  = rs[OP_CONV_SRC(op)].ui32; VM_TOP;
      case   CONV_TY_U32_TO_U16: rs[OP_CONV_DST(op)].ui16 = rs[OP_CONV_SRC(op)].ui32; VM_TOP;
      case CONV_TY_U32_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui32; VM_TOP;
      case    CONV_TY_S32_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].si32; VM_TOP;
      case   CONV_TY_S32_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].si32; VM_TOP;
      case CONV_TY_S32_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si32; VM_TOP;
      case  CONV_TY_FLOAT_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].f32; VM_TOP;
      case CONV_TY_FLOAT_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].f32; VM_TOP;
      case CONV_TY_FLOAT_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].f32; VM_TOP;
      }
    }

  vm_cmp: {
      dprintf("CMP(%u,%u,%u)\n", OP_CMP_LHS(op), OP_CMP_MODE(op), OP_CMP_RHS(op));
      switch ( OP_CMP_MODE(op) ) {
      case CMP_CODE_I_FALSE: status = 0; VM_TOP;
      case    CMP_CODE_I_EQ: status = rs[OP_CMP_LHS(op)].ui32 == rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case    CMP_CODE_I_NE: status = rs[OP_CMP_LHS(op)].ui32 != rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case   CMP_CODE_I_UGT: status = rs[OP_CMP_LHS(op)].ui32 >  rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case   CMP_CODE_I_UGE: status = rs[OP_CMP_LHS(op)].ui32 >= rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case   CMP_CODE_I_ULT: status = rs[OP_CMP_LHS(op)].ui32 <  rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case   CMP_CODE_I_ULE: status = rs[OP_CMP_LHS(op)].ui32 <= rs[OP_CMP_RHS(op)].ui32; VM_TOP;
      case   CMP_CODE_I_SGT: status = rs[OP_CMP_LHS(op)].si32 >  rs[OP_CMP_RHS(op)].si32; VM_TOP;
      case   CMP_CODE_I_SGE: status = rs[OP_CMP_LHS(op)].si32 >= rs[OP_CMP_RHS(op)].si32; VM_TOP;
      case   CMP_CODE_I_SLT: status = rs[OP_CMP_LHS(op)].si32 <  rs[OP_CMP_RHS(op)].si32; VM_TOP;
      case   CMP_CODE_I_SLE: status = rs[OP_CMP_LHS(op)].si32 <= rs[OP_CMP_RHS(op)].si32; VM_TOP;
      case  CMP_CODE_I_TRUE: status = 1; VM_TOP;
      case CMP_CODE_F_FALSE: status = 0; VM_TOP;
      case   CMP_CODE_F_OEQ: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 == rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_OGT: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 >  rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_OGE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 >= rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_OLT: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 <  rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_OLE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 <= rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_ONE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 != rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_ORD: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32); VM_TOP;
      case   CMP_CODE_F_UEQ: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 == rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_UGT: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 >  rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_UGE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 >= rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_ULT: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 <  rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_ULE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 <= rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_UNE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 != rs[OP_CMP_RHS(op)].f32; VM_TOP;
      case   CMP_CODE_F_UNO: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32); VM_TOP;
      case  CMP_CODE_F_TRUE: status = 1; VM_TOP;
      }
    }

  vm_control: {
      if ( (OP_CONTROL_COND(op) == CONTROL_COND_ALWAYS) | status ) {
        if ( OP_CONTROL_MODE(op) == CONTROL_MODE_CALL ) {
          *(++pc_stack) = pc;
        }
        pc = rs[OP_CONTROL_ADDR(op)].ui32;
      }
      VM_TOP;
    }
    
  vm_control_imm: {
      if ( (OP_CONTROL_IMM_COND(op) == CONTROL_COND_ALWAYS) | status ) {
        if ( OP_CONTROL_IMM_MODE(op) == CONTROL_MODE_CALL ) {
          *(++pc_stack) = pc;
        }
        pc = pc - 1 + (OP_CONTROL_IMM_OFFSET(op) - 256);
      }
      VM_TOP;
    }
    
  vm_control_immx: {
      pc++;
      if ( (OP_CONTROL_IMM_COND(op) == CONTROL_COND_ALWAYS) | status ) {
        if ( OP_CONTROL_IMMX_MODE(op) == CONTROL_MODE_CALL ) {
          *(++pc_stack) = pc;
        }
        pc = (OP_CONTROL_IMM_OFFSET(op) << 16) + prom[pc-1];
      }
      VM_TOP;
    }

  vm_return: {
      pc = *(pc_stack--);
      VM_TOP;
    }

  vm_halt: {
      dprintf("HALT\n");
      dprintf("\tr2 = %u\n", rs[2].ui32);
      return rs[2].ui32;
    }

  vm_load_imm: {
      rs[OP_LOAD_IMM_DST(op)].ui32 = OP_LOAD_IMM_VAL(op);
      VM_TOP;
    }

  vm_load_immx: {
      rs[OP_LOAD_IMMX_DST(op)].ui32 = (OP_LOAD_IMMX_HI(op)<<16) + prom[pc++];
      VM_TOP;
    }

  vm_select: {
      rs[OP_SELECT_DST(op)].ui32 = status ? rs[OP_SELECT_X(op)].ui32 : rs[OP_SELECT_Y(op)].ui32;
      VM_TOP;
    }

  vm_buf_mem: {
      MEM space;
      switch ( OP_BUF_MEM_SRC(op) ) {
      case LOAD_SPACE_ROM: // STORE_SPACE_DEBUG:
        space = OP_BUF_MEM_MODE(op) == BUF_MEM_MODE_READ ? rom : debug;
        break;
      case LOAD_SPACE_RAM: // STORE_SPACE_RAM:
        space = ram;
        break;
      case LOAD_SPACE_STACK: // STORE_SPACE_STACK:
        space = stack;
        break;
      case LOAD_SPACE_SYS: // STORE_SPACE_SYS:
        space = sys;
        break;
      }
      void *dst = bs + OP_BUF_MEM_DST(op);
      void *src = space + rs[OP_BUF_MEM_ADDR(op)].ui32;
      if ( OP_BUF_MEM_MODE(op) == BUF_MEM_MODE_WRITE ) {
        void *tmp = dst;
        dst = src;
        src = tmp;
      }
      memcpy( dst, src, sizeof(buf) );
      // XXX OP_BUF_MEM_SYNC(op)
      // intel -> _mm256_stream_load_si256
      //       -> _mm256_stream_si256
      VM_TOP;
    }
      
  vm_mem_fence: {
      // XXX intel
      // _mm_mfence();
      VM_TOP;
    }

  vm_buf_set: {
      if ( bf_get( OP_BUF_SET_IDX(op), 5, 1 ) == 1 ) { // 1-byte
        uint8_t target = bf_get( OP_BUF_SET_IDX(op), 0, 5 );
        bs[OP_BUF_SET_DST(op)].b8[target] = rs[OP_BUF_SET_SRC(op)].ui8;
      } else if ( bf_get( OP_BUF_SET_IDX(op), 4, 2 ) == 1 ) { // 2-byte
        uint8_t target = bf_get( OP_BUF_SET_IDX(op), 0, 4 );
        bs[OP_BUF_SET_DST(op)].b16[target] = rs[OP_BUF_SET_SRC(op)].ui16;
      } else /* if ( bf_get( OP_BUF_SET_IDX(op), 4, 2 ) == 0 ) */ { // 4-byte
        uint8_t target = bf_get( OP_BUF_SET_IDX(op), 0, 3 );
        bs[OP_BUF_SET_DST(op)].b32[target] = rs[OP_BUF_SET_SRC(op)].ui32;
      }
      VM_TOP;
    }

  vm_buf_read: {
      if ( bf_get( OP_BUF_READ_IDX(op), 5, 1 ) == 1 ) { // 1-byte
        uint8_t target = bf_get( OP_BUF_READ_IDX(op), 0, 5 );
        rs[OP_BUF_READ_SRC(op)].ui8 = bs[OP_BUF_READ_DST(op)].b8[target];
      } else if ( bf_get( OP_BUF_READ_IDX(op), 4, 2 ) == 1 ) { // 2-byte
        uint8_t target = bf_get( OP_BUF_READ_IDX(op), 0, 4 );
        rs[OP_BUF_READ_SRC(op)].ui16 = bs[OP_BUF_READ_DST(op)].b16[target];
      } else /* if ( bf_get( OP_BUF_READ_IDX(op), 4, 2 ) == 0 ) */ { // 4-byte
        uint8_t target = bf_get( OP_BUF_READ_IDX(op), 0, 3 );
        rs[OP_BUF_READ_SRC(op)].ui32 = bs[OP_BUF_READ_DST(op)].b32[target];
      }
      VM_TOP;
    }

  vm_buf_all: {
      void *dst = &rs;
      void *src = &bs[OP_BUF_ALL_DST(op)];
      if ( OP_BUF_ALL_MODE(op) == BUF_ALL_MODE_WRITE ) {
        void *tmp = dst;
        dst = src;
        src = tmp;
      }
      memcpy(dst, src, sizeof(buf));
      VM_TOP;
    }

  vm_stack_mov: {
      if ( OP_STACK_MOV_DIR(op) == STACK_DIR_READ ) {
        rs[OP_STACK_MOV_DST(op)].ui32 = isp;
        VM_TOP;
      } else {
        isp = rs[OP_STACK_MOV_DST(op)].ui32;
        VM_TOP;
      }
    }
  }
}

uint32_t vm2_run(uint32_t N, uint32_t m) {
  uint8_t status = 0;
  bufset bs = { 0 };
  reg r0, r1, r2, r3, r4, r5, r6, r7;
  uint32_t isp = 0;

 i0: r4.ui32 = N;
 i1:
 i2: r0.ui32 = 0;
 i3: r1.ui32 = m;
 i4: r2.ui32 = 1;
 i5: r3.ui32 = 1;
 i6: status = r0.ui32 == r1.ui32;
 i7: if (status) { goto i11; }
 i8: r2.ui32 = r1.ui32 * r2.ui32;
 i9: r1.ui32 = r1.ui32 - r3.ui32;
 i10: goto i6;
 i11: r4.ui32 = r4.ui32 - r3.ui32;
 i12: status = r0.ui32 != r4.ui32;
 i13: if (status) { goto i2; }
 i14: return r2.ui32;
}

int main ( int argc, char **argv ) {
  printf("reg    = %lu\n", sizeof(reg));
  printf("buf    = %lu\n", sizeof(buf));
  printf("OP_T   = %lu\n", sizeof(OP_T));

  uint8_t BYTECODE_p = argc == 2;

  char *bin_path = NULL;
  void *bin_ptr = NULL;
  FILE *bin_f = NULL;
  int bin_len = 0;
  size_t prom_size = 0;
  uint32_t N = 0;
  uint32_t m = 0;

  if ( BYTECODE_p ) {
    bin_path = argv[1];

    bin_f = fopen(bin_path, "r");
    if (bin_f == NULL) {
      return -1;
    }
    int bin_fd = fileno(bin_f);

    struct stat bin_st;
    fstat(bin_fd, &bin_st);
    bin_len = bin_st.st_size;

    bin_ptr =
      mmap(NULL, bin_len, PROT_EXEC | PROT_READ, MAP_FILE | MAP_PRIVATE, bin_fd, 0);
    if (bin_ptr == MAP_FAILED) {
      return -1;
    }
    prom_size = bin_len / 2;

  } else {
    N = atoi(argv[1]);
    m = atoi(argv[2]);
  }

  clock_t before = clock();
  uint32_t r =
    BYTECODE_p ? 
    vm_run( bin_ptr, prom_size, NULL, NULL, NULL, NULL, NULL, 0, NULL ) :
    vm2_run(N, m);
  clock_t after = clock();
  clock_t span = after - before;

  double fspan = span;
  double fcps = CLOCKS_PER_SEC;
  double fspan_ms = fspan/fcps * 1000.0;

  printf("%u => N(%u) in (%fms), 1 in (%fms)\n",
         r, N, fspan_ms, fspan_ms / ((double) N));

  if ( BYTECODE_p ) {
    if ( munmap(bin_ptr, bin_len) == -1 ) {
      return -1;
    }
    fclose(bin_f);
  }
  
  return 0;
}
