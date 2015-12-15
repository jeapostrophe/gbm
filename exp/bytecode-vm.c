#include <stdint.h>
#include "bytecode.h"

typedef union {
  uint8_t  ui8;
  uint16_t ui16;
  uint32_t ui32;
  int8_t   si8;
  int16_t  si16;
  int32_t  si32;
  float    f32;
} reg;

typedef reg regset[8];

// XXX intel -> ymm
typedef union {
  regset   all;
  uint8_t  b8[32];
  uint16_t b16[16];
  uint32_t b32[8];
  float    f32[8];
} buf;

typedef buf bufset[4];

typedef OP_T PROM[];

typedef bufset *MEM;

#include <math.h>
#include <string.h>
// XXX intel
#include "emmintrin.h"
#include "immintrin.h"

void vm_run( PROM prom,
             MEM rom, MEM ram, MEM stack, MEM sys, MEM debug,
             uint32_t ipc, uint32_t *pc_stack ) {
  uint32_t pc = ipc;
  uint8_t status = 0;
  bufset bs = { 0 };
  regset rs = { 0 };
  uint32_t isp = 0;

  while (1) {
    OP_T op = prom[pc];
    switch ( OP_CODE(op) ) {
    case OP_ADD:
      switch ( OP_ADD_MODE(op) ) {
      case   ARITH_MODE_INT: rs[OP_ADD_DST(op)].ui32 = rs[OP_ADD_LHS(op)].ui32 + rs[OP_ADD_RHS(op)].ui32; break;
      case ARITH_MODE_FLOAT: rs[OP_ADD_DST(op)].f32  = rs[OP_ADD_LHS(op)].f32  + rs[OP_ADD_RHS(op)].f32;  break;
      }
      pc++;
      break;
    case OP_SUB:
      switch ( OP_SUB_MODE(op) ) {
      case   ARITH_MODE_INT: rs[OP_SUB_DST(op)].ui32 = rs[OP_SUB_LHS(op)].ui32 - rs[OP_SUB_RHS(op)].ui32; break;
      case ARITH_MODE_FLOAT: rs[OP_SUB_DST(op)].f32  = rs[OP_SUB_LHS(op)].f32  - rs[OP_SUB_RHS(op)].f32;  break;
      }
      pc++;
      break;
    case OP_MUL:
      switch ( OP_MUL_MODE(op) ) {
      case   ARITH_MODE_INT: rs[OP_MUL_DST(op)].ui32 = rs[OP_MUL_LHS(op)].ui32 * rs[OP_MUL_RHS(op)].ui32; break;
      case ARITH_MODE_FLOAT: rs[OP_MUL_DST(op)].f32  = rs[OP_MUL_LHS(op)].f32  * rs[OP_MUL_RHS(op)].f32;  break;
      }
      pc++;
      break;
    case OP_DIV:
      switch ( OP_DIV_MODE(op) ) {
      case  DIV_MODE_UINT: rs[OP_DIV_DST(op)].ui32 = rs[OP_DIV_LHS(op)].ui32 / rs[OP_DIV_RHS(op)].ui32; break;
      case  DIV_MODE_SINT: rs[OP_DIV_DST(op)].si32 = rs[OP_DIV_LHS(op)].si32 / rs[OP_DIV_RHS(op)].si32; break;
      case DIV_MODE_FLOAT: rs[OP_DIV_DST(op)].f32  = rs[OP_DIV_LHS(op)].f32  / rs[OP_DIV_RHS(op)].f32;  break;
      }
      pc++;
      break;
    case OP_REM:
      switch ( OP_REM_MODE(op) ) {
      case  DIV_MODE_UINT: rs[OP_REM_DST(op)].ui32 = rs[OP_REM_LHS(op)].ui32 % rs[OP_REM_RHS(op)].ui32; break;
      case  DIV_MODE_SINT: rs[OP_REM_DST(op)].si32 = rs[OP_REM_LHS(op)].si32 % rs[OP_REM_RHS(op)].si32; break;
      case DIV_MODE_FLOAT: rs[OP_REM_DST(op)].f32  = fmodf( rs[OP_REM_LHS(op)].f32, rs[OP_REM_RHS(op)].f32 ); break;
      }
      pc++;
      break;
    case OP_FPRIM:
      switch ( OP_FPRIM_MODE(op) ) {
      case  FPRIM_MODE_SQRT: rs[OP_FPRIM_DST(op)].f32 = sqrtf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case   FPRIM_MODE_SIN: rs[OP_FPRIM_DST(op)].f32 = sinf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case   FPRIM_MODE_COS: rs[OP_FPRIM_DST(op)].f32 = cosf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case   FPRIM_MODE_EXP: rs[OP_FPRIM_DST(op)].f32 = expf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case  FPRIM_MODE_EXP2: rs[OP_FPRIM_DST(op)].f32 = exp2f( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case   FPRIM_MODE_LOG: rs[OP_FPRIM_DST(op)].f32 = logf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case FPRIM_MODE_LOG10: rs[OP_FPRIM_DST(op)].f32 = log10f( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case  FPRIM_MODE_LOG2: rs[OP_FPRIM_DST(op)].f32 = log2f( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case   FPRIM_MODE_ABS: rs[OP_FPRIM_DST(op)].f32 = fabs( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case FPRIM_MODE_FLOOR: rs[OP_FPRIM_DST(op)].f32 = floorf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case  FPRIM_MODE_CEIL: rs[OP_FPRIM_DST(op)].f32 = ceilf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case FPRIM_MODE_TRUNC: rs[OP_FPRIM_DST(op)].f32 = truncf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case  FPRIM_MODE_RINT: rs[OP_FPRIM_DST(op)].f32 = rintf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case FPRIM_MODE_NEARBYINT: rs[OP_FPRIM_DST(op)].f32 = nearbyintf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      case FPRIM_MODE_ROUND: rs[OP_FPRIM_DST(op)].f32 = roundf( rs[OP_FPRIM_SRC(op)].f32 ); break;
      }
      pc++;
      break;
    case OP_SHIFT:
      switch ( OP_SHIFT_MODE(op) ) {
      case             SHIFT_MODE_LEFT: rs[OP_SHIFT_DST(op)].ui32 = rs[OP_SHIFT_LHS(op)].ui32 << rs[OP_SHIFT_RHS(op)].ui32; break;
      case    SHIFT_MODE_RIGHT_LOGICAL: rs[OP_SHIFT_DST(op)].ui32 = rs[OP_SHIFT_LHS(op)].ui32 >> rs[OP_SHIFT_RHS(op)].ui32; break;
      case SHIFT_MODE_RIGHT_ARITHMETIC: rs[OP_SHIFT_DST(op)].si32 = rs[OP_SHIFT_LHS(op)].si32 >> rs[OP_SHIFT_RHS(op)].si32; break;
      }
      pc++;
      break;
    case OP_BITWISE:
      switch ( OP_BITWISE_MODE(op) ) {
      case  BIT_MODE_AND: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 & rs[OP_BITWISE_RHS(op)].ui32; break;
      case  BIT_MODE_IOR: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 | rs[OP_BITWISE_RHS(op)].ui32; break;
      case  BIT_MODE_XOR: rs[OP_BITWISE_DST(op)].ui32 = rs[OP_BITWISE_LHS(op)].ui32 ^ rs[OP_BITWISE_RHS(op)].ui32; break;
      case BIT_MODE_NAND: rs[OP_BITWISE_DST(op)].ui32 = ~ ( rs[OP_BITWISE_LHS(op)].ui32 & rs[OP_BITWISE_RHS(op)].ui32 ); break;
      }
      pc++;
      break;
    case OP_CONV:
      switch ( OP_CONV_TY(op) ) {
      case    CONV_TY_U8_TO_U16: rs[OP_CONV_DST(op)].ui16 = rs[OP_CONV_SRC(op)].ui8; break;
      case    CONV_TY_U8_TO_U32: rs[OP_CONV_DST(op)].ui32 = rs[OP_CONV_SRC(op)].ui8; break;
      case  CONV_TY_U8_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui8; break;
      case    CONV_TY_S8_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].si8; break;
      case    CONV_TY_S8_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].si8; break;
      case  CONV_TY_S8_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si8; break;
      case    CONV_TY_U16_TO_U8: rs[OP_CONV_DST(op)].ui8  = rs[OP_CONV_SRC(op)].ui16; break;
      case   CONV_TY_U16_TO_U32: rs[OP_CONV_DST(op)].ui32 = rs[OP_CONV_SRC(op)].ui16; break;
      case CONV_TY_U16_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui16; break;
      case    CONV_TY_S16_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].si16; break;
      case   CONV_TY_S16_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].si16; break;
      case CONV_TY_S16_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si16; break;
      case    CONV_TY_U32_TO_U8: rs[OP_CONV_DST(op)].ui8  = rs[OP_CONV_SRC(op)].ui32; break;
      case   CONV_TY_U32_TO_U16: rs[OP_CONV_DST(op)].ui16 = rs[OP_CONV_SRC(op)].ui32; break;
      case CONV_TY_U32_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].ui32; break;
      case    CONV_TY_S32_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].si32; break;
      case   CONV_TY_S32_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].si32; break;
      case CONV_TY_S32_TO_FLOAT: rs[OP_CONV_DST(op)].f32  = rs[OP_CONV_SRC(op)].si32; break;
      case  CONV_TY_FLOAT_TO_S8: rs[OP_CONV_DST(op)].si8  = rs[OP_CONV_SRC(op)].f32; break;
      case CONV_TY_FLOAT_TO_S16: rs[OP_CONV_DST(op)].si16 = rs[OP_CONV_SRC(op)].f32; break;
      case CONV_TY_FLOAT_TO_S32: rs[OP_CONV_DST(op)].si32 = rs[OP_CONV_SRC(op)].f32; break;
      }
      pc++;
      break;
    case OP_CMP:
      switch ( OP_CMP_MODE(op) ) {
      case CMP_CODE_I_FALSE: status = 0; break;
      case    CMP_CODE_I_EQ: status = rs[OP_CMP_LHS(op)].ui32 == rs[OP_CMP_RHS(op)].ui32; break;
      case    CMP_CODE_I_NE: status = rs[OP_CMP_LHS(op)].ui32 != rs[OP_CMP_RHS(op)].ui32; break;
      case   CMP_CODE_I_UGT: status = rs[OP_CMP_LHS(op)].ui32 >  rs[OP_CMP_RHS(op)].ui32; break;
      case   CMP_CODE_I_UGE: status = rs[OP_CMP_LHS(op)].ui32 >= rs[OP_CMP_RHS(op)].ui32; break;
      case   CMP_CODE_I_ULT: status = rs[OP_CMP_LHS(op)].ui32 <  rs[OP_CMP_RHS(op)].ui32; break;
      case   CMP_CODE_I_ULE: status = rs[OP_CMP_LHS(op)].ui32 <= rs[OP_CMP_RHS(op)].ui32; break;
      case   CMP_CODE_I_SGT: status = rs[OP_CMP_LHS(op)].si32 >  rs[OP_CMP_RHS(op)].si32; break;
      case   CMP_CODE_I_SGE: status = rs[OP_CMP_LHS(op)].si32 >= rs[OP_CMP_RHS(op)].si32; break;
      case   CMP_CODE_I_SLT: status = rs[OP_CMP_LHS(op)].si32 <  rs[OP_CMP_RHS(op)].si32; break;
      case   CMP_CODE_I_SLE: status = rs[OP_CMP_LHS(op)].si32 <= rs[OP_CMP_RHS(op)].si32; break;
      case  CMP_CODE_I_TRUE: status = 1; break;
      case CMP_CODE_F_FALSE: status = 0; break;
      case   CMP_CODE_F_OEQ: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 == rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_OGT: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 >  rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_OGE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 >= rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_OLT: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 <  rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_OLE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 <= rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_ONE: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32) && rs[OP_CMP_LHS(op)].f32 != rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_ORD: status = !isnan(rs[OP_CMP_LHS(op)].f32) && !isnan(rs[OP_CMP_RHS(op)].f32); break;
      case   CMP_CODE_F_UEQ: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 == rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_UGT: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 >  rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_UGE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 >= rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_ULT: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 <  rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_ULE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 <= rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_UNE: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32) || rs[OP_CMP_LHS(op)].f32 != rs[OP_CMP_RHS(op)].f32; break;
      case   CMP_CODE_F_UNO: status = isnan(rs[OP_CMP_LHS(op)].f32) || isnan(rs[OP_CMP_RHS(op)].f32); break;
      case  CMP_CODE_F_TRUE: status = 1; break;
      }
      pc++;
      break;
    case OP_CONTROL:
    case OP_CONTROL_IMM:
    case OP_CONTROL_IMMX:
      {
        uint8_t inst_size = ( OP_CODE(op) == OP_CONTROL_IMMX ) ? 2 : 1;
        if ( OP_CONTROL_COND(op) == CONTROL_COND_ALWAYS || status ) {
          if ( OP_CONTROL_MODE(op) == CONTROL_MODE_CALL ) {
            *(pc_stack++) = pc + inst_size;
          }
          if ( OP_CODE(op) == OP_CONTROL ) {
            pc = rs[OP_CONTROL_ADDR(op)].ui32;
          } else if ( OP_CODE(op) == OP_CONTROL_IMM ) {
            pc = pc + (OP_CONTROL_IMM_OFFSET(op) - 256);
          } else if ( OP_CODE(op) == OP_CONTROL_IMMX ) {
            pc = (OP_CONTROL_IMM_OFFSET(op) << 16) + prom[pc+1];
          }
        } else {
          pc = pc + inst_size;
        }
      }
      break;
    case OP_RETURN:
      pc = *(pc_stack--);
      break;
    case OP_HALT:
      return;
      break;
    case OP_LOAD_IMM:
      rs[OP_LOAD_IMM_DST(op)].ui32 = OP_LOAD_IMM_VAL(op);
      pc++;
      break;
    case OP_LOAD_IMMX:
      rs[OP_LOAD_IMMX_DST(op)].ui32 = (OP_LOAD_IMMX_HI(op) << 16) + prom[pc+1];
      pc += 2;
      break;
    case OP_SELECT:
      rs[OP_SELECT_DST(op)].ui32 = status ? rs[OP_SELECT_X(op)].ui32 : rs[OP_SELECT_Y(op)].ui32;
      pc++;
      break;
    case OP_BUF_MEM:
      {
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
        pc++;
      }
      break;
    case OP_MEM_FENCE:
      // XXX intel
      _mm_mfence();
      pc++;
      break;
    case OP_BUF_SET:
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
      pc++;
      break;
    case OP_BUF_READ:
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
      pc++;
      break;
    case OP_BUF_ALL:
      {
        void *dst = &rs;
        void *src = &bs[OP_BUF_ALL_DST(op)];
        if ( OP_BUF_ALL_MODE(op) == BUF_ALL_MODE_WRITE ) {
          void *tmp = dst;
          dst = src;
          src = tmp;
        }
        memcpy(dst, src, sizeof(buf));
        pc++;
      }
      break;
    case OP_STACK_MOV:
      switch ( OP_STACK_MOV_DIR(op) ) {
      case STACK_DIR_READ:
        rs[OP_STACK_MOV_DST(op)].ui32 = isp;
        break;
      case STACK_DIR_WRITE:
        isp = rs[OP_STACK_MOV_DST(op)].ui32;
        break;
      }
      pc++;
      break;
    default:
      pc++;
      break;
    }
  }
}

#include <stdio.h>

int main ( int argc, char **argv ) {
  printf("reg    = %lu\n", sizeof(reg));
  printf("regset = %lu\n", sizeof(regset));
  printf("buf    = %lu\n", sizeof(buf));
  printf("bufset = %lu\n", sizeof(bufset));
  printf("OP_T   = %lu\n", sizeof(OP_T));
  return 0;
}
