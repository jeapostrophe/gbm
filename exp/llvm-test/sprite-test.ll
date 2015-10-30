; External declaration of the puts function
declare i32 @exit() nounwind

; float =            1,     2,     3,     4,     5,     6,        7,              8
;   i16 =            2,     4,     6,     8,    10,    12,  13,  14,     15,     16
;    i8 =            4,     8,    12,    16,    20,    24,  26,  28, 29, 30, 31, 32
%athing = type { float, float, float, float, float, float, i16, i16, i8, i8, i8, i8 }

@one = external global %athing
@two = external global %athing

define i32 @main() {
  ; Make a one one
  store %athing { float 5.0, float 6.0,
         float 1.0, float 2.0,
         float 3.0, float 4.0,
         i16 25, i16 15,
         i8 1,
         i8 2, i8 3, i8 4 }, %athing* @one
  call i32 @exit()
  ; RESULT: Very slow with 13 memory stores

  ; Load a two one from somewhere else
  %a = load %athing* @two
  store %athing %a, %athing* @one
  call i32 @exit()
  ; RESULT: 6 vector loads, then a lot of parsing, then 12 memory stores, some as vectors

  ; See what the size of a sprite is
  %gp = alloca %athing
  %g = load %athing* %gp
  store %athing %g, %athing* @one
  call i32 @exit()
  ; RESULT %rsp has 40 added to it, but we start at %rsp+8 for alignment

  ; Try to make it use a wide vector op (exchange)
  %two_vector = bitcast %athing* @two to <8 x float>*
  %b = load <8 x float>* %two_vector
  %one_vector = bitcast %athing* @one to <8 x float>*
  store <8 x float> %b, <8 x float>* %one_vector
  call i32 @exit()
  ; RESULT: 1 vector load and 1 vector store

  ; Try to make it use a wide vector op, changing one of the floats
  %cv_orig = load <8 x float>* %two_vector
  %c_dx_orig = extractelement <8 x float> %cv_orig, i32 0
  %c_dx_change = fadd float %c_dx_orig, 1.0
  %cv_change = insertelement <8 x float> %cv_orig, float %c_dx_change, i32 0
  store <8 x float> %cv_change, <8 x float>* %one_vector
  call i32 @exit()
  ; RESULT: 1 vector load, 1 vector load of the constant, an addition, then a single vector store

  ; Try to use a vector op to change the shorts and bytes  
  %dv_orig = load <8 x float>* %two_vector
  ;; change the float
  %d_dx_orig = extractelement <8 x float> %dv_orig, i32 0
  %d_dx_change = fadd float %d_dx_orig, 1.0
  %dv_change0 = insertelement <8 x float> %dv_orig, float %d_dx_change, i32 0
  ;; change the short
  %dv_change0_s = bitcast <8 x float> %dv_change0 to <16 x i16>
  %dv_change1_s = insertelement <16 x i16> %dv_change0_s, i16 5, i32 14
  %dv_change1 = bitcast <16 x i16> %dv_change1_s to <8 x float>
  ;; change a byte
  %dv_change1_b = bitcast <8 x float> %dv_change1 to <32 x i8>
  %dv_change2_b = insertelement <32 x i8> %dv_change1_b, i8 66, i32 29
  %dv_change2 = bitcast <32 x i8> %dv_change2_b to <8 x float>
  store <8 x float> %dv_change2, <8 x float>* %one_vector
  call i32 @exit()
  ; RESULT: virtually equivalent to the above, but with some more vector manipulations

  ; Make a one one using vector ops
  %es_f = bitcast <2 x i16> <i16 25, i16 15> to float
  %ev_fs = insertelement <8 x float> <float 5.0, float 6.0, float 1.0, float 2.0, float 3.0, float 4.0, float 0.0, float 0.0 >, float %es_f, i32 6
  %eb_f = bitcast <4 x i8> <i8 1, i8 2, i8 3, i8 4> to float
  %ev_fsb = insertelement <8 x float> %ev_fs, float %eb_f, i32 7
  store <8 x float> %ev_fsb, <8 x float>* %one_vector
  call i32 @exit()
  ; RESULT: lots of complicated constants loaded from memory before a single write

  ; Make a one one using bitcasting vector ops
  %fv_fs_pre = bitcast <8 x float> <float 5.0, float 6.0, float 1.0, float 2.0, float 3.0, float 4.0, float 0.0, float 0.0 > to <16 x i16>
  %fv_fs_post0 = insertelement <16 x i16> %fv_fs_pre, i16 25, i8 13
  %fv_fs_post1 = insertelement <16 x i16> %fv_fs_post0, i16 15, i8 14
  %fv_fsb_pre = bitcast <16 x i16> %fv_fs_post1 to <32 x i8>
  %fv_fsb_post0 = insertelement <32 x i8> %fv_fsb_pre, i8 1, i8 28
  %fv_fsb_post1 = insertelement <32 x i8> %fv_fsb_post0, i8 2, i8 29
  %fv_fsb_post2 = insertelement <32 x i8> %fv_fsb_post1, i8 3, i8 30
  %fv_fsb_post3 = insertelement <32 x i8> %fv_fsb_post2, i8 4, i8 31
  %fv_fsb = bitcast <32 x i8> %fv_fsb_post3 to <8 x float>
  store <8 x float> %fv_fsb, <8 x float>* %one_vector
  call i32 @exit()
  ; RESULTS: one constant for the first six floats, then a trivial sequence of inserts, then a single write. This could be better, however, by inserting the bytes into a word and then the three words into the floats

  ret i32 0
}
