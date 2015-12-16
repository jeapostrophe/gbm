    define void @vm2_run(i16 *%prom, i32 %ipc) {
entry:
    br label %loop
loop:
    %loop_r1 = phi i32 [ 0, %entry ], [ %sub_r1p, %sub ]
    br label %sub
;;load_imm:
;;    br label %loop 
;;cmp:
;;    br label %loop
;;control_imm:
;;    br label %loop
;;mul:
;;    br label %loop
sub:
    %sub_r1 = phi i32 [ %loop_r1, %loop ]
    %sub_r1p = sub i32 %sub_r1, 0
    br label %loop
halt:
    ret void
    }
