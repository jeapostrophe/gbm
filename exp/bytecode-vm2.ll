    define void @vm2_run(i16 *%prom, i32 %ipc) {
entry:
    br label %loop
loop:
    br label %halt
load_imm:
    br label %loop
cmp:
    br label %loop
control_imm:
    br label %loop
mul:
    br label %loop
sub:
    %sub_r1 = phi i32 [ %loop_r1, %loop ]
    %sub_r3 = phi i32 [ %loop_r3, %loop ]
    %sub_r1p = sub i32 %sub_r1, %sub_r3
    br label %loop
halt:
    ret void
    }
