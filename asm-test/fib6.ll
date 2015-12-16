    define i64 @fac(i64 %arg) {
entry:
    %arg_is_zero = icmp eq i64 0, %arg
    br i1 %arg_is_zero, label %done, label %loop
loop:
    %arg_step = phi i64 [ %arg, %entry ], [ %arg_step_post, %loop ]
    %acc_step = phi i64 [ 1, %entry ], [ %acc_step_post, %loop ]
    %acc_step_post = mul i64 %acc_step, %arg_step
    %arg_step_post = sub i64 %arg_step, 1
    %arg_now_zero = icmp eq i64 0, %arg_step_post
    br i1 %arg_now_zero, label %done, label %loop
done:
    %acc_done = phi i64 [ 1, %entry ], [ %acc_step_post, %loop ]
    ret i64 %acc_done
    }
