#!/bin/sh
clang -emit-llvm -c $1 -o /tmp/llvm.$$.bc &&
    llvm-dis /tmp/llvm.$$.bc &&
    less /tmp/llvm.$$.ll
