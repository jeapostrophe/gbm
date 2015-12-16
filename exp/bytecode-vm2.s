	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 15, 2
	.globl	_vm2_run
	.align	4, 0x90
_vm2_run:                               ## @vm2_run
	.cfi_startproc
## BB#0:                                ## %entry
	.align	4, 0x90
LBB0_1:                                 ## %sub
                                        ## =>This Inner Loop Header: Depth=1
	jmp	LBB0_1
	.cfi_endproc


.subsections_via_symbols
