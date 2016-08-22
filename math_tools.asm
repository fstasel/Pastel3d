//--------------------------------------------------
// SUBROUTINES - MATH TOOLS
//--------------------------------------------------

.pc = *		"Math tools"

// Variables
param1:		.dword 0
param2:		.dword 0

//--------------------------------------------------

func_convert16toFP:	// param1 (int16) -> param1 (float32)
	:convert16toFP(param1, param1)
	rts

//--------------------------------------------------

func_sincosY:		// sin(Y)->param1; cos(Y)->param2
	:sincosY(param1, param2)
	rts

//--------------------------------------------------

func_dotvec:		// (r0).(r1) -> rr4 (also FP1);	r0:incremented for next object
	:push16(r1)
	:fzero(rr4)
	ldx #3
!loop:	txa pha
	:mov32indirect_(r0, FP1)
	:mov32indirect_(r1, FP2)
	jsr fmul
	:mov32(rr4, FP2)
	jsr fadd
	:mov32(FP1, rr4)
	:add16_immediate(r0, 4, r0)
	:add16_immediate(r1, 4, r1)
	pla tax
	dex
	bne !loop-
	:pull16(r1)
	rts

//--------------------------------------------------

func_mulmatvec:		// (r0)*(r1) -> (r2);	r0,r2:incremented for next object; rr4 used
	ldx #3
!loop:	txa pha
	jsr func_dotvec
	:mov32_indirect(rr4, r2)
	:add16_immediate(r2, 4, r2)
	pla tax
	dex
	bne !loop-
	rts

//--------------------------------------------------

func_addvec:		// (r0)+(r1) -> (r2);	r0,r2:incremented for next object	
	:push16(r1)
	ldx #3
!loop:	txa pha
	:mov32indirect_(r0, FP1)
	:mov32indirect_(r1, FP2)
	jsr fadd
	:mov32_indirect(FP1, r2)
	:add16_immediate(r0, 4, r0)
	:add16_immediate(r1, 4, r1)
	:add16_immediate(r2, 4, r2)
	pla tax dex
	bne !loop-
	:pull16(r1)
	rts
