//--------------------------------------------------
// SUBROUTINES - GPIO
//--------------------------------------------------

.pc = *		"GPIO Module"

//--------------------------------------------------

func_initIO:
	lda #0
	sta BORDER_REG
	sta BG1_REG
	lda #1
	sta IO_STRCOLOR
	:sto16(print_addr, clrscn)
	jsr func_prints
	:sto16(print_addr, set2)
	jsr func_prints
	:sto16(print_addr, text_title)
	jsr func_prints

	// Start SID random number generator
	lda #$FF
	sta $D40E
	sta $D40F
	lda #$80
	sta $D412
	rts

//--------------------------------------------------

func_prints:
	pha
	tya
	pha
	lda print_addr
	ldy print_addr + 1
	jsr IO_PRINTSTR
	pla
	tay
	pla
	rts

//--------------------------------------------------

func_printu:
	pha
	txa
	pha
	tya
	pha
	ldy #0
!loop:	tya pha
	:sto16(tmp, 10)
	:unsigned_div16(print_val, tmp, tmp2, tmp3)
	:mov16(tmp2, print_val)
	:add8(tmp3, zero, tmp3)
	pla tay iny
	lda tmp3 pha
	:cmpz16(print_val)
	beq !over+
	jmp !loop-
!over:	lda #0 sta tmp3 + 1
	:sto16(print_addr, tmp3)
!loop:	pla sta tmp3
	tya pha
	jsr func_prints
	pla tay dey
	bne !loop-
	pla
	tay
	pla
	tax
	pla
	rts

//--------------------------------------------------

func_printd:
	pha
	bit print_val + 1
	bpl !else+
	:sto16(print_addr, minus)
	jsr func_prints
	:neg16(print_val)
	jmp !skip+
!else:	:sto16(print_addr, space)
	jsr func_prints
!skip:	jsr func_printu
	pla
	rts

//--------------------------------------------------

func_printlnd:
	jsr func_printd
	:sto16(print_addr, cr)
	jsr func_prints
	rts

//--------------------------------------------------

func_printf:
	bit print_val + 1
	bpl !skip+
	:neg8(print_val)
!skip:	lda #0
	sta print_val + 1
	:sto16(print_addr, tmp)
!loop:	:sto16(tmp, 10)
	:mul16(print_val, tmp, tmp2)
	:mov16(tmp2, print_val)
	:add8(print_val + 1, zero, tmp)
	jsr func_prints
	lda #0
	sta print_val + 1
	lda print_val
	bne !loop-
	rts

//--------------------------------------------------

func_printfp:
	:mov32(print_val, FP1)
	jsr fix
	lda m1
	sta print_val + 1
	sta tmp2
	lda m1 + 1
	sta print_val
	lda m1 + 2
	sta tmp
	jsr func_printd
	:sto16(print_addr, dot)
	jsr func_prints
	lda tmp
	sta print_val
	lda tmp2
	sta print_val + 1
	jsr func_printf
	rts

//--------------------------------------------------

func_printlnfp:
	jsr func_printfp
	:sto16(print_addr, cr)
	jsr func_prints
	rts

//--------------------------------------------------

func_paint:
	pha tya pha lda r0 pha lda r1 pha
	lda paint_y
	sta r0
	lda #0
	sta r0 + 1
	:shl16(r0)
	:shl16(r0)
	:shl16(r0)
	:mov16(r0, r1)
	:shl16(r0)
	:shl16(r0)
	:add16(r1, r0, r1)
	ldy paint_x
	:sto16(r0, COL_SEGMENT)
	:add16(r0, r1, r0)
	lda paint_col
	sta (r0),y
	:sto16(r0, CHR_SEGMENT)
	:add16(r0, r1, r0)
	lda #$A0
	sta (r0),y
	pla sta r1 pla sta r0 pla tay pla
	rts

//--------------------------------------------------
// DATA
//--------------------------------------------------
set1:		.byte $8e, 0
set2:		.byte $0e, 0
clrscn:		.byte $93, 0
gohome:		.byte $13, 0
cr:		.byte 13, 0
minus:		.text "-" .byte 0
space:		.text " " .byte 0
dot:		.text "." .byte 0
zero:		.text "0" .byte 0

//--------------------------------------------------
// SUBROUTINES' VARIABLES
//--------------------------------------------------
print_addr:	.word 0
print_val:	.dword 0
tmp:		.word 0
tmp2:		.word 0
tmp3:		.word 0
paint_x:	.byte 0
paint_y:	.byte 0
paint_col:	.byte 0
