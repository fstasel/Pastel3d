//--------------------------------------------------
// MACROS
//--------------------------------------------------

.macro sto16(addr, val)
{
	lda #<val
	sta addr
	lda #>val
	sta addr + 1
}

.macro inc16(arg)
{
	inc arg
	bne over
	inc arg + 1
over:	
}

.macro add8(arg1, arg2, tar)
{
	clc
	lda arg1
	adc arg2
	sta tar
}

.macro adc8(arg1, arg2, tar)
{
	lda arg1
	adc arg2
	sta tar
}

.macro add8_16(arg1, arg2, tar)
{
	clc
	lda arg1
	adc arg2
	sta tar
	lda #0
	adc arg2 + 1
	sta tar + 1
}

.macro add16(arg1, arg2, tar)
{
	:add8(arg1, arg2, tar)
	:adc8(arg1 + 1, arg2 + 1, tar + 1)
}

.macro add8_immediate(arg1, arg2, tar)
{
	clc
	lda arg1
	adc #arg2
	sta tar
}

.macro adc8_immediate(arg1, arg2, tar)
{
	lda arg1
	adc #arg2
	sta tar
}

.macro add16_immediate(arg1, arg2, tar)
{
	:add8_immediate(arg1, <arg2, tar)
	:adc8_immediate(arg1 + 1, >arg2, tar + 1)
}

.macro dec16(arg)
{
	lda arg
	bne over
	dec arg + 1
over:	dec arg
}

.macro sub8(arg1, arg2, tar)
{
	sec
	lda arg1
	sbc arg2
	sta tar
}

.macro sbc8(arg1, arg2, tar)
{
	lda arg1
	sbc arg2
	sta tar
}

.macro sub16(arg1, arg2, tar)
{
	:sub8(arg1, arg2, tar)
	:sbc8(arg1 + 1, arg2 + 1, tar + 1)
}

.macro sbc16(arg1, arg2, tar)
{
	:sbc8(arg1, arg2, tar)
	:sbc8(arg1 + 1, arg2 + 1, tar + 1)
}

.macro sub8_immediate(arg1, arg2, tar)
{
	sec
	lda arg1
	sbc #arg2
	sta tar
}

.macro sbc8_immediate(arg1, arg2, tar)
{
	lda arg1
	sbc #arg2
	sta tar
}

.macro sub16_immediate(arg1, arg2, tar)
{
	:sub8_immediate(arg1, <arg2, tar)
	:sbc8_immediate(arg1 + 1, >arg2, tar + 1)
}

.macro sbc16_immediate(arg1, arg2, tar)
{
	:sbc8_immediate(arg1, <arg2, tar)
	:sbc8_immediate(arg1 + 1, >arg2, tar + 1)
}

.macro neg8(arg)
{
	sec
	lda #0
	sbc arg
	sta arg
}

.macro neg16(arg)
{
	sec
	lda #0
	sbc arg
	sta arg
	lda #0
	sbc arg + 1
	sta arg + 1
}

.macro shr16(arg)
{
	lsr arg + 1
	ror arg
}

.macro shl16(arg)
{
	asl arg
	rol arg + 1
}

.macro ror16(arg)
{
	ror arg + 1
	ror arg
}

.macro rol16(arg)
{
	rol arg
	rol arg + 1
}

.macro signed_divbytwo8(arg)
{
	lda arg
	asl
	ror arg
}

.macro signed_divbytwo16(arg)
{
	lda arg + 1
	asl
	ror arg + 1
	ror arg
}

.macro clr16(arg)
{
	lda #0
	sta arg
	sta arg + 1
}

.macro mov16(src, des)
{
	lda src
	sta des
	lda src + 1
	sta des + 1
}

.macro mov16Y_(src, des)
{
	lda src,y
	sta des
	lda src + 1,y
	sta des + 1
}

.macro clr32(arg)
{
	lda #0
	sta arg
	sta arg + 1
	sta arg + 2
	sta arg + 3
}

.macro mov32(src, des)
{
	lda src
	sta des
	lda src + 1
	sta des + 1
	lda src + 2
	sta des + 2
	lda src + 3
	sta des + 3
}

.macro mov32_Y(src, des)
{
	lda src
	sta des,y
	lda src + 1
	sta des + 1,y
	lda src + 2
	sta des + 2,y
	lda src + 3
	sta des + 3,y
}

.macro mov32Y_(src, des)
{
	lda src,y
	sta des
	lda src + 1,y
	sta des + 1
	lda src + 2,y
	sta des + 2
	lda src + 3,y
	sta des + 3
}

.macro mov32_indirect(src, des)
{
	ldy #0
	lda src
	sta (des),y
	iny
	lda src + 1
	sta (des),y
	iny
	lda src + 2
	sta (des),y
	iny
	lda src + 3
	sta (des),y
}

.macro mov32indirect_(src, des)
{
	ldy #0
	lda (src),y
	sta des
	iny
	lda (src),y
	sta des + 1
	iny
	lda (src),y
	sta des + 2
	iny
	lda (src),y
	sta des + 3
}

.macro mov32indirectY_(src, des)
{
	lda (src),y
	sta des
	iny
	lda (src),y
	sta des + 1
	iny
	lda (src),y
	sta des + 2
	iny
	lda (src),y
	sta des + 3
}

.macro mov32_indirectY(src, des)
{
	lda src
	sta (des),y
	iny
	lda src + 1
	sta (des),y
	iny
	lda src + 2
	sta (des),y
	iny
	lda src + 3
	sta (des),y
}

.macro mov_indirect16(src, des, reg)
{
	:mov16(src, reg)
	ldy #0
	lda (reg),y
	sta des
	iny
	lda (reg),y
	sta des + 1
}

.macro abs16(arg)
{
	bit arg
	bpl over
	:neg16(arg)
over:
}

.macro push16(arg)
{
	lda arg
	pha
	lda arg + 1
	pha
}

.macro pull16(arg)
{
	pla
	sta arg + 1
	pla
	sta arg
}

.macro mul16(arg1, arg2, tar)
{
	:push16(arg1)
	:clr16(tar)
	ldx #16
loop:	:shl16(tar)
	:shl16(arg1)
	bcc next
	:add16(arg2, tar, tar)
next:	dex
	bne loop
	:pull16(arg1)
}

.macro mul8x(arg1, arg2, tar)
{
	lda arg1
	pha
	:clr16(tar)
	ldx #8
loop:	:shl16(tar)
	asl arg1
	bcc next
	:add8_16(arg2, tar, tar)
next:	dex
	bne loop
	pla
	sta arg1
}

.macro mul8(arg1, arg2, tar)
{
	lda arg1
	pha
	lda #0
	ldx #8
loop:	asl
	asl arg1
	bcc next
	clc
	adc arg2
next:	dex
	bne loop
	sta tar
	pla
	sta arg1
}

.macro unsigned_div16(arg1, arg2, quo, rem)
{
	:push16(arg2)
	:mov16(arg1, quo)
	:clr16(rem)
	ldx #16
	:shl16(quo)
loop:	:rol16(rem)
	:cmp16(rem, arg2)
	bcc next
	:sbc16(rem, arg2, rem)
next:	:rol16(quo)
	dex
	bne loop
	:pull16(arg2)
}

.macro unsigned_div8(arg1, arg2, quo, rem)
{
	lda arg2
	pha
	lda arg1
	sta quo
	lda #0
	ldx #8
	asl quo
loop:	rol
	cmp arg2
	bcc next
	sbc arg2
next:	rol quo
	dex
	bne loop
	sta rem
	pla
	sta arg2
}

.macro div16(arg1, arg2, quo, rem)
{
	ldy #0
	:mov16(arg1, quo)
	bpl pass1
	:neg16(quo)
	iny
pass1:	:push16(arg2)
	bpl pass2
	:neg16(arg2)
	dey
pass2:	:clr16(rem)
	ldx #16
	:shl16(quo)
loop:	:rol16(rem)
	:cmp16(rem, arg2)
	bcc next
	:sbc16(rem, arg2, rem)
next:	:rol16(quo)
	dex
	bne loop
	tya
	beq pass3
	:neg16(quo)
pass3:	:pull16(arg2)
}

.macro div8(arg1, arg2, quo, rem)
{
	ldy #0
	lda arg1
	sta quo
	bpl pass1
	:neg8(quo)
	iny
pass1:	lda arg2
	pha
	bpl pass2
	:neg8(arg2)
	dey
pass2:	lda #0
	ldx #8
	asl quo
loop:	rol
	cmp arg2
	bcc next
	sbc arg2
next:	rol quo
	dex
	bne loop
	sta rem
	tya
	beq pass3
	:neg8(quo)
pass3:	pla
	sta arg2
}

.macro cmp16(arg1, arg2)	// use bcc for <= and use bcs for >=
{
	lda arg1 + 1
	cmp arg2 + 1
	bne done
	lda arg1
	cmp arg2
done:
}

.macro cmpz16(arg)
{
	lda arg + 1
	bne done
	lda arg
done:
}

.macro isqrt16(n, root, remainder, place, buffer)
{
	:clr16(root)
	:mov16(n, remainder)
	:sto16(place, 16384)
loop1:	:cmp16(place, remainder)
	beq loop2
	bcs iloop1
	jmp loop2
iloop1:	:shr16(place)
	:shr16(place)
	jmp loop1
loop2:	:cmpz16(place)
	beq over
	:add16(root, place, buffer)
	:cmp16(remainder, buffer)
	beq doit
	bcc skip
doit:	:sub16(remainder, buffer, remainder)
	:mov16(place, buffer)
	:shl16(buffer)
	:add16(root, buffer, root)
skip:	:shr16(root)
	:shr16(place)
	:shr16(place)
	jmp loop2	
over:
}

.macro convert8to16(arg, tar)
{
	lda arg
	sta tar
	bpl pos
	lda #$FF
	jmp end
pos:	lda #0
end:	sta tar + 1
}

.macro convert16toFP1(arg)
{
	lda arg
	sta m1+ 1
	lda arg + 1
	sta m1
	lda #0
	sta m1 + 2
	jsr float		
}


.macro convert16toFP1_immediate(arg)
{
	lda #<arg
	sta m1 + 1
	lda #>arg
	sta m1
	lda #0
	sta m1 + 2
	jsr float
}

.macro convert16toFP(arg, tar)
{
	:convert16toFP1(arg)
	:mov32(FP1, tar)
}

.macro convert16toFP_immediate(arg, tar)
{
	:convert16toFP1_immediate(arg)
	:mov32(FP1, tar)
}

.macro fsqrt(fn, froot, t0_16, t1_16, t2_16)
{
	:mov32(fn, FP1)
	lda x1
	and #1
	beq skip
	jsr rtar
skip:	lda x1
	eor #$80
	sta x1
	asl
	ror x1
	lda x1
	eor #$80
	adc #7
	sta x1
	lda m1 ldy m1 + 1
	sta m2 + 1 sty m2
	:isqrt16(m2, m1, t0_16, t1_16, t2_16)
	lda m1 ldy m1 + 1
	sta m1 + 1 sty m1
	lda #0 sta m1 + 2
	jsr norm
	:mov32(FP1, froot)
}

.macro sincosY(fsin, fcos)
{
	lda sintableL,y
	sta fsin
	lda sintableH,y
	sta fsin + 1
	tya
	clc
	adc #64
	tay
	lda sintableL,y
	sta fcos
	lda sintableH,y
	sta fcos + 1
	:convert16toFP(fsin, fsin)
	:convert16toFP(fcos, fcos)
	:sub8_immediate(fsin, 15, fsin)
	:sub8_immediate(fcos, 15, fcos)
}

.macro fzero(tar)
{
	lda #0
	sta tar
	sta tar + 1
	sta tar + 2
	sta tar + 3
}

.macro fone(tar)
{
	lda #$80
	sta tar
	lda #$40
	sta tar + 1
	lda #0
	sta tar + 2
	sta tar + 3
}

.macro fset(tar, argh, argl)
{
	lda #<argl
	sta tar + 3
	lda #>argl
	sta tar + 2
	lda #<argh
	sta tar + 1
	lda #>argh
	sta tar
}

.macro fcmpz(arg)		// test exact zero
{				// do not use for < or >
	lda #0
	cmp arg
	bne over
	cmp arg + 1
	bne over
	cmp arg + 2
	bne over
	cmp arg + 3
over:
}

.macro fcmpe(arg1, arg2)	// test exact equal
{				// do not use for < or >
	lda arg1
	cmp arg2
	bne over
	lda arg1 + 1
	cmp arg2 + 1
	bne over
	lda arg1 + 2
	cmp arg2 + 2
	bne over
	lda arg2 + 3
	cmp arg2 + 3
over:
}

.macro fcmpFP1(arg)		// use normalized fp!
{
	bit arg + 1
	bmi neg1
	bit m1
	bmi over
	jmp comp
neg1:	bit m1
	bpl over
comp:	lda x1
	cmp arg
	bne over
	lda m1
	cmp arg + 1
	bne over
	lda m1 + 1
	cmp arg + 2
	bne over
	lda m1 + 2
	cmp arg + 3
over:
}

.macro fcmpo(arg)		// use normalized fp!
{				// compare with 1.0
	bit arg + 1
	bmi over
	lda arg
	cmp #$80
	bne over
	lda arg + 1
	cmp #$40
	bne over
	lda arg + 2
	bne over
	lda arg + 3
over:
}

.macro fincFP1()
{
	:fone(FP2)
	jsr fadd
}

.macro fdecFP2()
{
	:fone(FP1)
	jsr fsub
}

.macro faddFP1(arg)
{
	:mov32(arg, FP2)
	jsr fadd
}

.macro fsubFP2(arg)
{
	:mov32(arg, FP1)
	jsr fsub
}

.macro fsubFP1(arg)
{
	:mov32(arg, FP2)
	jsr fsub
}

.macro fmulFP1(arg)
{
	:mov32(arg, FP2)
	jsr fmul
}

.macro fdivFP1(arg)
{
	:mov32(arg, FP2)
	jsr fdiv
}

.macro fdivFP2(arg)
{
	:mov32(arg, FP1)
	jsr fdiv
}

.macro getColor(r1, r2, r3)
{
	asl r1 rol
	asl r1 rol
	asl r1 rol
	asl r2 rol
	asl r2 rol
	asl r3 rol
	asl r3 rol
	asl r3 rol
	tay
	lda coltable,y
}

.macro pusha()
{
	pha
	tya
	pha
	txa
	pha
	:push16(r0)
	:push16(r1)
	:push16(r2)
	:push16(r3)
	:push16(r4)
	:push16(r5)
	:push16(r6)
	:push16(r7)
	:push16(r8)
	:push16(r9)
	:push16(r10)
	:push16(r11)
	:push16(r12)
	:push16(r13)
	:push16(r14)
	:push16(r15)
}

.macro pulla()
{
	:pull16(r15)
	:pull16(r14)
	:pull16(r13)
	:pull16(r12)
	:pull16(r11)
	:pull16(r10)
	:pull16(r9)
	:pull16(r8)
	:pull16(r7)
	:pull16(r6)
	:pull16(r5)
	:pull16(r4)
	:pull16(r3)
	:pull16(r2)
	:pull16(r1)
	:pull16(r0)
	pla
	tax
	pla
	tay
	pla
}

//--------------------------------------------------
// FUNCTIONS - IO
//--------------------------------------------------

.function toPet(text, i)
{
	.var k = text.charAt(i) + 0
	.if(k >= 65 && k <= 90) .eval k = k + 32
	else .if(k >= 1 && k <= 26) .eval k = k + 64
	.return k
}

//--------------------------------------------------

.macro debugf(arg)
{
	:pusha()
	:mov32(arg, print_val)
	jsr func_printfp
	:pulla()
}

.macro debug100f(arg)
{
	:pusha()
	:mov32(arg, FP2)
	:convert16toFP1_immediate(100)
	jsr fmul
	:mov32(FP1, print_val)
	jsr func_printfp
	:pulla()
}

.macro debug8(arg)
{
	:pusha()
	lda arg sta print_val
	lda #0 sta print_val+1
	jsr func_printd
	:pulla()
}

.macro debug16(arg)
{
	:pusha()
	:mov16(arg, print_val)
	jsr func_printd
	:pulla()
}

.macro debugA()
{
	sta print_val
	lda #0 sta print_val+1
	jsr func_printd
}
