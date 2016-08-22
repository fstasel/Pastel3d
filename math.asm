// 32-bit floating point routines

.const FP1 = $07	// Floating-point register 1
.const FP2 = $03	// Floating-point register 2

.const sign = $02
.const x2 = $03
.const m2 = $04
.const x1 = $07
.const m1 = $08
.const e = $0b

.pc = *		"Floating point module"

add:	clc		// clear carry
	ldx  #$2	// index for 3-byte add.
add1:	lda  m1,x
	adc  m2,x	// add a byte of mant2 to mant1
	sta  m1,x
	dex           	// index to next more signif. byte.
	bpl  add1     	// loop until done.
	rts           	// return
md1:	asl  sign     	// clear lsb of sign.
	jsr  abswap	// abs val of m1, then swap with m2
abswap:	bit  m1		// mant1 negative?
	bpl  abswap1	// no, swap with mant2 and return.
	jsr  fcompl	// yes, complement it.
	inc  sign	// incr sign, complementing lsb.
abswap1:sec           	// set carry for return to mul/div.
swap:	ldx  #$4      	// index for 4 byte swap.
swap1:	sty  e-1,x
	lda  x1-1,x	// swap a byte of exp/mant1 with
	ldy  x2-1,x	// exp/mant2 and leave a copy of
	sty  x1-1,x   	// mant1 in e (3 bytes).  e+3 used
	sta  x2-1,x
	dex           	// advance index to next byte
	bne  swap1    	// loop until done.
	rts           	// return
float:	lda  #$8e     	// init exp1 to 14,
	sta  x1       	// then normalize to float.
norm1:	lda  m1       	// high-order mant1 byte.
	cmp  #$c0     	// upper two bits unequal?
	bmi  rts1     	// yes, return with mant1 normalized
	dec  x1       	// decrement exp1.
	asl  m1+2
	rol  m1+1     	// shift mant1 (3 bytes) left.
	rol  m1
norm:	lda  x1       	// exp1 zero?
	bne  norm1    	// no, continue normalizing.
rts1:	rts           	// return.
fsub:	jsr  fcompl   	// cmpl mant1,clears carry unless 0
swpalgn:jsr  algnswp  	// right shift mant1 or swap with
fadd:	lda  x2
	cmp  x1       	// compare exp1 with exp2.
	bne  swpalgn  	// if #,swap addends or align mants.
	jsr  add      	// add aligned mantissas.
addend:	bvc  norm     	// no overflow, normalize result.
	bvs  rtlog    	// ov: shift m1 right, carry into sign
algnswp:bcc  swap     	// swap if carry clear,
rtar:	lda  m1       	// sign of mant1 into carry for
	asl           	// right arith shift.
rtlog:	inc  x1       	// incr x1 to adjust for right shift
	beq  ovfl     	// exp1 out of range.
rtlog1:	ldx  #$fa     	// index for 6:byte right shift.
ror1:	ror  e+3,x
	inx           	// next byte of shift.
	bne  ror1     	// loop until done.
	rts           	// return.
fmul:	jsr  md1      	// abs val of mant1, mant2
	adc  x1       	// add exp1 to exp2 for product exp
	jsr  md2      	// check prod. exp and prep. for mul
	clc           	// clear carry for first bit.
mul1:	jsr  rtlog1   	// m1 and e right (prod and mplier)
	bcc  mul2     	// if carry clear, skip partial prod
	jsr  add      	// add multiplicand to product.
mul2:	dey           	// next mul iteration.
	bpl  mul1     	// loop until done.
mdend:	lsr  sign     	// test sign lsb.
normx:	bcc  norm     	// if even,normalize prod,else comp
fcompl:	sec           	// set carry for subtract.
	ldx  #$3      	// index for 3 byte subtract.
compl1:	lda  #$0      	// clear a.
	sbc  x1,x     	// subtract byte of exp1.
	sta  x1,x     	// restore it.
	dex           	// next more significant byte.
	bne  compl1   	// loop until done.
	beq  addend   	// normalize (or shift rt if ovfl).
fdiv:	jsr  md1      	// take abs val of mant1, mant2.
	sbc  x1       	// subtract exp1 from exp2.
	jsr  md2      	// save as quotient exp.
div1:	sec           	// set carry for subtract.
	ldx  #$2      	// index for 3-byte subtraction.
div2:	lda  m2,x
	sbc  e,x      	// subtract a byte of e from mant2.
	pha           	// save on stack.
	dex           	// next more significant byte.
	bpl  div2     	// loop until done.
	ldx  #$fd     	// index for 3-byte conditional move
div3:	pla           	// pull byte of difference off stack
	bcc  div4     	// if m2<e then don't restore m2.
	sta  m2+3,x
div4:	inx           	// next less significant byte.
	bne  div3     	// loop until done.
	rol  m1+2
	rol  m1+1     	// roll quotient left, carry into lsb
	rol  m1
	asl  m2+2
	rol  m2+1     	// shift dividend left
	rol  m2
	bcs  ovfl     	// ovfl is due to unnormed divisor
	dey           	// next divide iteration.
	bne  div1     	// loop until done 23 iterations.
	beq  mdend    	// norm. quotient and correct sign.
md2:	stx  m1+2
	stx  m1+1     	// clear mant1 (3 bytes) for mul/div.
	stx  m1
	bcs  ovchk    	// if calc. set carry,check for ovfl
	bmi  md3      	// if neg then no underflow.
	pla           	// pop one return level.
	pla
	bcc  normx    	// clear x1 and return.
md3:	eor  #$80     	// complement sign bit of exponent.
	sta  x1       	// store it.
	ldy  #$17     	// count 24 mul/23 div iterations.
	rts           	// return.
ovchk:	bpl  md3      	// if positive exp then no ovfl.
ovfl:	jmp  ovloc
fix1:	jsr  rtar
fix:	lda  x1
	bpl  undfl
	cmp  #$8e
	bne  fix1
	bit  m1
	bpl  fixrts
	lda  m1+2
	beq  fixrts
	inc  m1+1
	bne  fixrts
	inc  m1
fixrts:	rts
undfl:	lda  #$0
	sta  m1
	sta  m1+1
	rts
ovloc:	rts
