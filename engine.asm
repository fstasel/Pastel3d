//--------------------------------------------------
// RENDERING ENGINE
//--------------------------------------------------

.pc = *		"Rendering Engine"

//--------------------------------------------------

func_switchgfx:
	lda SCRCONT_REG
	eor #SCRCONT_EOR
	sta SCRCONT_REG
	lda MEMSET_REG
	eor #MEMSET_EOR
	sta MEMSET_REG
	lda gmode	// Skip mcm mode
	and #1		// if hires mode enabled
	bne !skip+
	lda SCRCONT_REG2
	eor #SCRCONT_EOR2 
	sta SCRCONT_REG2
!skip:	rts

//--------------------------------------------------

func_startRender:
	// Init rendering
	jsr func_readSettings
	jsr func_initRender
	:sto16(print_addr, text_render)
	jsr func_prints

	// Start rendering
	lda tmode_enabled	// Text mode?
	beq !no+
	jmp !yes+

!no:	// Main rendering routine
	:sto16(print_addr, text_palettepass)
	jsr func_prints

	jsr func_writeImageHeader // Write image header

	lda gmode	// Skip global palette pass
	and #1		// if hires mode enabled
	bne !skip+

	jsr func_fastRender
	jsr func_globalPalettePass

	jsr func_writeImageGlobal // Write image global data

!skip:	// Switch gfx mode
	jsr func_switchgfx

	// Prepare v_shift in case of laced modes
	:clr32(v_shift)
	lda gmode
	and #2
	beq !start+
	:mov32(scale_factor, v_shift)
	dec v_shift

!start:	jsr func_graphicRender

	jsr func_writeImageColmap // Write image colmap

	lda gmode	// Skip writing colsegment
	and #1		// if hires mode enabled
	bne !skip+

	jsr func_writeImageColSegment // Write image colsegment

!skip:	jsr func_writeImageBitmap // Write image bitmap

	// Re-start rendering in case of laced mode
	:fcmpz(v_shift)
	beq !stop+
	:clr32(v_shift)
	lda #$FF sta opcLoresLacedSwapper + 1
	lda #$FF sta opcHiresLacedSwapper + 1
	jmp !start-

!stop:	jsr func_fclose		  // Close image file
	rts

!yes:	// Text rendering routine
	jsr func_tmopc		// init text mode opcode
	jsr func_fastRender	// start fast rendering
	rts

//--------------------------------------------------

func_initRender:
	:sto16(print_addr, text_init)
	jsr func_prints
	
	:sto16(r6, objrot)
	:sto16(r7, Rmat)		// for all objects
	ldx numobj inx			// including cam
!loop:	txa pha

	ldy #0 lda (r6),y tay		// sina = sin(objrotx)
	jsr func_sincosY		// cosa = cos(objrotx)
	:mov32(param1, sina)
	:mov32(param2, cosa)
	:inc16(r6)

	ldy #0 lda (r6),y tay		// sinb = sin(objroty)
	jsr func_sincosY		// cosb = cos(objroty)
	:mov32(param1, sinb)
	:mov32(param2, cosb)
	:inc16(r6)
	
	ldy #0 lda (r6),y tay		// sing = sin(objrotz)
	jsr func_sincosY		// cosg = cos(objrotz)
	:mov32(param1, sing)
	:mov32(param2, cosg)
	:inc16(r6)

	// Compute inverse rot mat
	:mov32(cosb, FP1)		// Rmat[0] = cosb*cosg
	:fmulFP1(cosg)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosb, FP1)		// Rmat[1] = cosb*sing
	:fmulFP1(sing)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(sinb, FP1)		// Rmat[2] = -sinb
	jsr fcompl
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosg, FP1)		// Rmat[3] = cosg*sina*sinb-cosa*sing
	:fmulFP1(sina)
	:fmulFP1(sinb)
	:mov32(FP1, rr0)
	:mov32(cosa, FP1)
	:fmulFP1(sing)
	:fsubFP1(rr0)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(sing, FP1)		// Rmat[4] = sing*sina*sinb+cosa*cosg
	:fmulFP1(sina)
	:fmulFP1(sinb)
	:mov32(FP1, rr0)
	:mov32(cosa, FP1)
	:fmulFP1(cosg)
	:faddFP1(rr0)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosb, FP1)		// Rmat[5] = cosb*sina
	:fmulFP1(sina)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosg, FP1)		// Rmat[6] = cosg*cosa*sinb+sina*sing
	:fmulFP1(cosa)
	:fmulFP1(sinb)
	:mov32(FP1, rr0)
	:mov32(sina, FP1)
	:fmulFP1(sing)
	:faddFP1(rr0)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosa, FP1)		// Rmat[7] = cosa*sing*sinb-cosg*sina
	:fmulFP1(sing)
	:fmulFP1(sinb)
	:mov32(FP1, rr0)
	:mov32(cosg, FP1)
	:fmulFP1(sina)
	:fsubFP1(rr0)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	:mov32(cosb, FP1)		// Rmat[8] = cosb*cosa
	:fmulFP1(cosa)
	:mov32_indirect(FP1, r7)
	:add16_immediate(r7, 4, r7)

	pla tax
	dex
	beq !over+
	jmp !loop-
!over:	
	// Compute Wvec = Rmat_0 * campos
	:sto16(r0, Rmat)
	:sto16(r1, objpos)
	:sto16(r2, Wvec)
	jsr func_mulmatvec

	// Compute WTvec_i = Wvec + objpos_i
	:sto16(r0, objpos)
	:sto16(r1, Wvec)
	:sto16(r2, WTvec)		// for all objects
	ldx numobj inx			// including cam
!loop:	txa pha
	jsr func_addvec
	pla tax
	dex
	bne !loop-

	// Compute Bvec_i = Rmat_i * WTvec_i
	:sto16(r0, Rmat)
	:sto16(r1, WTvec)
	:sto16(r2, Bvec)		// for all objects
	ldx numobj inx			// including cam
!loop:	txa pha
	jsr func_mulmatvec
	:add16_immediate(r1, 12, r1)
	pla tax
	dex
	bne !loop-

	jsr func_clearHist
	ldx scale_factor
	inx
	stx scale_factor2
	rts

//--------------------------------------------------

func_initu0v0:
	// u0 = -width * 4 * scale_factor
	:add8_immediate(width, 2, FP1)
	lda width + 1 sta FP1 + 1
	lda width + 2 sta FP1 + 2
	lda width + 3 sta FP1 + 3
	jsr fcompl
	:fmulFP1(scale_factor)
	:mov32(FP1, u0)

	// v0 = height * 4 * scale_factor + v_shift
	:add8_immediate(height, 2, FP1)
	lda height + 1 sta FP1 + 1
	lda height + 2 sta FP1 + 2
	lda height + 3 sta FP1 + 3
	:fmulFP1(scale_factor)
	:faddFP1(v_shift)
	:mov32(FP1, v0)

	// duv = 8 * scalefactor
	:add8_immediate(scale_factor, 3, duv)
	lda scale_factor + 1 sta duv + 1
	lda scale_factor + 2 sta duv + 2
	lda scale_factor + 3 sta duv + 3

	// Set width8, height8
	lda TEXTWIDTH sta width8
	lda TEXTHEIGHT sta height8
	rts

//--------------------------------------------------

func_initu0v0fr:
	jsr func_initu0v0
opcFastRenderInit:
	jmp $0000
renderForHistogram:
	:mul8x(width8, height8, r0)
	:sto16(r1, 64)
!loop:	:cmp16(r0, r1)
	bcc renderForDisplay
	inc duv
	lsr width8
	lsr height8
	:shr16(r0)
	:shr16(r0)
	jmp !loop-
renderForDisplay:
	rts

//--------------------------------------------------

func_gmopc:			// opcode modifier function
	lda gmode		// for output gfx modes
	and #1
	beq !lores+
//hires
	lda #7				// 8x8 block mode
	sta opcPixelPerRasterBlock + 1
	sta opcPixelPerRasterBlock2 + 1
	:sto16(opcPalettePassMode + 1, func_palettePass_hires)
	:sto16(opcScanLine_U_step + 1, scale_factor)
	lda #ASL sta opc_accHistAddrMode
	jmp !next+
!lores:	
	lda #3				// 4x8 block mode
	sta opcPixelPerRasterBlock + 1
	sta opcPixelPerRasterBlock2 + 1
	:sto16(opcPalettePassMode + 1, func_palettePass_lores)
	:sto16(opcScanLine_U_step + 1, scale_factor2)
	lda #NOP sta opc_accHistAddrMode
!next:

//all gfx modes
	:sto16(opcFastRenderOutput + 1, func_accHist)
	:sto16(opcFastRenderInit + 1, renderForHistogram)
	lda #0 sta opcLoresLacedSwapper + 1
	lda #0 sta opcHiresLacedSwapper + 1
	rts

//--------------------------------------------------

func_tmopc:
	// Modify opcode for text paint mode
	:sto16(opcFastRenderOutput + 1, func_textpaint)
	:sto16(opcFastRenderInit + 1, renderForDisplay)
	rts

//--------------------------------------------------

func_clearHist:
	lda #0
	ldy #15
!loop:	sta chist,y
	dey
	bpl !loop-
	rts

//--------------------------------------------------

func_colorConverter:		// Input: (Xreg,Yreg) for 8x8 cell
	stx r0			// r0 = x
	sty r0 + 1		// r0 + 1 = y

	// Clear data if necessary
opcPixelPerRasterBlock2:
	cpx #7
	bne !noclear+
	cpy #7
	bne !noclear+
	jsr func_clearHist
!noclear:
	// Accumulate histogram
	jsr func_accHist
	// Check block complete
	lda r0
	bne !no+
	lda r0 + 1
	bne !no+
	jmp !ok+
!no:	rts
!ok:
	// Palette pass
opcPalettePassMode:
	jsr $0000
	rts

//--------------------------------------------------

func_globalPalettePass:		// Input: chist
	// Find histogram mode	// Output: r0 = max chist, r0 + 1 = first color
	lda #0
	sta r0 sta r0 + 1	// r0 = 0; r0 + 1 = 0
	ldx #15
!loop:	lda chist,x
	beq !cont+
	cmp r0			// if chist_i > r0
	bcc !cont+		// then
	sta r0			// r0 = chist_i
	stx r0 + 1		// r0 + 1 = i
!cont:	dex
	bpl !loop-
	// Set global background color
	lda r0 + 1
	sta BG1_REG
	rts

//--------------------------------------------------

func_palettePass_lores:		// Input: chist, blHSL
				// r0..r6: used
	// Find histogram modes
	lda #0
	sta r0 sta r0 + 1	// r0 = 0; r0 + 1 = 0
	sta r1 sta r1 + 1	// r1 = 0; r1 + 1 = 0
	sta r2 sta r2 + 1	// r2 = 0; r2 + 1 = 0
	ldx #15
!loop:	lda chist,x
	beq !cont+
	cpx BG1_REG
	beq !cont+
	cmp r0			// if chist_i > r0
	bcc !skip+		// then
	ldy r1 sty r2		// r2 = r1
	ldy r1 + 1 sty r2 + 1	// r2 + 1 = r1 + 1
	ldy r0 sty r1		// r1 = r0
	ldy r0 + 1 sty r1 + 1	// r1 + 1 = r0 + 1
	sta r0			// r0 = chist_i
	stx r0 + 1		// r0 + 1 = i
	jmp !cont+
!skip:	cmp r1			// else if chist_i > r1
	bcc !skip+		// then
	ldy r1 sty r2		// r2 = r1
	ldy r1 + 1 sty r2 + 1	// r2 + 1 = r1 + 1
	sta r1			// r1 = chist_i
	stx r1 + 1		// r1 + 1 = i
	jmp !cont+
!skip:	cmp r2			// else if chist_i > r2
	bcc !cont+		// then
	sta r2			// r2 = chist_i
	stx r2 + 1		// r2 + 1 = i
!cont:	dex
	bpl !loop-

	// Get color byte (first and second)
	lda r1 + 1
	asl asl asl asl
	ora r0 + 1
	sta r3			// r3 = (second,first) color
				// and additionally r2 + 1 = (third) color
	lda BG1_REG sta r4	// r4 = common bg (fourth) color

	// Compute color mapping bits
	ldx #15
!loop:	lda chist,x
	bne !cdist+
!cont:	dex
	bpl !loop-
	jmp !bloop+
!cdist:	// Match color by minimum distance
	txa
	asl asl asl asl
	tay
!loop:	lda disttable,y
	cmp r0 + 1
	beq !out10+
	cmp r1 + 1
	beq !out01+
	cmp r2 + 1
	beq !out11+
	cmp r4
	bne !next+
	lda #$00
	jmp !out+
!next:	iny
	jmp !loop-
!out10:	lda #$AA
	jmp !out+
!out01: lda #$11
	jmp !out+
!out11:	lda #$FF
!out:	sta cbit,x
	jmp !cont-

	// Block loop
!bloop:	ldy #0
	sty r4 + 1
!ly:	tya pha
	ldy r4 + 1
	ldx #0
!lx:
	txa pha
	lda blHSL,y
	tax
	lda coltable,x
	sta r5
	pla
	sta r5 + 1
	pla pha
	eor r5 + 1
opcLoresLacedSwapper:
	eor #$00		// $00 or $FF (2nd scan in laced)
	lsr
	bcs !sec+
	lda r5			// Extract primary color
	and #$0F
	tax
	lda cbit,x
	ldx r5 + 1
	jmp !cont+
!sec:	lda r5			// Extract secondary color
	lsr lsr lsr lsr
	tax
	lda cbit,x
	ldx r5 + 1
!cont:	lsr ror r6
	lsr ror r6

	iny
	inx
	cpx #4
	beq !ex+
	jmp !lx-
!ex:	sty r4 + 1
	pla tay
	lda r6
	sta bitblock,y
	iny
	cpy #8
	beq !ey+
	jmp !ly-
!ey:
	// Set block colors
	:mov16(cmptr, r5)	// Get colormap ptr
	lda r3
	ldy #0
	sta (r5),y
	:mov16(cm2ptr, r5)	// Get colormap2 ptr
	lda r2 + 1
	sta (r5),y
	
	// Prepare 8x8 block
	:mov16(bmptr, r6)	// Get bitmap ptr for first 8x1
	ldy #0
	ldx #7			// loop row
!loop:	// Set pixels
	lda bitblock,x
	sta (r6),y
	iny
	dex
	bpl !loop-

	// Update colormaps and bitmap ptr
	:inc16(cmptr)
	:inc16(cm2ptr)
	:add16_immediate(bmptr, 8, bmptr)
	rts

//--------------------------------------------------

func_palettePass_hires:		// Input: chist, blHSL
				// r0..r6: used
	// Find histogram modes
	lda #0
	sta r1 sta r1 + 1	// r1 = 0; r1 + 1 = 0
	sta r2 sta r2 + 1	// r2 = 0; r2 + 1 = 0
	ldx #15
!loop:	lda chist,x
	beq !cont+
	cmp r1			// if chist_i > r1
	bcc !skip+		// then
	ldy r1 sty r2		// r2 = r1
	ldy r1 + 1 sty r2 + 1	// r2 + 1 = r1 + 1
	sta r1			// r1 = chist_i
	stx r1 + 1		// r1 + 1 = i
	jmp !cont+
!skip:	cmp r2			// else if chist_i > r2
	bcc !cont+		// then
	sta r2			// r2 = chist_i
	stx r2 + 1		// r2 + 1 = i
!cont:	dex
	bpl !loop-

	// Get color byte
	lda r2 + 1
	asl asl asl asl
	ora r1 + 1
	sta r0			// r0 = (second,first) color

	// Compute color mapping bits
	ldx #15
!loop:	lda chist,x
	bne !cdist+
!cont:	dex
	bpl !loop-
	jmp !bloop+
!cdist:	// Match color by minimum distance
	txa
	asl asl asl asl
	tay
!loop:	lda disttable,y
	cmp r1 + 1
	beq !out0+
	cmp r2 + 1
	bne !next+
	lda #$FF
	jmp !out+
!next:	iny
	jmp !loop-
!out0:	lda #$00
!out:	sta cbit,x
	jmp !cont-

	// Block loop
!bloop:	ldy #0
	sty r0 + 1
!ly:	tya pha
	ldy r0 + 1
	ldx #0
!lx:
	txa pha
	lda blHSL,y
	tax
	lda coltable,x
	sta r3
	pla
	sta r3 + 1
	pla pha
	eor r3 + 1
opcHiresLacedSwapper:
	eor #$00		// $00 or $FF (2nd scan in laced)
	lsr
	bcs !sec+
	lda r3			// Extract primary color
	and #$0F
	tax
	lda cbit,x
	ldx r3 + 1
	jmp !cont+
!sec:	lda r3			// Extract secondary color
	lsr lsr lsr lsr
	tax
	lda cbit,x
	ldx r3 + 1
!cont:	asl
	ror r4

	iny
	inx
	cpx #8
	beq !ex+
	jmp !lx-
!ex:	sty r0 + 1
	pla tay
	lda r4
	sta bitblock,y
	iny
	cpy #8
	beq !ey+
	jmp !ly-
!ey:
	// Set block colors
	:mov16(cmptr, r5)	// Get colormap ptr
	lda r0
	ldy #0
	sta (r5),y
	
	// Prepare 8x8 block
	:mov16(bmptr, r6)	// Get bitmap ptr for first 8x1
	ldy #0
	ldx #7			// loop row
!loop:	// Set pixels
	lda bitblock,x
	sta (r6),y
	iny
	dex
	bpl !loop-

	// Update colormap and bitmap ptr
	:inc16(cmptr)
	:add16_immediate(bmptr, 8, bmptr)
	rts

//--------------------------------------------------

func_accHist:		// Input: colh, cols, coll: color
			// r0 = x, r0 + 1 = y
			// Output: chist: filled
			// r1,r2,r3: used

	lda r0 + 1	// Block pixel index
	asl asl
opc_accHistAddrMode:
	asl		// asl (hires) or nop (lores)
	ora r0
	tay
	lda colh
	sta r1
	lda cols
	sta r2
	lda coll
	sta r3
	asl r1 rol
	asl r1 rol
	asl r1 rol
	asl r2 rol
	asl r2 rol
	asl r3 rol
	asl r3 rol
	asl r3 rol

	// Dithering
	sta r1
	lda r3
	cmp $D41B
	bcs !skip+
	lda r1
	and #$07
	beq !skip+
	dec r1
!skip:	lda r1
	sta blHSL,y
	tay
	lda coltable,y
	tay
	and #$0F
	tax
	inc chist,x	// Accumulate histogram for primary color
	tya
	lsr lsr lsr lsr
	tax
	inc chist,x	// Accumulate histogram for secondary color
	rts

//--------------------------------------------------

func_graphicRender:
	// Set bitmap and colmap ptr
	:sto16(bmptr0, BITMAP)
	:sto16(cmptr0, COLMAP)
	:sto16(cm2ptr0, COL_SEGMENT)

	// Init u0,v0
	jsr func_initu0v0
	
	lda #0
	sta paint_y		// paint_y = 0
	:mov32(v0, v)		// v = v0
!ldy:	lda #0
	sta paint_x		// paint_x = 0
	:mov32(u0, u)		// u = u0
	:mov16(bmptr0, bmptr)
	:mov16(cmptr0, cmptr)
	:mov16(cm2ptr0, cm2ptr)

!ldx:	ldy #7			// Yreg = 7
!ly:	tya pha

opcPixelPerRasterBlock:
	ldx #7			// Xreg = 7 (hires) or 3 (lores)
!lx:	txa pha

	jsr func_engine

	pla tax pla tay		// Recall Xreg and Yreg
	tya pha txa pha
	jsr func_colorConverter	// Color converter subroutine

	:mov32(u, FP1)		// u + sc -> u or u + sc*2 -> u (lores)
opcScanLine_U_step:
	:faddFP1(scale_factor)
	:mov32(FP1, u)
	pla tax
	dex			// Xreg--
	bmi !ex+
	jmp !lx-

!ex:	:mov32(u, FP2)		// u - duv -> u
	:fsubFP2(duv)
	:mov32(FP1, u)
	:mov32(v, FP2)		// v - sc -> v
	:fsubFP2(scale_factor)
	:mov32(FP1, v)
	pla tay
	dey			// Yreg--
	bmi !ey+
	jmp !ly-

!ey:	:mov32(v, FP1)		// v + duv -> v
	:faddFP1(duv)
	:mov32(FP1, v)
	:mov32(u, FP1)		// u + duv -> u
	:faddFP1(duv)
	:mov32(FP1, u)
	inc paint_x		// paint_x++
	lda paint_x		// cmp paint_x,width
	cmp width8
	bpl !edx+
	jmp !ldx-

!edx:	:mov32(v, FP2)		// v - duv -> v
	:fsubFP2(duv)
	:mov32(FP1, v)
	:add16_immediate(bmptr0, 320, bmptr0)
	:add16_immediate(cmptr0, 40, cmptr0)
	:add16_immediate(cm2ptr0, 40, cm2ptr0)
	inc paint_y		// paint_y++
	lda paint_y		// cmp paint_y,height
	cmp height8
	bpl !edy+
	jmp !ldy-
!edy:	rts

//--------------------------------------------------

func_fastRender:
	// Init u0,v0
	jsr func_initu0v0fr

	lda #0
	sta paint_y		// paint_y = 0
	:mov32(v0, v)		// v = v0
!ldy:	lda #0
	sta paint_x		// paint_x = 0
	:mov32(u0, u)		// u = u0
!ldx:

	jsr func_engine

	// Put paint x,y into r0, r0 + 1 (necessary for hist. acc.)
	lda paint_x
	sta r0
	lda paint_y
	sta r0 + 1

opcFastRenderOutput:
	jsr $0000

	:mov32(u, FP1)		// u + duv -> u
	:faddFP1(duv)
	:mov32(FP1, u)
	inc paint_x		// paint_x++
	lda paint_x		// cmp paint_x,width
	cmp width8
	bpl !edx+
	jmp !ldx-

!edx:	:mov32(v, FP2)		// v - duv -> v
	:fsubFP2(duv)
	:mov32(FP1, v)
	inc paint_y		// paint_y++
	lda paint_y		// cmp paint_y,height
	cmp height8
	bpl !edy+
	jmp !ldy-
!edy:	rts

//--------------------------------------------------

func_textpaint:
	lda colh sta r1
	lda cols sta r2
	lda coll sta r3
	:getColor(r1,r2,r3)
	sta paint_col
	jsr func_paint
	rts

//--------------------------------------------------

func_engine:
	:mov32(u, FP2)		// px = u / focal
	:fdivFP2(objscale)
	:mov32(FP1, px)
	:mov32(v, FP2)		// py = v / focal
	:fdivFP2(objscale)
	:mov32(FP1, py)

	// Compute Q = R*[px py 1]
	:mov32(Rmat, FP1)
	:fmulFP1(px)
	:mov32(FP1, Qvec)
	:mov32(Rmat + 4, FP1)
	:fmulFP1(py)
	:faddFP1(Qvec)
	:faddFP1(Rmat + 8)
	:mov32(FP1, Qvec)
	:mov32(Rmat + 12, FP1)
	:fmulFP1(px)
	:mov32(FP1, Qvec + 4)
	:mov32(Rmat + 16, FP1)
	:fmulFP1(py)
	:faddFP1(Qvec + 4)
	:faddFP1(Rmat + 20)
	:mov32(FP1, Qvec + 4)
	:mov32(Rmat + 24, FP1)
	:fmulFP1(px)
	:mov32(FP1, Qvec + 8)
	:mov32(Rmat + 28, FP1)
	:fmulFP1(py)
	:faddFP1(Qvec + 8)
	:faddFP1(Rmat + 32)
	:mov32(FP1, Qvec + 8)

	// Object geometry call
	:sto16(r11, WTvec + 12) // ptr to WTvec of the 1st obj
	:sto16(r10, objscale + 4) // ptr to scale of the 1st obj
	:sto16(r9, objpos + 12)	// ptr to pos of the 1st obj
	:sto16(r8, Rmat + 36)	// ptr to rotmat of the 1st obj
	:sto16(r7, s + 4)	// ptr to s of the 1st obj
	:sto16(r6, Bvec + 12)	// ptr to Bvec of the 1st obj
	ldy #1			// shift for the 1st obj
	ldx numobj
!loop:	txa pha
	tya pha

	// !!!! DONT MODIFY R6-R11 IN GEOMETRY ROUTINE !!!!
	lda objtype,y
	cmp #1
	bne !skip+
	jsr func_planeGeometry	// objtype = 1
	jmp !out+
!skip:	cmp #2
	bne !out+
	jsr func_sphereGeometry	// objtype = 2

!out:	pla tay iny
	:add16_immediate(r6, 12, r6)
	:add16_immediate(r7, 4, r7)
	:add16_immediate(r8, 36, r8)
	:add16_immediate(r9, 12, r9)
	:add16_immediate(r10, 4, r10)
	:add16_immediate(r11, 12, r11)
	pla tax dex
	bne !loop-

	// Choose front
	lda #0
	sta colh
	sta cols
	sta coll
	:fone(rr2)	// rr2 = 1
	lda #$FF	// smax (max fp) -> rr0
	sta rr0
	sta rr0 + 2
	sta rr0 + 3
	lda #$7F
	sta rr0 + 1
	lda #4		// shift for s of the 1st obj
	sta r4
	ldy #1		// shift for si and li of the 1st obj
	ldx numobj
!loop:	txa pha
	tya pha
	ldy r4
	:mov32Y_(s, FP1)
	:fcmpFP1(rr0)	// s_i <= smax AND
	bpl !skip+
	:fcmpFP1(rr2)	// s_i >= 1
	bmi !skip+
!ok:	:mov32Y_(s, rr0)// then smax = s_i
	pla tay pha
	lda colh,y sta colh	// colh = colh_i
	lda cols,y sta cols	// cols = cols_i
	lda coll,y sta coll	// coll = coll_i
!skip:	pla tay iny
	:add8_immediate(r4, 4, r4)
	pla tax dex
	beq !over+
	jmp !loop-
!over:
	rts

//--------------------------------------------------

func_planeGeometry:
	sty r20 + 1		// Save byte shift

	// Avec = Rmat_i * Qvec
	:mov16(r8, r0)		// Rmat
	:sto16(r1, Qvec)
	:sto16(r2, Avec)
	jsr func_mulmatvec
	
	// s_i = Bvec_i / Avec_2
	ldy #8
	:mov32indirectY_(r6, FP2)
	:fdivFP2(Avec + 8)
	:mov32_indirect(FP1, r7)

	// x = s_i * Avec_0 - Bvec_i_0
	// y = s_i * Avec_1 - Bvec_i_1
	:mov32(FP1, rr4)
	:fmulFP1(Avec)
	:mov32(FP1, FP2)
	:mov32indirect_(r6, FP1)
	jsr fsub
	:mov32(FP1, rr2)	// x (rr2)
	:mov32(rr4, FP1)
	:fmulFP1(Avec + 4)
	:mov32(FP1, FP2)
	ldy #4
	:mov32indirectY_(r6, FP1)
	jsr fsub
	:mov32(FP1, rr4)	// y (rr4)

	// Checkerboard
	ldy r20 + 1
	lda objcolorH,y
	sta colh,y
	lda objcolorS,y
	sta cols,y
	lda objcolorL,y
	sta coll,y
	:mov32indirect_(r10, FP1)
	:fcmpo(FP1)
	bmi nochecker
	:fdivFP1(rr2)
	jsr fix
	lda FP1 + 2 sta r20
	:mov32indirect_(r10, FP1)
	:fdivFP1(rr4)
	jsr fix
	lda r20 eor FP1 + 2
	sta r20
	lda rr2 + 1 eor rr4 + 1
	asl rol eor r20
	lsr
	bcc nochecker
	ldy r20 + 1
	lda #0
	sta colh,y
	sta cols,y
	sta coll,y
nochecker:
	// xx = Rmat_i_0 * x + R_mat_i_3 * y + objposx_i
	// yy = Rmat_i_1 * x + R_mat_i_4 * y + objposy_i
	// zz = Rmat_i_2 * x + R_mat_i_5 * y + objposz_i
	:mov32indirect_(r8, FP1)	// Rmat_0
	:fmulFP1(rr2)
	:mov32(FP1, rr12)
	ldy #4
	:mov32indirectY_(r8, FP1)	// Rmat_1
	:fmulFP1(rr2)
	:mov32(FP1, rr14)
	ldy #8
	:mov32indirectY_(r8, FP1)	// Rmat_2
	:fmulFP1(rr2)
	:mov32(FP1, rr16)
	ldy #12
	:mov32indirectY_(r8, FP1)	// Rmat_3
	:fmulFP1(rr4)
	:faddFP1(rr12)
	:mov32(FP1, rr12)
	ldy #16
	:mov32indirectY_(r8, FP1)	// Rmat_4
	:fmulFP1(rr4)
	:faddFP1(rr14)
	:mov32(FP1, rr14)
	ldy #20
	:mov32indirectY_(r8, FP1)	// Rmat_5
	:fmulFP1(rr4)
	:faddFP1(rr16)
	:mov32(FP1, rr16)
	:mov32indirect_(r9, FP1)	// objposx
	:faddFP1(rr12)
	:mov32(FP1, xx)			// xx
	ldy #4
	:mov32indirectY_(r9, FP1)	// objposy
	:faddFP1(rr14)
	:mov32(FP1, yy)			// yy
	ldy #8
	:mov32indirectY_(r9, FP1)	// objposz
	:faddFP1(rr16)
	:mov32(FP1, zz)			// zz
	
	// nx ny nz
	ldy #24
	:mov32indirectY_(r8, nx)	// nx
	ldy #28
	:mov32indirectY_(r8, ny)	// ny
	ldy #32
	:mov32indirectY_(r8, nz)	// nz

	ldy r20 + 1			// Byte shift
	lda colh,y			// No illumination
	bne !light+			// for black squares
	lda cols,y
	bne !light+
	lda coll,y
	bne !light+
	rts
!light:	jsr func_processLight
	rts

//--------------------------------------------------

func_sphereGeometry:
	sty r20 + 1			// Save byte shift
	
	// Avec_0 = Qvec.Qvec
	:sto16(r0, Qvec)		// r0 -> Qvec
	:sto16(r1, Qvec)		// r1 -> Qvec
	jsr func_dotvec			// rr4 = FP1 = Qvec.Qvec
	:mov32(FP1, Avec)

	// Avec_1 = -2 * Qvec.WTvec_i
	:sto16(r0, Qvec)		// r0 -> Qvec
	:mov16(r11, r1)			// r1 -> WTvec_i
	jsr func_dotvec			// rr4 = FP1 = Qvec.WTvec_i
	jsr fcompl			// FP1 = -FP1
	inc FP1				// FP1 *= 2
	:mov32(FP1, Avec + 4)

	// Avec_2 = WTvec_i.WTvec_i - radius^2
	:mov16(r11, r0)			// r0 -> Wtvec_i, r1 not changed
	jsr func_dotvec			// rr4 = WTvec_i.WTvec_i
	:mov32indirect_(r10, FP1)	// FP1 = radius
	:mov32(FP1, FP2)
	jsr fmul			// FP1 = radius^2
	:mov32(rr4, FP2)
	jsr fsub			// WTvec_i.WTvec_i - radius^2
	:mov32(FP1, Avec + 8)

	// Discriminant = Avec_1 ^ 2 - 4*Avec_0*Avec_2
	:mov32(Avec + 4, FP1)
	:mov32(Avec + 4, FP2)
	jsr fmul
	:mov32(FP1, rr12)		// Avec_1 ^ 2
	:mov32(Avec, FP1)
	:mov32(Avec + 8, FP2)
	jsr fmul			// Avec_0*Avec_2
	inc FP1 inc FP1			// 4*Avec_0*Avec_2
	:mov32(rr12, FP2)
	jsr fsub
	:mov32(FP1, rr12)		// rr12 is discriminant

	// if discr. < 0 then s_i = 0
	lda rr12 + 1
	bpl !pos+
!zeros:	ldy #0
	lda #0
	sta (r7),y iny
	sta (r7),y iny
	sta (r7),y iny
	sta (r7),y
	jmp !out+

	// discr. = sqrt(discr.)
!pos:	:fsqrt(rr12, rr4, r0, r1, r2)
	:mov32(rr4, rr12)		// rr12 = sqrt(discr.)

	// s1 = (-Avec_1 + discr) / (2 * Avec_0)
	:mov32(Avec, rr4)
	inc rr4				// rr4 = 2 * Avec_0
	:mov32(Avec + 4, FP1)
	jsr fcompl
	:mov32(FP1, rr14)		// rr14 = -Avec_1
	:mov32(rr12, FP2)
	jsr fadd			// -Avec_1 + discr
	:mov32(FP1, FP2)
	:mov32(rr4, FP1)
	jsr fdiv
	:mov32(FP1, rr16)		// rr16 is s1

	// s2 = (-Avec_1 - discr) / (2 * Avec_0)
	:mov32(rr14, FP2)
	:mov32(rr12, FP1)
	jsr fsub			// -Avec_1 - discr
	:mov32(FP1, FP2)
	:mov32(rr4, FP1)
	jsr fdiv
	:mov32(FP1, rr18)		// rr18 is s2

	// if s1 < 1 && s2 < 1 then s_i = 0
	:fcmpo(rr16)
	bpl !s1ok+
	:fcmpo(rr18)
	bpl !s2ok+
	// Sphere is not in sight
	jmp !zeros-
!s2ok:	// Cam inside sphere: s1 is behind, use s2
!uses2:	:mov32_indirect(rr18, r7)	// s_i output
	:mov32(rr18, rr4)		// rr4 is s_i
	jmp !cont+
!s1ok:	:fcmpo(rr18)
	bpl !usem+
	// Cam inside sphere: s2 is behind, use s1
!uses1:	:mov32_indirect(rr16, r7)	// s_i output
	:mov32(rr16, rr4)		// rr4 is s_i
	jmp !cont+
!usem:	// Use the side in sight
	:mov32(rr16, FP1)
	:fcmpFP1(rr18)
	bmi !uses1-
	jmp !uses2-
!cont:
	// Compute surface point
	:mov32(rr4, FP1)
	:mov32(Qvec, FP2)
	jsr fmul
	:mov32(FP1, FP2)
	:mov32indirect_(r11, FP1)
	jsr fsub
	:mov32(FP1, rr12)		// rr12 = x = s_i*Qvec_0 - WTvec_i_0
	:mov32(rr4, FP1)
	:mov32(Qvec + 4, FP2)
	jsr fmul
	:mov32(FP1, FP2)
	ldy #4
	:mov32indirectY_(r11, FP1)
	jsr fsub
	:mov32(FP1, rr14)		// rr14 = y = s_i*Qvec_1 - WTvec_i_1
	:mov32(rr4, FP1)
	:mov32(Qvec + 8, FP2)
	jsr fmul
	:mov32(FP1, FP2)
	ldy #8
	:mov32indirectY_(r11, FP1)
	jsr fsub
	:mov32(FP1, rr16)		// rr16 = z = s_i*Qvec_2 - WTvec_i_2

	// Translate surface point w.r.t. world coord. sys.
	:mov32(rr12, FP1)
	:mov32indirect_(r9, FP2)
	jsr fadd
	:mov32(FP1, xx)			// xx = x + posx
	:mov32(rr14, FP1)
	ldy #4
	:mov32indirectY_(r9, FP2)
	jsr fadd
	:mov32(FP1, yy)			// yy = y + posy
	:mov32(rr16, FP1)
	ldy #8
	:mov32indirectY_(r9, FP2)
	jsr fadd
	:mov32(FP1, zz)			// zz = z + posz
	
	// Compute surface normal
	:mov32indirect_(r10, rr4)
	:mov32(rr4, FP1)
	:fdivFP1(rr12)
	:mov32(FP1, nx)			// nx = x / radius
	:mov32(rr4, FP1)
	:fdivFP1(rr14)
	:mov32(FP1, ny)			// ny = y / radius
	:mov32(rr4, FP1)
	:fdivFP1(rr16)
	:mov32(FP1, nz)			// nz = z / radius

	// Set object color
!out:	ldy r20 + 1
	lda objcolorH,y
	sta colh,y
	lda objcolorS,y
	sta cols,y
	lda objcolorL,y
	sta coll,y

	// Process light source
	jsr func_processLight
	rts

//--------------------------------------------------

func_processLight:
	sty r20 + 1			// Input: xx, yy, zz
	:mov32(lsx, FP2)		//	  nx, ny, nz
	:fsubFP2(xx)			//	  Yreg: Byte shift for colh, cols, coll
	:mov32(FP1, rr12) // dx=lsx-xx	// Output: coll updated
	:mov32(lsy, FP2)		//	   r6-r11: not disturbed
	:fsubFP2(yy)
	:mov32(FP1, rr14) // dy=lsy-yy
	:mov32(lsz, FP2)
	:fsubFP2(zz)
	:mov32(FP1, rr16) // dz=lsz-zz
	:fmulFP1(rr16)		// dz^2
	:mov32(FP1, rr18)	// rr18 = dz^2
	:mov32(rr14, FP1)
	:fmulFP1(rr14)		// dy^2
	:faddFP1(rr18)
	:mov32(FP1, rr18)	// rr18 += dy^2
	:mov32(rr12, FP1)
	:fmulFP1(rr12)		// dx^2
	:faddFP1(rr18)
	:mov32(FP1, rr18)	// rr18 += dx^2
	:fsqrt(rr18, rr4, r0, r1, r2)	// rr4 = sqrt(rr18)
	:mov32(rr12, FP2)
	:fdivFP2(rr4)
	:mov32(FP1, rr12)	// dx norm. 
	:mov32(rr14, FP2)
	:fdivFP2(rr4)
	:mov32(FP1, rr14)	// dy norm.
	:mov32(rr16, FP2)
	:fdivFP2(rr4)
	:mov32(FP1, rr16)	// dz norm.

	:sto16(r0, rr12)	// ptr to {dx, dy, dx}
	:sto16(r1, nx)		// ptr to {nx, ny, nz}
	jsr func_dotvec		// rr4 = dx*nx+dy*ny+dz*nz
				// rr4 is illumination coef.
	:fzero(FP1)
	:fcmpFP1(rr4)		// cmp 0,rr4
	bmi !skip+
	ldy r20 + 1		// if rr4 < 0, shadow zone, no light
	lda #0
	sta coll,y
	jmp !ambience+
!skip:	:fcmpo(rr4)		// cmp rr4, 1
	bmi !over+
	:fone(rr4)		// set rr4 = 1 if rr4 > 1
!over:
	// Convert relative luminance fix to float
	ldy r20 + 1
	lda coll,y		// coll -= lamb
	sec
	sbc lamb
	bcs !skip+
	lda #0
!skip:	
	sta m1 + 1
	lda #0
	sta m1
	sta m1 + 2
	jsr float
	
	// Compute coll *= ic^2
	:mov32(rr4, FP2)
	jsr fmul
	:mov32(rr4, FP2)
	jsr fmul
	jsr fix
	ldy r20 + 1
	lda m1 + 1
	sta coll,y		// scaled lum

!ambience:
	// Ambience
	lda lamb		// coll += lamb
	clc adc coll,y
	bcc !skip+
	lda #$FF
!skip:
	sta coll,y
	rts

//--------------------------------------------------

func_readSettings:
	lda GMODE sta gmode

	:mov16(TEXTWIDTH, param1)
	jsr func_convert16toFP
	:mov32(param1, width)

	:mov16(TEXTHEIGHT, param1)
	jsr func_convert16toFP
	:mov32(param1, height)

	:mov16(SCALEF, param1)
	jsr func_convert16toFP
	:mov32(param1, scale_factor)

	lda TMODEENABLED sta tmode_enabled
	lda NUMOBJECTS sta numobj

	:sto16(r0, objtype)
	:sto16(r1, objpos)
	:sto16(r2, objrot)
	:sto16(r3, objscale)
	:sto16(r4, objcolorH)
	:sto16(r5, objcolorS)
	:sto16(r6, objcolorL)
	ldx numobj
	inx
	lda #0 sta r7
!loop:	txa pha 

	ldy r7 ldx #0
	lda OBJTYPE,y sta (r0,x)
	:inc16(r0)

	ldy r7
	:mov16Y_(OBJPOSX, param1)
	jsr func_convert16toFP
	:mov32_indirect(param1, r1)
	:add16_immediate(r1, 4, r1)

	ldy r7
	:mov16Y_(OBJPOSY, param1)
	jsr func_convert16toFP
	:mov32_indirect(param1, r1)
	:add16_immediate(r1, 4, r1)

	ldy r7
	:mov16Y_(OBJPOSZ, param1)
	jsr func_convert16toFP
	:mov32_indirect(param1, r1)
	:add16_immediate(r1, 4, r1)

	ldy r7 ldx #0
	lda OBJROTX,y sta (r2,x)
	:inc16(r2)
	lda OBJROTY,y sta (r2,x)
	:inc16(r2)
	lda OBJROTZ,y sta (r2,x)
	:inc16(r2)

	ldy r7
	:mov16Y_(OBJSCALE, param1)
	jsr func_convert16toFP
	:mov32_indirect(param1, r3)
	:add16_immediate(r3, 4, r3)

	// Read and convert from HSL-8bit to HSL-24bit
	ldy r7 ldx #0
	lda OBJCOLOR,y
	and #$E0
	bpl !skip+
	ora #$1F
!skip:	sta (r4,x)
	:inc16(r4)

	lda OBJCOLOR,y
	and #$18 asl asl asl
	bpl !skip+
	ora #$3F
!skip:	sta (r5,x)
	:inc16(r5)

	lda OBJCOLOR,y
	asl asl asl asl asl
	bpl !skip+
	ora #$1F
!skip:	sta (r6,x)
	:inc16(r6)

	:add8_immediate(r7, OBJATTRIB_LEN, r7)
	pla tax dex
	beq !over+
	jmp !loop-

!over:	ldy r7
	:mov16Y_(OBJTYPE, param1)
	jsr func_convert16toFP
	:mov32(param1, lsx)
	ldy r7 iny iny sty r7

	:mov16Y_(OBJTYPE, param1)
	jsr func_convert16toFP
	:mov32(param1, lsy)
	ldy r7 iny iny sty r7

	:mov16Y_(OBJTYPE, param1)
	jsr func_convert16toFP
	:mov32(param1, lsz)
	ldy r7 iny iny

	lda OBJTYPE,y
	sta lamb

	// Init gfx mode opcode
	jsr func_gmopc

	rts

//--------------------------------------------------
// IMAGE LOAD/SAVE FUNCTIONS
//--------------------------------------------------

func_writeImageHeader:
	jsr func_fopenwrite
	:sto16(address, gmode)
	:sto16(length, 1)
	jsr func_fwrite
	:sto16(address, TEXTWIDTH)
	:sto16(length, 1)
	jsr func_fwrite
	:sto16(address, TEXTHEIGHT)
	:sto16(length, 1)
	jsr func_fwrite
	rts

//--------------------------------------------------

func_writeImageGlobal:
	:sto16(address, BG1_REG)
	:sto16(length, 1)
	jsr func_fwrite
	rts

//--------------------------------------------------

func_writeImageColmap:
	:sto16(address, COLMAP)
	:sto16(length, 1000)
	jsr func_fwrite
	rts

//--------------------------------------------------

func_writeImageColSegment:
	:sto16(address, COL_SEGMENT)
	:sto16(length, 1000)
	jsr func_fwrite
	rts

//--------------------------------------------------

func_writeImageBitmap:
	:sto16(address, BITMAP)
	:sto16(length, 8000)
	jsr func_fwrite
	rts

//--------------------------------------------------
// DATA
//--------------------------------------------------

// Import data
settings: .import source "settings.inc"
.import source "palette.inc"

text_title:	.fill 35, toPet("Pastel 3D Rendering Engine for C=64",i) .byte 13
		.fill 14, toPet("By FST (c)2015", i) .byte 13
		.fill 25, toPet("Version 1.0 (asm/float32)",i) .byte 13, 13, 0
text_init:	.fill 15, toPet("Initializing...",i) .byte 13, 0
text_render:	.fill 12, toPet("Rendering...",i) .byte 13, 0
text_palettepass:
		.fill 15, toPet("Palette pass...",i) .byte 13, 0
sintableH:	.fill 256,>round(32767*sin(toRadians(i*360/256)))
sintableL:	.fill 256,<round(32767*sin(toRadians(i*360/256)))

//--------------------------------------------------
// VARIABLES
//--------------------------------------------------

Rmat:		.fill MAX_OBJECTS * 9 * 4, 0
Qvec:		.dword 0, 0, 0
Wvec:		.dword 0, 0, 0
Avec:		.dword 0, 0, 0
Pvec:		.dword 0, 0
WTvec:		.fill MAX_OBJECTS * 3 * 4, 0
Bvec:		.fill MAX_OBJECTS * 3 * 4, 0
s:		.fill MAX_OBJECTS * 4, 0
colh:		.fill MAX_OBJECTS, 0
cols:		.fill MAX_OBJECTS, 0
coll:		.fill MAX_OBJECTS, 0
DLvec:		.dword 0, 0, 0
cosa:		.dword 0
cosb:		.dword 0
cosg:		.dword 0
sina:		.dword 0
sinb:		.dword 0
sing:		.dword 0
u:		.dword 0
v:		.dword 0
v_shift:	.dword 0
px:		.dword 0
py:		.dword 0
u0:		.dword 0
v0:		.dword 0
duv:		.dword 0
xx:		.dword 0
yy:		.dword 0
zz:		.dword 0
nx:		.dword 0
ny:		.dword 0
nz:		.dword 0
blHSL:		.fill 64,0
chist:		.fill 16,0
cbit:		.fill 16,0
bitblock:	.fill 8,0
bmptr0:		.word 0
cmptr0:		.word 0
cm2ptr0:	.word 0
bmptr:		.word 0
cmptr:		.word 0
cm2ptr:		.word 0

//--------------------------------------------------
// SETTINGS VARIABLES
//--------------------------------------------------

gmode:		.byte 0
width:		.dword 0
height:		.dword 0
width8:		.byte 0
height8:	.byte 0
scale_factor:	.dword 0
scale_factor2:	.byte 0
tmode_enabled:	.byte 0
numobj:		.byte 0
objtype:	.fill MAX_OBJECTS,0
objpos:		.fill MAX_OBJECTS * 3 * 4, 0
objrot:		.fill MAX_OBJECTS * 3, 0
objscale:	.fill MAX_OBJECTS * 4, 0
objcolorH:	.fill MAX_OBJECTS, 0
objcolorS:	.fill MAX_OBJECTS, 0
objcolorL:	.fill MAX_OBJECTS, 0
lsx:		.dword 0
lsy:		.dword 0
lsz:		.dword 0
lamb:		.byte 0

//--------------------------------------------------
