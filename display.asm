//--------------------------------------------------
// IMAGE DISPLAY TOOL FOR PASTEL 3D
// By FST 2015(c)
//--------------------------------------------------

// Includes
.import source "macros.inc"

.const CODE_SEGMENT = $080D	// Code segment
.const IO_PRINTSTR = $AB1E	// BASIC ROM routine for print
.const KERNAL_OPEN = $FFC0	// File open
.const KERNAL_CLOSE = $FFC3	// File close
.const KERNAL_CLRCHN = $FFCC	// Clear channels
.const KERNAL_SETNAM = $FFBD	// Set filename
.const KERNAL_SETLFS = $FFBA	// Set file params
.const KERNAL_CHKIN = $FFC6	// Set default input
.const KERNAL_CHKOUT = $FFC9 	// Set default output
.const KERNAL_CHRIN = $FFCF	// Read byte
.const KERNAL_CHROUT = $FFD2	// Write byte
.const KERNAL_LOAD = $FFD5	// Load file
.const KERNAL_SAVE = $FFD8	// Save file
.const SCRCONT_REG = $D011	// Screen control register #1
.const SCRCONT_REG2 = $D016	// Screen control register #2
.const MEMSET_REG = $D018	// Memory setup register
.const COL_SEGMENT = $D800	// Color table
.const BORDER_REG = $D020	// Border color register
.const BG1_REG = $D021		// BG#1 color register
.const CIA2_PA = $DD00		// CIA#2 Port A setup register
.const CIA1_IC = $DC0D		// CIA#1 Interrupt control reg
.const CIA1_IS = $DD0D		// CIA#1 Interrupt status reg
.const RASTERLINE = $D012	// Rasterline for interrupt
.const INTVECL = $0314		// Interrupt vector Low
.const INTVECH = $0315		// Interrupt vector High
.const VIC_ICR = $D01A		// VIC Interrupt control reg
.const VIC_ISR = $D019		// VIC Interrupt status reg
.const COLMAP1 = $4000		// Colormap for the 1st screen
.const BITMAP1 = $6000		// Bitmap for the 1st screen
.const COLMAP2 = $8000		// Colormap for the 2nd screen
.const BITMAP2 = $A000		// Bitmap for the 2nd screen
.const TAILRL = 0

//--------------------------------------------------
// REGISTERS
//--------------------------------------------------
.const r0 = $52
.const r1 = $54
.const r2 = $56
.const r3 = $58
.const r4 = $5A
.const r5 = $5C
.const r6 = $5E
.const r7 = $60

//--------------------------------------------------
// CODE
//--------------------------------------------------

// Basic Upstart Module
.pc = $0801	"Basic Upstart"
:BasicUpstart(MAIN)

// Main
.pc = CODE_SEGMENT	"Code"

MAIN:
	jsr func_prompt
	jsr func_loadImage
	jsr func_initMode
	jsr func_activate
	jsr KERNAL_CHRIN
	jsr func_restoreMode
	rts

//--------------------------------------------------

int_head:
	// Store registers
	sta opc_a2 + 1
	stx opc_x2 + 1
	sty opc_y2 + 1

	lda #<int_tail	// Go back to tail int.
	sta INTVECL
	lda #>int_tail
	sta INTVECH
	lda #TAILRL
	sta RASTERLINE
	lda SCRCONT_REG
	ora #$80
	sta SCRCONT_REG

	asl VIC_ISR	// Ack int.

	// Restore registers
opc_a2:	lda #$00
opc_x2:	ldx #$00
opc_y2:	ldy #$00
	jmp $EA7E

//--------------------------------------------------

int_tail:
	// Store registers
	sta opc_a + 1
	stx opc_x + 1
	sty opc_y + 1
lda BORDER_REG
eor #$FF
sta BORDER_REG
	// Handle laced-modes
	lda CIA2_PA
	eor #3
	sta CIA2_PA

	// Handle lores-laced mode
	lda gmode
	and #1
	bne !skip+
	lda CIA2_PA	// Check current bank
	and #$03
	cmp #2
	beq !usecs1+
	lda #<colseg2	// copy colseg2
	sta opc1 + 1
	lda #>colseg2
	sta opc2 + 1
//	jsr func_copyseg2
	jmp !switch+
//	jmp !skip+
!usecs1:
	lda #<colseg1	// copy colseg1
	sta opc1 + 1
	lda #>colseg1
	sta opc2 + 1
!switch:
	jsr func_copyseg
//	jsr func_copyseg1
!skip:

	// Configure next interrupt
	lda gmode	// Fli mode?
	and #4
	beq !no+
	lda #<int_head 	// If yes
	sta INTVECL	// then, use header interrupt
	lda #>int_head	// at line:0
	sta INTVECH
	lda #0
	sta RASTERLINE
	lda SCRCONT_REG
	and #$7F
	sta SCRCONT_REG
!no:	asl VIC_ISR	// Ack int.

lda BORDER_REG
eor #$FF
sta BORDER_REG

	// Restore registers
opc_a:	lda #$00
opc_x:	ldx #$00
opc_y:	ldy #$00
	jmp $EA31

//--------------------------------------------------

func_restoreMode:
	sei
	lda scr1	// restore setup registers
	sta SCRCONT_REG
	lda msr
	sta MEMSET_REG
	lda scr2
	sta SCRCONT_REG2
	lda cia2par
	sta CIA2_PA
	lda #0
	sta VIC_ICR	// disable raster interrupt
	lda #$FF	// enable CIA1 interrupts
	sta CIA1_IC
	lda ivl		// restore interrupt vector
	sta INTVECL
	lda ivh
	sta INTVECH
	lda bgr
	sta BG1_REG
	cli
	rts

//--------------------------------------------------

func_setupRasterInt:
	sei
	lda #$7F	// disable and clear CIA1 interrupts
	sta CIA1_IC
	sta CIA1_IS
	lda CIA1_IC
	lda CIA1_IS
	lda #1		// enable raster interrupt
	sta VIC_ICR
	lda #TAILRL	// Use interrupt
	sta RASTERLINE	// at line:TAILRL
	lda SCRCONT_REG
	ora #$80
	sta SCRCONT_REG
	lda #<int_tail	// Setup interrupt vector
	sta INTVECL
	lda #>int_tail
	sta INTVECH
	cli
	rts

//--------------------------------------------------

func_activate:
	lda gmode
	and #6
	bne !setup+
	rts
!setup:	jsr func_setupRasterInt	
	rts

//--------------------------------------------------

func_copyseg:
opc1:	lda #0 sta r0
opc2:	lda #0 sta r0 + 1
	:sto16(r1, COL_SEGMENT)
	ldx #4
!loop1:	ldy #0
!loop2:	lda (r0),y
	sta (r1),y
	iny
	bne !loop2-
	inc r0 + 1
	inc r1 + 1
	dex
	bne !loop1-
	rts

//--------------------------------------------------

func_initMode:
	lda #0		// black border
	sta BORDER_REG
	lda SCRCONT_REG
	sta scr1
	ora #$20	// bitmap mode
	sta SCRCONT_REG
	lda MEMSET_REG
	sta msr
	ora #$08	// use 2nd half of bank for bitmap
	and #$0F	// use 1st colormap of bank
	sta MEMSET_REG
	lda SCRCONT_REG2
	sta scr2
	lda INTVECL
	sta ivl
	lda INTVECH
	sta ivh
	lda gmode	// Skip mcm mode and colseg data
	and #1		// if hires mode enabled
	bne !skip+
	lda SCRCONT_REG2
	eor #$10 	// enable mcm
	sta SCRCONT_REG2
	lda #<colseg1	// copy colseg1
	sta opc1 + 1
	lda #>colseg1
	sta opc2 + 1
	jsr func_copyseg
//	jsr func_copyseg1
!skip:	lda CIA2_PA
	sta cia2par
	and #$FE	// Use bank #1
	ora #$02
	sta CIA2_PA
	rts

//--------------------------------------------------

func_loadImage:
	jsr func_fopenread

	// Load header
	:sto16(address, gmode)
	:sto16(length, 1)
	jsr func_fread
	:sto16(address, width)
	:sto16(length, 1)
	jsr func_fread
	:sto16(address, height)
	:sto16(length, 1)
	jsr func_fread

	// Load global data if lores
	lda BG1_REG
	sta bgr
	lda gmode
	and #1
	bne !skip+
	:sto16(address, BG1_REG)
	:sto16(length, 1)
	jsr func_fread
!skip:
	:sto16(cmptr, COLMAP1)
	:sto16(bmptr, BITMAP1)
	:sto16(csptr, colseg1)
	// Set laced flag
	lda gmode sta laflag

	// Set fli counter
!loop:	lda #0
	sta flicnt
	lda gmode
	and #4
	beq !load+
	lda #7
	sta flicnt

	// Load colmap data
!load:	:mov16(cmptr, address)
	:sto16(length, 1000)
	jsr func_fread
	
	// Load fli colors if fli mode
	lda flicnt
	beq !next+
	dec flicnt
	lda cmptr + 1
	clc adc #4 sta cmptr + 1
	jmp !load-
	
!next:	// Load color segment data if lores
	lda gmode
	and #1
	bne !skip+
	:mov16(csptr, address)
	:sto16(length, 1000)
	jsr func_fread

	// Load bitmap data
!skip:	:mov16(bmptr, address)
	:sto16(length, 8000)
	jsr func_fread

	// Load 2nd image if laced
	lda laflag
	and #2
	beq !over+
	eor #2
	sta laflag
	:sto16(cmptr, COLMAP2)
	:sto16(bmptr, BITMAP2)
	:sto16(csptr, colseg2)
	jmp !loop-

!over:	jsr func_fclose
	rts

//--------------------------------------------------

func_prints:
	lda print_addr
	ldy print_addr + 1
	jsr IO_PRINTSTR
	rts

//--------------------------------------------------

func_inputs:
	:mov16(input_addr, r0)
	ldy #0
!loop:	jsr KERNAL_CHRIN 
	cmp #$0d
	beq !end+
	sta (r0),y
	iny
	bne !loop-
!end:	lda #0
	sta (r0),y
	rts

//--------------------------------------------------

func_setnam:
	// Set filename
	lda filenameLen
	ldx #<filename
	ldy #>filename
	jsr KERNAL_SETNAM
	rts

//--------------------------------------------------

func_fopenread:		// Input: filenameAddr, filenameLen
	jsr func_setnam
	lda #1
	ldx device
	ldy #0		// read
	jsr KERNAL_SETLFS
	jsr KERNAL_OPEN
	ldx #1
	jsr KERNAL_CHKIN
	rts

//--------------------------------------------------

func_fclose:
	lda #1
	jsr KERNAL_CLOSE
	jsr KERNAL_CLRCHN
	rts

//--------------------------------------------------

func_fread:
	:add16(address, length, endaddress)
	:mov16(address, r0)
!loop:	:cmp16(r0, endaddress)
	beq !out+
	jsr KERNAL_CHRIN
	ldy #0
	sta (r0),y
	:inc16(r0)
	jmp !loop-
!out:	rts

//--------------------------------------------------

func_prompt:
	:sto16(print_addr, prompt_text)
	jsr func_prints
	:sto16(input_addr, filename)
	jsr func_inputs
	sty filenameLen
	rts

//--------------------------------------------------
// FUNCTION PARAMETERS
//--------------------------------------------------
filename:	.fill 256, 0
filenameLen:	.byte 0
address:	.word 0
length:		.word 0
device:		.byte 8
print_addr:	.word 0
input_addr:	.word 0

//--------------------------------------------------
// FUNCTION VARIABLES
//--------------------------------------------------
endaddress:	.word 0
prompt_text:	.fill 15, toPet("enter filename:",i) .byte 0
gmode:		.byte 0
width:		.byte 0
height:		.byte 0
bmptr:		.word 0
cmptr:		.word 0
csptr:		.word 0
laflag:		.byte 0
flicnt:		.byte 0
colseg1:	.fill 1000,0
colseg2:	.fill 1000,0
scr1:		.byte 0
scr2:		.byte 0
msr:		.byte 0
cia2par:	.byte 0
ivl:		.byte 0
ivh:		.byte 0
bgr:		.byte 0
/*
.pc = *		"copyseg1"
func_copyseg1:
	.for(var s = 0; s < 1000; s++)
	{
		lda colseg1 + s
		sta COL_SEGMENT + s
	}
	rts

.pc = $4400	"copyseg2"
func_copyseg2:
	.for(var s = 0; s < 1000; s++)
	{
		lda colseg2 + s
		sta COL_SEGMENT + s
	}
	rts
*/