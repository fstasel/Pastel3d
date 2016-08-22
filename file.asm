//--------------------------------------------------
// KERNAL CONSTANTS
//--------------------------------------------------

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

//--------------------------------------------------
// FILE I/O FUNCTIONS
//--------------------------------------------------

.pc = *		"File I/O module"

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

func_fopenwrite:	// Input: filenameAddr, filenameLen
	jsr func_setnam
	lda #1
	ldx device
	ldy #1		// write
	jsr KERNAL_SETLFS
	jsr KERNAL_OPEN
	ldx #1
	jsr KERNAL_CHKOUT
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

func_fwrite:
	:add16(address, length, endaddress)
	:mov16(address, r0)
!loop:	:cmp16(r0, endaddress)
	beq !out+
	ldy #0
	lda (r0),y
	jsr KERNAL_CHROUT
	:inc16(r0)
	jmp !loop-
!out:	rts

//--------------------------------------------------
// FUNCTION PARAMETERS
//--------------------------------------------------
filename:	.fill 5, toPet("test4",i)
filenameLen:	.byte 5
address:	.word 0
length:		.word 0
device:		.byte 8

//--------------------------------------------------
// FUNCTION VARIABLES
//--------------------------------------------------
endaddress:	.word 0
