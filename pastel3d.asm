//--------------------------------------------------
// PASTEL 3D FOR COMMODORE 64
// By FST 2015(c)
//--------------------------------------------------

// Includes
.import source "constants.inc"
.import source "registers.inc"
.import source "macros.inc"

//--------------------------------------------------
// GFX
//--------------------------------------------------
.pc = COLMAP		"Color map"
.fill 1000, $0B
.pc = BITMAP		"Bitmap"
.fill 8192, $AA

//--------------------------------------------------
// CODE
//--------------------------------------------------

// Basic Upstart Module
.pc = $0801	"Basic Upstart"
:BasicUpstart(MAIN)

// Main
.pc = CODE_SEGMENT	"Main"

MAIN:
	jsr func_initIO
	jsr func_startRender
	rts

// Import libs
.import source "math.asm"
.import source "math_tools.asm"
.import source "gpio.asm"
.import source "file.asm"
.import source "engine.asm"
