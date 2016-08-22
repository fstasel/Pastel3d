//--------------------------------------------------
// SETTINGS
//--------------------------------------------------
//
// .byte graphicMode 		// Bits: FLR
				// (R)esolution: 0 = lores(MCM), 1 = hires
				// (L)aced: 0 = normal, 1 = laced
				// (F)li: 0 = disabled, 1 = enabled
// .word textWidth, textHeight	// 40,25 (text mode) -> 320,200 (graphic mode)
// .word scaleFactor		// Viewport scaler
// .byte textModeEnabled	// 0:disabled (graphic mode), 1:enabled
// .byte NumObjects
//
// OBJECTS:
// .byte Type .word Param 1,2,3 .byte Param 4,5,6 .word Param 7 .byte Param 8
//
// CAM: 0, posX, posY, posZ, rotX, rotY, rotZ, focal, 0
// PLANE: 1, posX, posY, posZ, rotX, rotY, rotZ, scale, color
// SPHERE: 2, posX, posY, posZ, 0, 0, 0, radius, color
//
// LIGHTSOURCE:
// .word posX, posY, posZ .byte ambience
//--------------------------------------------------
// Axes:           
//         +y
//
//          A    _ +z
//          |    /|
//          |   /
//          |  /
//          | /
//          |/
//        O --------> +x
//
//--------------------------------------------------
// Color byte: %HHHSSLLL
// 8-bit HSL color input
//--------------------------------------------------

.byte %011
.word 40, 25
.word 1
.byte 0
.byte 2
// CAMERA
.byte 0	.word 0, 0, 0		.byte 40, 0, 0		.word 200	.byte 0
// OBJECTS
.byte 1	.word 0, 0, 500		.byte 128, 0, 0	.word 150	.byte %00000111
.byte 2 .word 0, 600, 300	.byte 0, 0, 0	.word 200	.byte %01011111
// LIGHT SOURCE
.word 100, 300, -100 .byte 55
