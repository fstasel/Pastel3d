// Pastel 3D Rendering Engine palette data
// Prepared by FST

// 8-bit HSL to C64 color palette conversion table
coltable:
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$b8,$88,$8f,$ff,$f1
.byte $00,$0b,$bb,$b9,$99,$98,$88,$81
.byte $00,$02,$22,$2a,$aa,$af,$ff,$f1
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0c,$cc,$c8,$88,$87,$77,$71
.byte $00,$0c,$cc,$c2,$22,$72,$77,$71
.byte $00,$09,$99,$98,$88,$78,$77,$71
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$b7,$77,$7f,$ff,$f1
.byte $00,$0b,$bb,$b5,$55,$57,$77,$71
.byte $00,$05,$55,$5d,$dd,$d7,$77,$71
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$bc,$cc,$c7,$77,$71
.byte $00,$0b,$bb,$b7,$77,$73,$33,$31
.byte $00,$05,$55,$5d,$dd,$d3,$33,$31
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$be,$ee,$ef,$ff,$f1
.byte $00,$0b,$bb,$be,$ee,$e3,$33,$31
.byte $00,$05,$55,$5e,$ee,$e3,$33,$31
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$b6,$66,$6f,$ff,$f1
.byte $00,$0b,$bb,$b6,$66,$63,$33,$31
.byte $00,$06,$66,$6e,$ee,$e3,$33,$31
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$04,$44,$4c,$cc,$cf,$ff,$f1
.byte $00,$04,$44,$46,$66,$6f,$ff,$f1
.byte $00,$04,$44,$46,$66,$6e,$ee,$e1
.byte $00,$0b,$bb,$bc,$cc,$fc,$ff,$f1
.byte $00,$0b,$bb,$b4,$44,$4f,$ff,$f1
.byte $00,$02,$22,$24,$44,$4f,$ff,$f1
.byte $00,$02,$22,$24,$44,$4a,$aa,$a1

// Distance table
disttable:
.byte 0,9,11,2,6,5,12,14,4,8,13,7,10,15,3,1
.byte 1,3,10,15,13,7,8,4,14,12,5,6,2,9,11,0
.byte 2,9,0,8,6,5,4,14,13,7,10,3,1,11,12,15
.byte 3,1,13,7,14,8,4,5,10,6,2,9,0,11,12,15
.byte 4,8,7,6,3,2,9,10,13,14,5,1,0,11,12,15
.byte 5,14,6,9,13,7,8,0,2,10,4,3,1,11,12,15
.byte 6,14,5,0,2,10,8,13,4,9,7,3,1,11,12,15
.byte 7,8,10,13,1,3,5,4,14,2,9,6,0,11,12,15
.byte 8,7,4,2,10,13,5,9,1,6,3,14,0,11,12,15
.byte 9,2,0,5,8,4,6,14,13,7,10,3,1,11,12,15
.byte 10,7,13,1,8,6,5,4,14,2,3,9,0,11,12,15
.byte 11,0,12,15,4,8,7,1,2,9,10,5,6,13,14,3
.byte 12,11,15,0,1,4,8,7,2,9,10,5,6,13,14,3
.byte 13,3,7,10,1,5,14,8,6,4,2,9,0,11,12,15
.byte 14,6,5,13,3,7,10,4,2,9,8,1,0,11,12,15
.byte 15,12,1,11,4,8,0,7,2,9,10,5,6,13,14,3