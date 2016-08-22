//--------------------------------------------------
// GENERAL CONSTANTS
//--------------------------------------------------

.const CODE_SEGMENT = $4000	// Code segment
.const SCRCONT_REG = $D011	// Screen control register #1
.const SCRCONT_EOR = $20	// Switch txt mode <-> gfx mode
.const SCRCONT_REG2 = $D016	// Screen control register #2
.const SCRCONT_EOR2 = $10	// Switch lores_mcm <-> hires
.const MEMSET_REG = $D018	// Memory setup register
.const MEMSET_EOR = $28		// Switch bitmap to $2000, Colmap to $0C00
.const BITMAP = $2000		// Bitmap address
.const COLMAP = $0C00		// Colormap address
.const IO_PRINTSTR = $AB1E	// BASIC ROM routine for print
.const IO_STRCOLOR = $0286	// BASIC/print string color address
.const COL_SEGMENT = $D800	// Color table
.const CHR_SEGMENT = $0400	// Char. table
.const BORDER_REG = $D020	// Border color register
.const BG1_REG = $D021		// BG#1 color register

//--------------------------------------------------
// SETTINGS CONSTANTS
//--------------------------------------------------

.const MAX_OBJECTS = 16		// Max objects
.const OBJATTRIB_LEN = 13	// Len of each obj
.const GMODE = settings + 0	// Shift for settings
.const TEXTWIDTH = settings + 1
.const TEXTHEIGHT = settings + 3
.const SCALEF = settings + 5
.const TMODEENABLED = settings + 7
.const NUMOBJECTS = settings + 8
.const OBJTYPE = settings + 9
.const OBJPOSX = settings + 10
.const OBJPOSY = settings + 12
.const OBJPOSZ = settings + 14
.const OBJROTX = settings + 16
.const OBJROTY = settings + 17
.const OBJROTZ = settings + 18
.const OBJSCALE = settings + 19
.const OBJCOLOR = settings + 21
