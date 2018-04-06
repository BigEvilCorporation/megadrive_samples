;==============================================================
; SEGA MEGA DRIVE/GENESIS - DEMO 3 - SPRITES SAMPLE
;==============================================================
; by Big Evil Corporation
;==============================================================

; A small, discreet, and complete sprites sample, with a healthy
; dose of comments and explanations for beginners.
; Runs on genuine hardware, and (hopefully) all emulators.
;
; I recommend reading and understanding the Scroll Planes
; sample first.
;
; To assemble this program with ASM68K.EXE:
;    ASM68K.EXE /p sprites.asm,sprites.bin,sprites.map,sprites.lst
;
; To assemble this program with SNASM68K.EXE:
;    SNASM68K.EXE /p sprites.asm,sprites.map,sprites.lst,sprites.bin
;
; sprites.asm = this source file
; sprites.bin = the binary file, fire this up in your emulator!
; sprites.lst = listing file, shows assembled addresses alongside
;               your source code, open in a text editor
; sprites.map = symbol map file for linking (unused)

;==============================================================

; Start of ROM
ROM_Start:

;==============================================================
; CPU VECTOR TABLE
;==============================================================
	dc.l   0x00FFE000			; Initial stack pointer value
	dc.l   CPU_EntryPoint		; Start of program
	dc.l   CPU_Exception 		; Bus error
	dc.l   CPU_Exception 		; Address error
	dc.l   CPU_Exception 		; Illegal instruction
	dc.l   CPU_Exception 		; Division by zero
	dc.l   CPU_Exception 		; CHK CPU_Exception
	dc.l   CPU_Exception 		; TRAPV CPU_Exception
	dc.l   CPU_Exception 		; Privilege violation
	dc.l   INT_Null				; TRACE exception
	dc.l   INT_Null				; Line-A emulator
	dc.l   INT_Null				; Line-F emulator
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Spurious exception
	dc.l   INT_Null				; IRQ level 1
	dc.l   INT_Null				; IRQ level 2
	dc.l   INT_Null				; IRQ level 3
	dc.l   INT_HInterrupt		; IRQ level 4 (horizontal retrace interrupt)
	dc.l   INT_Null  			; IRQ level 5
	dc.l   INT_VInterrupt		; IRQ level 6 (vertical retrace interrupt)
	dc.l   INT_Null				; IRQ level 7
	dc.l   INT_Null				; TRAP #00 exception
	dc.l   INT_Null				; TRAP #01 exception
	dc.l   INT_Null				; TRAP #02 exception
	dc.l   INT_Null				; TRAP #03 exception
	dc.l   INT_Null				; TRAP #04 exception
	dc.l   INT_Null				; TRAP #05 exception
	dc.l   INT_Null				; TRAP #06 exception
	dc.l   INT_Null				; TRAP #07 exception
	dc.l   INT_Null				; TRAP #08 exception
	dc.l   INT_Null				; TRAP #09 exception
	dc.l   INT_Null				; TRAP #10 exception
	dc.l   INT_Null				; TRAP #11 exception
	dc.l   INT_Null				; TRAP #12 exception
	dc.l   INT_Null				; TRAP #13 exception
	dc.l   INT_Null				; TRAP #14 exception
	dc.l   INT_Null				; TRAP #15 exception
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	
;==============================================================
; SEGA MEGA DRIVE ROM HEADER
;==============================================================
	dc.b "SEGA MEGA DRIVE "                                 ; Console name
	dc.b "BIGEVILCORP.    "                                 ; Copyright holder and release date
	dc.b "HELLO WORLD                                     " ; Domestic name
	dc.b "HELLO WORLD                                     " ; International name
	dc.b "GM XXXXXXXX-XX"                                   ; Version number
	dc.w 0x0000                                             ; Checksum
	dc.b "J               "                                 ; I/O support
	dc.l ROM_Start                                          ; Start address of ROM
	dc.l ROM_End-1                                          ; End address of ROM
	dc.l 0x00FF0000                                         ; Start address of RAM
	dc.l 0x00FF0000+0x0000FFFF                              ; End address of RAM
	dc.l 0x00000000                                         ; SRAM enabled
	dc.l 0x00000000                                         ; Unused
	dc.l 0x00000000                                         ; Start address of SRAM
	dc.l 0x00000000                                         ; End address of SRAM
	dc.l 0x00000000                                         ; Unused
	dc.l 0x00000000                                         ; Unused
	dc.b "                                        "         ; Notes (unused)
	dc.b "  E             "                                 ; Country codes
	
;==============================================================
; INITIAL VDP REGISTER VALUES
;==============================================================
; In this demo, we're particularly interested in register 0x5,
; which specifies the address of the Sprite Attribute Table
; (SAT) within VRAM. Here it's set to 0xF000.
;==============================================================
VDPRegisters:
	dc.b 0x14 ; 0x00: H interrupt on, palettes on
	dc.b 0x74 ; 0x01: V interrupt on, display on, DMA on, Genesis mode on
	dc.b 0x30 ; 0x02: Pattern table for Scroll Plane A at VRAM 0xC000 (bits 3-5 = bits 13-15)
	dc.b 0x00 ; 0x03: Pattern table for Window Plane at VRAM 0x0000 (disabled) (bits 1-5 = bits 11-15)
	dc.b 0x07 ; 0x04: Pattern table for Scroll Plane B at VRAM 0xE000 (bits 0-2 = bits 11-15)
	dc.b 0x78 ; 0x05: Sprite Attribute Table at VRAM 0xF000 (bits 0-6 = bits 9-15)
	dc.b 0x00 ; 0x06: Unused
	dc.b 0x00 ; 0x07: Background colour: bits 0-3 = colour, bits 4-5 = palette
	dc.b 0x00 ; 0x08: Unused
	dc.b 0x00 ; 0x09: Unused
	dc.b 0x08 ; 0x0A: Frequency of Horiz. interrupt in Rasters (number of lines travelled by the beam)
	dc.b 0x00 ; 0x0B: External interrupts off, V scroll per-page, H scroll per-page
	dc.b 0x81 ; 0x0C: Shadows and highlights off, interlace off, H40 mode (320 x 224 screen res)
	dc.b 0x3F ; 0x0D: Horiz. scroll table at VRAM 0xFC00 (bits 0-5)
	dc.b 0x00 ; 0x0E: Unused
	dc.b 0x02 ; 0x0F: Autoincrement 2 bytes
	dc.b 0x01 ; 0x10: Scroll plane size: 64x32 tiles
	dc.b 0x00 ; 0x11: Window Plane X pos 0 left (pos in bits 0-4, left/right in bit 7)
	dc.b 0x00 ; 0x12: Window Plane Y pos 0 up (pos in bits 0-4, up/down in bit 7)
	dc.b 0xFF ; 0x13: DMA length lo byte
	dc.b 0xFF ; 0x14: DMA length hi byte
	dc.b 0x00 ; 0x15: DMA source address lo byte
	dc.b 0x00 ; 0x16: DMA source address mid byte
	dc.b 0x80 ; 0x17: DMA source address hi byte, memory-to-VRAM mode (bits 6-7)
	
	even
	
;==============================================================
; CONSTANTS
;==============================================================
	
; VDP port addresses
vdp_control				equ 0x00C00004
vdp_data				equ 0x00C00000

; VDP commands
vdp_cmd_vram_write		equ 0x40000000
vdp_cmd_cram_write		equ 0xC0000000
vdp_cmd_vsram_write		equ 0x40000010

; VDP memory addresses
; according to VDP registers 0x2, 0x4, 0x5, and 0xD (see table above)
vram_addr_tiles			equ 0x0000
vram_addr_plane_a		equ 0xC000
vram_addr_plane_b		equ 0xE000
vram_addr_sprite_table	equ 0xF000	; NEW in this demo - the Sprite Attribute Table (SAT)
vram_addr_hscroll		equ 0xFC00

; Screen width and height (in pixels)
vdp_screen_width		equ 0x0140
vdp_screen_height		equ 0x00F0

; The plane width and height (in tiles)
; according to VDP register 0x10 (see table above)
vdp_plane_width			equ 0x40
vdp_plane_height		equ 0x20

; The size of the sprite plane (512x512 pixels)
;
; With only a 320x240 display size, a lot of this
; is off screen, which is useful for hiding sprites
; when not needed (saves needing to adjust the linked
; list in the attribute table).
vdp_sprite_plane_width	equ 0x0200
vdp_sprite_plane_height	equ 0x0200

; The sprite border (invisible area left + top) size
;
; The sprite plane is 512x512 pixels, but is offset by
; -128 pixels in both X and Y directions. To see a sprite
; on screen at 0,0 we need to offset its position by
; this border.
vdp_sprite_border_x		equ 0x80
vdp_sprite_border_y		equ 0x80

; Hardware version address
hardware_ver_address	equ 0x00A10001

; TMSS
tmss_address			equ 0x00A14000
tmss_signature			equ 'SEGA'

; The size of a word and longword
size_word				equ 2
size_long				equ 4

; The size of one palette (in bytes, words, and longwords)
size_palette_b			equ 0x20
size_palette_w			equ size_palette_b/size_word
size_palette_l			equ size_palette_b/size_long

; The size of one graphics tile (in bytes, words, and longwords)
size_tile_b				equ 0x20
size_tile_w				equ size_tile_b/size_word
size_tile_l				equ size_tile_b/size_long

; Sprite initial draw positions (in pixels)
sprite_1_start_pos_x	equ vdp_sprite_border_x
sprite_1_start_pos_y	equ vdp_sprite_border_y
sprite_2_start_pos_x	equ vdp_sprite_border_x+0x0040
sprite_2_start_pos_y	equ vdp_sprite_border_y+0x0020

; Speed (in pixels per frame) to move our sprites
sprite_1_move_speed_x	equ 0x1
sprite_1_move_speed_y	equ 0x1
sprite_2_move_speed_x	equ 0x2
sprite_2_move_speed_y	equ 0x0

;==============================================================
; VRAM WRITE MACROS
;==============================================================
	
; Set the VRAM (video RAM) address to write to next
SetVRAMWrite: macro addr
	move.l  #(vdp_cmd_vram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm
	
; Set the CRAM (colour RAM) address to write to next
SetCRAMWrite: macro addr
	move.l  #(vdp_cmd_cram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm

; Set the VSRAM (vertical scroll RAM) address to write to next
SetVSRAMWrite: macro addr
	move.l  #(vdp_cmd_vsram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm

;==============================================================
; SPRITE ATTRIBUTE MACRO
;==============================================================
; A macro to help build an entry in the Sprite Attribute
; Table, since manipulating structures and bit twiddling isn't
; the focus of this demo, and would make the code harder to
; read.
;==============================================================
; Proper game implementations would make use of a local SAT
; table in RAM and use DMA to transfer the table to VRAM each
; frame (which also allows us to use RAM like a "stream" to write
; this data more efficiently) but this is the best method for
; teaching the basics first.
;==============================================================
; Each sprite attribute entry is in the following format:
;
;   Y coordinate      1 word - the Y coordinate on the sprite plane
;   Dimensions bits   1 byte - bits describing the layout (1x1 tiles up to 4x4 tiles)
;   Linked list next  1 byte - the index of the next sprite to draw, or 0 if end of list
;   Prio/palette/flip 1 byte - the priority (bit 15), palette (bits 14-13),
;                              v/h flip (bits 12 and 11), and top 3 bits of the tile ID
;   Tile ID bottom    1 byte - the bottom 8 bits of the tile ID
;   X coordinate      1 word - the X coordinate on the sprite plane
;==============================================================

; Writes a sprite attribute structure to 4 registers, ready to write to VRAM
BuildSpriteStructure: macro x_pos,	; X pos on sprite plane
	y_pos,							; Y pos on sprite plane
	dimension_bits,					; Sprite tile dimensions (4 bits)
	next_id,						; Next sprite index in linked list
	priority_bit,					; Draw priority
	palette_id,						; Palette index
	flip_x,							; Flip horizontally
	flip_y,							; Flip vertically
	tile_id,						; First tile index
	reg1,							; Output: reg1
	reg2,							; Output: reg2
	reg3,							; Output: reg3
	reg4							; Output: reg4

	move.w #y_pos, \reg1
	move.w #(\dimension_bits<<8|\next_id), \reg2
	move.w #(\priority_bit<<14|\palette_id<<13|\flip_x<<11|\flip_y<<10|\tile_id), \reg3
	move.w #x_pos, \reg4
	endm

;==============================================================
; MEMORY MAP
;==============================================================
; We need to store the current sprite positions in RAM and update
; them each frame. There are a few ways to create a memory map,
; but the cleanest, simplest, and easiest to maintain method
; uses the assembler's "RS" keywords. RSSET begins a new table of
; offsets starting from any other offset (here we're starting at
; 0x00FF0000, the start of RAM), and allows us to add named entries
; of any size for the "variables". We can then read/write these
; variables using the offsets' labels (see INT_VInterrupt for use
; cases).
;==============================================================
	RSSET 0x00FF0000			; Start a new offset table from beginning of RAM
ram_sprite_1_pos_x		rs.w 1	; 1 table entry of word size for sprite 1's X pos
ram_sprite_1_pos_y		rs.w 1	; 1 table entry of word size for sprite 1's Y pos
ram_sprite_2_pos_x		rs.w 1	; 1 table entry of word size for sprite 2's X pos
ram_sprite_2_pos_y		rs.w 1	; 1 table entry of word size for sprite 2's Y pos

; !! Be careful when adding any table entries of BYTE size, since
; you'll need to start worrying about alignment. More of this in a
; future demo.

;==============================================================
; PALETTE
;==============================================================
; In this demo we'll be using one palette per sprite,
; so we've added a palette count to upload the correct number
; of entries.
;==============================================================
Palettes:

; Palette for sprite 1
Palette1:
	dc.w 0x0000
	dc.w 0x0020
	dc.w 0x0EEE
	dc.w 0x00AC
	dc.w 0x02EA
	dc.w 0x00EE
	dc.w 0x0008
	dc.w 0x000C
	dc.w 0x000A
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000

; Palette for sprite 2
Palette2:
	dc.w 0x0000
	dc.w 0x0004
	dc.w 0x0226
	dc.w 0x0040
	dc.w 0x0446
	dc.w 0x0262
	dc.w 0x0662
	dc.w 0x004A
	dc.w 0x0468
	dc.w 0x0882
	dc.w 0x006C
	dc.w 0x0202
	dc.w 0x04A0
	dc.w 0x0AC2
	dc.w 0x06AE
	dc.w 0x02EC

; Number of palettes to write to CRAM
palette_count	equ 0x2

;==============================================================
; TILE IDs
;==============================================================
; The indices of the first tile in each sprite. We only need
; to tell the sprite table where to find the starting tile of
; each sprite, so we don't bother keeping track of every tile
; index.
;
; Note we still leave the first tile blank (planes A and B are
; filled with tile 0) so we'll be uploading our sprite tiles
; from index 1.
;
; See bottom of the file for the sprite tiles themselves.
;==============================================================
tile_id_blank		equ 0x00	; The blank tile at index 0
tile_id_sprite_1	equ 0x01	; Sprite 1 index (4 tiles)
tile_id_sprite_2	equ 0x05	; Sprite 2 index (12 tiles)

; Total number of tiles in the sprites to upload to VRAM
tile_count			equ 0x11	; Total tiles = 16

;==============================================================
; CODE ENTRY POINT
;==============================================================
CPU_EntryPoint:

	;==============================================================
	; Initialise the Mega Drive
	;==============================================================

	; Write the TMSS signature (if a model 1+ Mega Drive)
	jsr    VDP_WriteTMSS

	; Load the initial VDP registers
	jsr    VDP_LoadRegisters

	;==============================================================
	; Clear VRAM (video memory)
	;==============================================================

	; Setup the VDP to write to VRAM address 0x0000 (start of VRAM)
	SetVRAMWrite 0x0000

	; Write 0's across all of VRAM
	move.w #(0x00010000/size_word)-1, d0	; Loop counter = 64kb, in words (-1 for DBRA loop)
	@ClrVramLp:								; Start of loop
	move.w #0x0, vdp_data					; Write a 0x0000 (word size) to VRAM
	dbra   d0, @ClrVramLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Write the palettes to CRAM (colour memory)
	;==============================================================
	
	; Setup the VDP to write to CRAM address 0x0000 (first palette)
	SetCRAMWrite 0x0000
	
	; Write the palettes to CRAM
	;
	; This time we're writing multiple palettes, so multiply the word count
	; by the palette count (and don't forget the -1 for the loop counter).
	lea    Palettes, a0				; Move palette address to a0
	move.w #(palette_count*size_palette_w)-1, d0	; Loop counter = 8 words in palette (-1 for DBRA loop)
	@PalLp:							; Start of loop
	move.w (a0)+, vdp_data			; Write palette entry, post-increment address
	dbra d0, @PalLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Write the sprite tiles to VRAM
	;==============================================================
	
	; Setup the VDP to write to VRAM address 0x0020 (the address of the first sprite tile, index 1)
	;
	; We need to leave the first tile blank (we cleared VRAM, so it should be all 0's) for
	; planes A and B to display, so skip the first tile (offset address by size_tile_b).
	SetVRAMWrite vram_addr_tiles+size_tile_b
	
	; Write the sprite tiles to VRAM
	lea    sprite_tiles, a0						; Move the address of the first graphics tile into a0
	move.w #(tile_count*size_tile_l)-1, d0		; Loop counter = 8 longwords per tile * num tiles (-1 for DBRA loop)
	@CharLp:									; Start of loop
	move.l (a0)+, vdp_data						; Write tile line (4 bytes per line), and post-increment address
	dbra d0, @CharLp							; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Set up the Sprite Attribute Table (SAT)
	;==============================================================

	; The Sprite Attribute Table is a table of sprites to draw.
	; Each entry in the table describes the first tile ID, the number
	; of tiles to draw (and their layout), the X and Y position
	; (on the 512x512 sprite plane), the palette to draw with, a
	; priority flag, and X/Y flipping flags.
	;
	; Sprites can be layed out in these tile dimensions:
	;
	; 1x1 (1 tile)  - 0000
	; 1x2 (2 tiles) - 0001
	; 1x3 (3 tiles) - 0010
	; 1x4 (4 tiles) - 0011
	; 2x1 (2 tiles) - 0100
	; 2x2 (4 tiles) - 0101
	; 2x3 (6 tiles) - 0110
	; 2x4 (8 tiles) - 0111
	; 3x1 (3 tiles) - 1000
	; 3x2 (6 tiles) - 1001
	; 3x3 (9 tiles) - 1010
	; 3x4 (12 tiles)- 1011
	; 4x1 (4 tiles) - 1100
	; 4x2 (8 tiles) - 1101
	; 4x3 (12 tiles)- 1110
	; 4x4 (16 tiles)- 1111
	;
	; The tiles are layed out in COLUMN MAJOR, rather than planes A and B
	; which are row major. Tiles within a sprite cannot be reused (since it
	; only accepts a starting tile and a count/layout) so the whole sprite
	; needs uploading to VRAM in one consecutive chunk, even if some tiles
	; are duplicates.
	;
	; The X/Y flipping flags take the layout into account, you don't need
	; to re-adjust the layout, position, or tile IDs to flip the entire
	; sprite as a whole.
	;
	; There are 64 entries in the table, but the number of them drawn,
	; and the order in which they're processed, is determined by a linked
	; list. Each sprite entry has an index to the NEXT sprite to be drawn.
	; If this index is 0, the list ends, and the VDP won't draw any more
	; sprites this frame.

	; Start writing to the sprite attribute table in VRAM
	SetVRAMWrite vram_addr_sprite_table

	;==============================================================
	; Set up sprite 1

	; Write all values into registers first to make it easier. We
	; write to VRAM one word at a time (auto-increment is set to 2
	; in VDP register 0xF), so we'll assign each word to a register.
	;
	; Since bit twiddling and manipulating structures isn't the focus of
	; this sample, we have a macro to simplify this part.

	; Position:   sprite_1_start_pos_x,sprite_1_start_pos_y
	; Dimensions: 2x2 tiles (8 tiles total) = 0101 in binary (see table above)
	; Next link:  sprite index 1 is next to be processed
	; Priority:   0
	; Palette id: 0
	; Flip X:     0
	; Flip Y:     0
	; Tile id:    tile_id_sprite_1
	BuildSpriteStructure sprite_1_start_pos_x,sprite_1_start_pos_y,%0101,0x1,0x0,0x0,0x0,0x0,tile_id_sprite_1,d0,d1,d2,d3

	; Write the entire sprite attribute structure to the sprite table
	move.w d0, vdp_data
	move.w d1, vdp_data
	move.w d2, vdp_data
	move.w d3, vdp_data

	;==============================================================
	; Set up sprite 2

	; Position:   sprite_2_start_pos_x,sprite_2_start_pos_y
	; Dimensions: 4x3 tiles (16 tiles total) = 1110 in binary (see table above)
	; Next link:  sprite index 0 (end of linked list)
	; Priority:   0
	; Palette id: 1
	; Flip X:     0
	; Flip Y:     0
	; Tile id:    tile_id_sprite_2
	BuildSpriteStructure sprite_2_start_pos_x,sprite_2_start_pos_y,%1110,0x0,0x0,0x1,0x0,0x0,tile_id_sprite_2,d0,d1,d2,d3

	; Write the entire sprite attribute structure to the sprite table
	move.w d0, vdp_data
	move.w d1, vdp_data
	move.w d2, vdp_data
	move.w d3, vdp_data

	;==============================================================
	; Intitialise variables in RAM
	;==============================================================
	move.w #sprite_1_start_pos_x, ram_sprite_1_pos_x
	move.w #sprite_1_start_pos_y, ram_sprite_1_pos_y
	move.w #sprite_2_start_pos_x, ram_sprite_2_pos_x
	move.w #sprite_2_start_pos_y, ram_sprite_2_pos_y

	;==============================================================
	; Initialise status register and set interrupt level.
	; This begins firing vertical and horizontal interrupts.
	;
	; Since the vinterrupt does something meaningful in this
	; demo, we start this AFTER setting up the VDP to draw and
	; intialising the variables in RAM.
	;==============================================================
	move.w #0x2300, sr

	; Finished!
	
	;==============================================================
	; Loop forever
	;==============================================================
	; This loops forever, effectively ending our main routine,
	; but the VDP will continue to run of its own accord and
	; will still fire vertical and horizontal interrupts (which is
	; where our update code is), so the demo continues to run.
	;
	; For a game, it would be better to use this loop for processing
	; input and game code, and wait here until next vblank before
	; looping again. We only use vinterrupt for updates in this demo
	; for simplicity (because we don't yet have any timing code).
	@InfiniteLp:
	bra @InfiniteLp
	
;==============================================================
; INTERRUPT ROUTINES
;==============================================================

; Vertical interrupt - run once per frame (50hz in PAL, 60hz in NTSC)
INT_VInterrupt:

	; Fetch current sprite coordinates from RAM
	move.w ram_sprite_1_pos_x, d0
	move.w ram_sprite_1_pos_y, d1
	move.w ram_sprite_2_pos_x, d2
	move.w ram_sprite_2_pos_y, d3

	; Animate them (x/y coords are 9 bits, so this
	; wraps around the whole 512x512 sprite plane)
	addi.w #sprite_1_move_speed_x, d0
	addi.w #sprite_1_move_speed_y, d1
	addi.w #sprite_2_move_speed_x, d2
	addi.w #sprite_2_move_speed_y, d3

	; Store updated values back in RAM for next frame
	move.w d0, ram_sprite_1_pos_x
	move.w d1, ram_sprite_1_pos_y
	move.w d2, ram_sprite_2_pos_x
	move.w d3, ram_sprite_2_pos_y

	; Write updated coordinates to the Sprite Attribute Table in VRAM.
	; Each entry is 8 bytes in size, so sprite 1 is at table+0x0000,
	; and sprite 2 is at table+0x0008.
	;
	; The Y coord is the 1st word in the structure, and the X coord is
	; the 4th. As already noted, there are cleaner ways to do this,
	; like storing the tables in RAM and copying them via DMA every
	; frame, but that's beyond the focus of this sample.

	; Sprite 1's Y coordinate is at table+0x0000
	SetVRAMWrite vram_addr_sprite_table+0x0000
	move.w d1, vdp_data

	; Sprite 1's X coordinate is at table+0x0006
	SetVRAMWrite vram_addr_sprite_table+0x0006
	move.w d0, vdp_data

	; Sprite 2's Y coordinate is at table+0x0008
	SetVRAMWrite vram_addr_sprite_table+0x0008
	move.w d3, vdp_data

	; Sprite 2's X coordinate is at table+0x000E
	SetVRAMWrite vram_addr_sprite_table+0x000E
	move.w d2, vdp_data

	rte

; Horizontal interrupt - run once per N scanlines (N = specified in VDP register 0xA)
INT_HInterrupt:
	; Doesn't do anything in this demo
	rte

; NULL interrupt - for interrupts we don't care about
INT_Null:
	rte

; Exception interrupt - called if an error has occured
CPU_Exception:
	; Just halt the CPU if an error occurred
	stop   #0x2700
	rte
	
;==============================================================
; UTILITY FUNCTIONS
;==============================================================

VDP_WriteTMSS:

	; Poke the TMSS to show "LICENSED BY SEGA..." message and allow us to
	; access the VDP (or it will lock up on first access).
	move.b hardware_ver_address, d0			; Move Megadrive hardware version to d0
	andi.b #0x0F, d0						; The version is stored in last four bits, so mask it with 0F
	beq    @SkipTMSS						; If version is equal to 0, skip TMSS signature
	move.l #tmss_signature, tmss_address	; Move the string "SEGA" to 0xA14000
	@SkipTMSS:

	; Check VDP
	move.w vdp_control, d0					; Read VDP status register (hangs if no access)
	
	rts

VDP_LoadRegisters:

	; Set VDP registers
	lea    VDPRegisters, a0		; Load address of register table into a0
	move.w #0x18-1, d0			; 24 registers to write (-1 for loop counter)
	move.w #0x8000, d1			; 'Set register 0' command to d1

	@CopyRegLp:
	move.b (a0)+, d1			; Move register value from table to lower byte of d1 (and post-increment the table address for next time)
	move.w d1, vdp_control		; Write command and value to VDP control port
	addi.w #0x0100, d1			; Increment register #
	dbra   d0, @CopyRegLp		; Decrement d0, and jump back to top of loop if d0 is still >= 0
	
	rts

;==============================================================
; SPRITE TILES
;==============================================================
; The sprite graphics tiles. Too big to paste in here, so we'll
; include them from external files at the bottom of the ROM.
;
; If your tile data is in binary format rather than text, use
; INCBIN instead of INCLUDE.
;==============================================================

sprite_tiles:
	; Sprite 1 - a red Fuzzl, 2x2 tiles
	include "sprite1.asm"

	; Sprite 2 - a bouncy mushroom, 4x3 tiles
	include "sprite2.asm"

; The end of ROM
ROM_End:
