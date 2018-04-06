;==============================================================
; SEGA MEGA DRIVE/GENESIS - DEMO 1 - HELLO WORLD SAMPLE
;==============================================================
; by Big Evil Corporation
;==============================================================

; A small, discreet, and complete Hello World sample, with
; a healthy dose of comments and explanations for beginners.
; Runs on genuine hardware, and (hopefully) all emulators.
;
; To assemble this program with ASM68K.EXE:
;    ASM68K.EXE /p hello.asm,hello.bin,hello.map,hello.lst
;
; To assemble this program with SNASM68K.EXE:
;    SNASM68K.EXE /p hello.asm,hello.map,hello.lst,hello.bin
;
; hello.asm = this source file
; hello.bin = the binary file, fire this up in your emulator!
; hello.lst = listing file, shows assembled addresses alongside your source code, open in a text editor
; hello.map = symbol map file for linking (unused)

;==============================================================

; A label defining the start of ROM so we can compute the total size.
ROM_Start:

;==============================================================
; CPU VECTOR TABLE
;==============================================================
; A table of addresses that the CPU needs to know about -
; things like stack address, "main()" function address,
; vertical/horizontal interrupt addresses, etc.
;==============================================================
; For any interrupts we don't want to handle in this demo,
; we specify INT_Null (an interrupt at the bottom of the
; file that doesn't do anything).
;==============================================================
; This must be the very first thing in the ROM, since the CPU
; reads it from 0x0000 on bootup.
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
; A structure that specifies some metadata about the ROM, like
; its name, author, version number, release date, region,
; and any special peripherals used.
; Note that the Mega Drive console itself doesn't read any of
; this, it's more a convenience for the programmer, but
; most emulators will read the title and region.
;==============================================================
; If your emulator doesn't show the correct ROM name, then this
; table is in the wrong place or in the wrong format.
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
; 24 register values to be copied to the VDP during initialisation.
; These specify things like initial width/height of the planes,
; addresses within VRAM to find scroll/sprite data, the
; background palette/colour index, whether or not the display
; is on, and clears initial values for things like DMA.
;==============================================================
VDPRegisters:
	dc.b 0x14 ; 0x00:  H interrupt on, palettes on
	dc.b 0x74 ; 0x01:  V interrupt on, display on, DMA on, Genesis mode on
	dc.b 0x30 ; 0x02:  Pattern table for Scroll Plane A at VRAM 0xC000 (bits 3-5 = bits 13-15)
	dc.b 0x00 ; 0x03:  Pattern table for Window Plane at VRAM 0x0000 (disabled) (bits 1-5 = bits 11-15)
	dc.b 0x07 ; 0x04:  Pattern table for Scroll Plane B at VRAM 0xE000 (bits 0-2 = bits 11-15)
	dc.b 0x78 ; 0x05:  Sprite table at VRAM 0xF000 (bits 0-6 = bits 9-15)
	dc.b 0x00 ; 0x06:  Unused
	dc.b 0x00 ; 0x07:  Background colour: bits 0-3 = colour, bits 4-5 = palette
	dc.b 0x00 ; 0x08:  Unused
	dc.b 0x00 ; 0x09:  Unused
	dc.b 0x08 ; 0x0A: Frequency of Horiz. interrupt in Rasters (number of lines travelled by the beam)
	dc.b 0x00 ; 0x0B: External interrupts off, V scroll fullscreen, H scroll fullscreen
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
; Defines names for commonly used values and addresses to make
; the code more readable.
;==============================================================
	
; VDP port addresses
vdp_control				equ 0x00C00004
vdp_data				equ 0x00C00000

; VDP commands
vdp_cmd_vram_write		equ 0x40000000
vdp_cmd_cram_write		equ 0xC0000000

; VDP memory addresses
; according to VDP registers 0x2 and 0x4 (see table above)
vram_addr_tiles			equ 0x0000
vram_addr_plane_a		equ 0xC000
vram_addr_plane_b		equ 0xE000

; Screen width and height (in pixels)
vdp_screen_width		equ 0x0140
vdp_screen_height		equ 0x00F0

; The plane width and height (in tiles)
; according to VDP register 0x10 (see table above)
vdp_plane_width			equ 0x40
vdp_plane_height		equ 0x20

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

; Hello World draw position (in tiles)
text_pos_x				equ 0x08
text_pos_y				equ 0x04

;==============================================================
; VRAM WRITE MACROS
;==============================================================
; Some utility macros to help generate addresses and commands for
; writing data to video memory, since they're tricky (and
; error prone) to calculate manually.
; The resulting command and address is written to the VDP's
; control port, ready to accept data in the data port.
;==============================================================
	
; Set the VRAM (video RAM) address to write to next
SetVRAMWrite: macro addr
	move.l  #(vdp_cmd_vram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm
	
; Set the CRAM (colour RAM) address to write to next
SetCRAMWrite: macro addr
	move.l  #(vdp_cmd_cram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm
	
;==============================================================
; PALETTE
;==============================================================
; A single colour palette (16 colours) we'll be using to draw text.
; Colour #0 is always transparent, no matter what colour value
; you specify.
; We only use white (colour 2) and transparent (colour 0) in this
; demo, the rest are just examples.
;==============================================================
; Each colour is in binary format 0000 BBB0 GGG0 RRR0,
; so 0x0000 is black, 0x0EEE is white (NOT 0x0FFF, since the
; bottom bit is discarded), 0x000E is red, 0x00E0 is green, and
; 0x0E00 is blue.
;==============================================================
Palette:
	dc.w 0x0000	; Colour 0 = Transparent
	dc.w 0x0000	; Colour 1 = Black
	dc.w 0x0EEE	; Colour 2 = White
	dc.w 0x000E	; Colour 3 = Red
	dc.w 0x00E0	; Colour 4 = Blue
	dc.w 0x0E00	; Colour 5 = Green
	dc.w 0x0E0E	; Colour 6 = Pink
	dc.w 0x0000	; Leave the rest black...
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	dc.w 0x0000
	
;==============================================================
; GRAPHICS TILES
;==============================================================
; The 8x8 pixel graphics tiles that describe the font.
; We only need to specify glyphs for "HELO WRD" since they're reusable.
; 'SPACE' is first, which is unneccessary but it's a good teaching tool for
; why we leave the first tile in memory blank (try changing it
; and see what happens!).
;==============================================================
; 0 = transparent pixel
; 2 = colour 'white' in our palette (see palette above)
;==============================================================
; Change all of the 2's to 3, 4 or 5 to draw the text in red, blue
; or green (see the palette above).
;==============================================================
CharacterSpace:
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	
CharacterH:
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22222220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x00000000
	
CharacterE:
	dc.l 0x22222220
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22222220
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22222220
	dc.l 0x00000000
	
CharacterL:
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22222220
	dc.l 0x00000000
	
CharacterO:
	dc.l 0x22222220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22222220
	dc.l 0x00000000
	
CharacterW:
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22020220
	dc.l 0x22020220
	dc.l 0x22020220
	dc.l 0x22222220
	dc.l 0x00000000
	
CharacterR:
	dc.l 0x22222200
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22222200
	dc.l 0x22022000
	dc.l 0x22002200
	dc.l 0x22000220
	dc.l 0x00000000
	
CharacterD:
	dc.l 0x22222200
	dc.l 0x22002220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x22002220
	dc.l 0x22222200
	dc.l 0x00000000
	
;==============================================================
; TILE IDs
;==============================================================
; The indices of each tile above. Once the tiles have been
; written to VRAM, the VDP refers to each tile by its index.
;==============================================================
tile_id_space	equ 0x0
tile_id_h		equ 0x1
tile_id_e		equ 0x2
tile_id_l		equ 0x3
tile_id_o		equ 0x4
tile_id_w		equ 0x5
tile_id_r		equ 0x6
tile_id_d		equ 0x7
tile_count		equ 0x8	; Last entry is just the count

;==============================================================
; CODE ENTRY POINT
;==============================================================
; The "main()" function. Your code starts here. Once the CPU
; has finished initialising, it will jump to this entry point
; (specified in the vector table at the top of the file).
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
	; Initialise status register and set interrupt level.
	; This begins firing vertical and horizontal interrupts.
	;==============================================================
	move.w #0x2300, sr
	
	;==============================================================
	; Write the palette to CRAM (colour memory)
	;==============================================================
	
	; Setup the VDP to write to CRAM address 0x0000 (first palette)
	SetCRAMWrite 0x0000
	
	; Write the palette to CRAM
	lea    Palette, a0				; Move palette address to a0
	move.w #size_palette_w-1, d0	; Loop counter = 8 words in palette (-1 for DBRA loop)
	@PalLp:							; Start of loop
	move.w (a0)+, vdp_data			; Write palette entry, post-increment address
	dbra d0, @PalLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Write the font tiles to VRAM
	;==============================================================
	
	; Setup the VDP to write to VRAM address 0x0000 (the address of the first graphics tile, index 0)
	SetVRAMWrite vram_addr_tiles
	
	; Write the font glyph tiles to VRAM
	lea    CharacterSpace, a0					; Move the address of the first graphics tile into a0
	move.w #(tile_count*size_tile_l)-1, d0		; Loop counter = 8 longwords per tile * num tiles (-1 for DBRA loop)
	@CharLp:									; Start of loop
	move.l (a0)+, vdp_data						; Write tile line (4 bytes per line), and post-increment address
	dbra d0, @CharLp							; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Write the tile IDs of "HELLO WORLD" to Plane A's cell grid
	;==============================================================

	; Each scroll plane is made up of a 64x32 tile grid (this size is specified in VDP register 0x10),
	; with each cell specifying the index of each tile to draw, the palette to draw it with, and
	; some flags (for priority and flipping).
	;
	; Each plane cell is 1 word in size (2 bytes), in the binary format
	; ABBC DEEE EEEE EEEE, where:
	;
	;   A = Draw priority (1 bit)
	;   B = Palette index (2 bits, specifies palette 0, 1, 2, or 3)
	;   C = Flip tile horizontally (1 bit)
	;   D = Flip tile vertically (1 bit)
	;   E = Tile index to draw (11 bits, specifies tile index from 0 to 2047)
	;
	; Since we're using priority 0, palette 0, and no flipping, we
	; only need to write the tile ID and leave everything else zero.
	
	; Setup the VDP to write the tile ID at text_pos_x,text_pos_y in plane A's cell grid.
	; Plane A's cell grid starts at address 0xC000, which is specified in VDP register 0x2.
	;
	; Since each cell is 1 word in size, to compute a cell address within plane A:
	; ((y_pos * plane_width) + x_pos) * size_word
	SetVRAMWrite vram_addr_plane_a+(((text_pos_y*vdp_plane_width)+text_pos_x)*size_word)
	
	; then move the tile ID for "H" to VRAM
	move.w #tile_id_h, vdp_data		; H
	
	; Repeat for the remaining characters in the string.
	; We don't need to adjust the VRAM address each time, since the auto-increment
	; register (VDP register 0xF) is set to 2, so the destination address
	; will automatically increment by one word (conveniently the size of a cell)
	; after each write.
	move.w #tile_id_e, vdp_data		; E
	move.w #tile_id_l, vdp_data		; L
	move.w #tile_id_l, vdp_data		; L
	move.w #tile_id_o, vdp_data		; 0
	move.w #tile_id_space, vdp_data	; SPACE
	move.w #tile_id_w, vdp_data		; W
	move.w #tile_id_o, vdp_data		; O
	move.w #tile_id_r, vdp_data		; R
	move.w #tile_id_l, vdp_data		; L
	move.w #tile_id_d, vdp_data		; D

	; Finished!
	
	;==============================================================
	; Loop forever
	;==============================================================
	; This loops forever, effectively ending our code. The VDP will
	; still continue to run (and fire vertical/horizontal interrupts)
	; of its own accord, so it will continue to render our Hello World
	; even though the CPU is stuck in this loop.
	@InfiniteLp:
	bra @InfiniteLp
	
;==============================================================
; INTERRUPT ROUTINES
;==============================================================
; The interrupt routines, as specified in the vector table at
; the top of the file.
; Note that we use RTE to return from an interrupt, not
; RTS like a subroutine.
;==============================================================

; Vertical interrupt - run once per frame
INT_VInterrupt:
	; Doesn't do anything in this demo
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
	; Just halt the CPU if an error occurred. Later on, you may want to write
	; an exception handler to draw the current state of the machine to screen
	; (registers, stack, error type, etc) to help debug the problem.
	stop   #0x2700
	rte
	
;==============================================================
; UTILITY FUNCTIONS
;==============================================================
; Subroutines to initialise the TMSS, and load all VDP registers
;==============================================================

VDP_WriteTMSS:

	; The TMSS (Trademark Security System) locks up the VDP if we don't
	; write the string 'SEGA' to a special address. This was to discourage
	; unlicensed developers, since doing this displays the "LICENSED BY SEGA
	; ENTERPRISES LTD" message to screen (on Mega Drive models 1 and higher).
	;
	; First, we need to check if we're running on a model 1+, then write
	; 'SEGA' to hardware address 0xA14000.

	move.b hardware_ver_address, d0			; Move Megadrive hardware version to d0
	andi.b #0x0F, d0						; The version is stored in last four bits, so mask it with 0F
	beq    @SkipTMSS						; If version is equal to 0, skip TMSS signature
	move.l #tmss_signature, tmss_address	; Move the string "SEGA" to 0xA14000
	@SkipTMSS:

	; Check VDP
	move.w vdp_control, d0					; Read VDP status register (hangs if no access)
	
	rts

VDP_LoadRegisters:

	; To initialise the VDP, we write all of its initial register values from
	; the table at the top of the file, using a loop.
	;
	; To write a register, we write a word to the control port.
	; The top bit must be set to 1 (so 0x8000), bits 8-12 specify the register
	; number to write to, and the bottom byte is the value to set.
	;
	; In binary:
	;   100X XXXX YYYY YYYY
	;   X = register number
	;   Y = value to write

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

; A label defining the end of ROM so we can compute the total size.
ROM_End:
