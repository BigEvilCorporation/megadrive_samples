;==============================================================
; SEGA MEGA DRIVE/GENESIS - DEMO 4 - GAMEPAD SAMPLE
;==============================================================
; by Big Evil Corporation
;==============================================================

; A small, discreet, and complete gamepad sample, with a healthy
; dose of comments and explanations for beginners.
; Runs on genuine hardware, and (hopefully) all emulators.
;
; I recommend reading and understanding the Sprites sample first.
;
; To assemble this program with ASM68K.EXE:
;    ASM68K.EXE /p gamepad.asm,gamepad.bin,gamepad.map,gamepad.lst
;
; To assemble this program with SNASM68K.EXE:
;    SNASM68K.EXE /p gamepad.asm,gamepad.map,gamepad.lst,gamepad.bin
;
; gamepad.asm = this source file
; gamepad.bin = the binary file, fire this up in your emulator!
; gamepad.lst = listing file, shows assembled addresses alongside
;               your source code, open in a text editor
; gamepad.map = symbol map file for linking (unused)

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
vram_addr_sprite_table	equ 0xF000
vram_addr_hscroll		equ 0xFC00

; Screen width and height (in pixels)
vdp_screen_width		equ 0x0140
vdp_screen_height		equ 0x00F0

; The plane width and height (in tiles)
; according to VDP register 0x10 (see table above)
vdp_plane_width			equ 0x40
vdp_plane_height		equ 0x20

; The size of the sprite plane (512x512 pixels)
vdp_sprite_plane_width	equ 0x0200
vdp_sprite_plane_height	equ 0x0200

; The sprite border (invisible area left + top) size
vdp_sprite_border_x		equ 0x80
vdp_sprite_border_y		equ 0x80

; Hardware version address
hardware_ver_address	equ 0x00A10001

; TMSS
tmss_address			equ 0x00A14000
tmss_signature			equ 'SEGA'

; Gamepad/IO port addresses.
; See PAD_ReadPadA near bottom of file for usage
pad_ctrl_a				equ 0x00A10009	; IO port A control port
pad_ctrl_b				equ 0x00A1000B	; IO port B control port
pad_data_a				equ 0x00A10003	; IO port A data port
pad_data_b				equ 0x00A10005	; IO port B data port

; Pad read latch, for fetching second byte from data port
pad_byte_latch			equ 0x40

; Gamepad button bits
;
; After converting pad input data to a word (in PAD_ReadPadA),
; we can test each individual bit using BTST and these constants.
;
; The pad data word is in the binary format:
;  00SA0000 00CBRLDU
; (Start, A, C, B, Right, Left, Down, Up)

pad_button_up			equ 0x0
pad_button_down			equ 0x1
pad_button_left			equ 0x2
pad_button_right		equ 0x3
pad_button_a			equ 0xC
pad_button_b			equ 0x4
pad_button_c			equ 0x5
pad_button_start		equ 0xD

; All gamepad button bits (for masking)
pad_button_all			equ 0x303F

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

; Text draw pos (in tiles)
text_pos_x				equ 0x02
text_pos_y				equ 0x02

; Sprite initial draw position (in pixels)
sprite_start_pos_x		equ vdp_sprite_border_x
sprite_start_pos_y		equ vdp_sprite_border_y

; Speed (in pixels per frame) to move the sprite
sprite_move_speed_x		equ 0x1
sprite_move_speed_y		equ 0x1

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
	RSSET 0x00FF0000			; Map from start of RAM
ram_sprite_pos_x		rs.w 1	; Sprite x pos (word)
ram_sprite_pos_y		rs.w 1	; Sprite y pos (word)

;==============================================================
; PALETTE
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

; Number of palettes to write to CRAM
palette_count	equ 0x1

;==============================================================
; TILE IDs
;==============================================================
tile_id_blank		equ 0x00	; The blank tile at index 0
tile_id_u			equ 0x01	; Font tile U
tile_id_d			equ 0x02	; Font tile D
tile_id_l			equ 0x03	; Font tile L
tile_id_r			equ 0x04	; Font tile R
tile_id_a			equ 0x05	; Font tile A
tile_id_b			equ 0x06	; Font tile B
tile_id_c			equ 0x07	; Font tile C
tile_id_s			equ 0x08	; Font tile S
tile_id_sprite_1	equ 0x09	; Sprite 1 index (4 tiles)
tile_count			equ 0x0C	; Total tiles to load (excl. blank) = 12

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

	; Initialise gamepads
	jsr    PAD_InitPads

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
	lea    Palettes, a0				; Move palette address to a0
	move.w #(palette_count*size_palette_w)-1, d0	; Loop counter = 8 words in palette (-1 for DBRA loop)
	@PalLp:							; Start of loop
	move.w (a0)+, vdp_data			; Write palette entry, post-increment address
	dbra d0, @PalLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Write the sprite tiles to VRAM
	;==============================================================
	
	; Setup the VDP to write to VRAM address 0x0020 (skips the first
	; tile, leaving it blank).
	SetVRAMWrite vram_addr_tiles+size_tile_b
	
	; Write all graphics tiles to VRAM
	lea    Tiles, a0							; Move the address of the first graphics tile into a0
	move.w #(tile_count*size_tile_l)-1, d0		; Loop counter = 8 longwords per tile * num tiles (-1 for DBRA loop)
	@CharLp:									; Start of loop
	move.l (a0)+, vdp_data						; Write tile line (4 bytes per line), and post-increment address
	dbra d0, @CharLp							; Decrement d0 and loop until finished (when d0 reaches -1)

	;==============================================================
	; Initialise scroll
	;==============================================================
	SetVRAMWrite vram_addr_hscroll
	move.w #0x0000, vdp_data	; Plane A h-scroll
	move.w #0x0000, vdp_data	; Plane B h-scroll

	SetVSRAMWrite 0x0000
	move.w #0x0000, vdp_data	; Plane A v-scroll
	move.w #0x0000, vdp_data	; Plane B v-scroll
	
	;==============================================================
	; Set up the Sprite Attribute Table (SAT)
	;==============================================================

	; Start writing to the sprite attribute table in VRAM
	SetVRAMWrite vram_addr_sprite_table

	; Set up sprite 1
	;
	; Position:   sprite_start_pos_x,sprite_start_pos_y
	; Dimensions: 2x2 tiles (8 tiles total) = 0101 in binary
	; Next link:  sprite index 1 is next to be processed
	; Priority:   0
	; Palette id: 0
	; Flip X:     0
	; Flip Y:     0
	; Tile id:    tile_id_sprite_1
	BuildSpriteStructure sprite_start_pos_x,sprite_start_pos_y,%0101,0x1,0x0,0x0,0x0,0x0,tile_id_sprite_1,d0,d1,d2,d3

	; Write the entire sprite attribute structure to the sprite table
	move.w d0, vdp_data
	move.w d1, vdp_data
	move.w d2, vdp_data
	move.w d3, vdp_data

	;==============================================================
	; Intitialise variables in RAM
	;==============================================================
	move.w #sprite_start_pos_x, ram_sprite_pos_x
	move.w #sprite_start_pos_y, ram_sprite_pos_y

	;==============================================================
	; Initialise status register and set interrupt level.
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

	; Read pad A state, result in d0 (word size)
	; in format: 00SA0000 00CBRLDU
	jsr    PAD_ReadPadA

	; Draw pad A state to screen
	jsr    DrawPadState

	; Fetch current sprite coordinate from RAM
	move.w ram_sprite_pos_x, d2
	move.w ram_sprite_pos_y, d3

	; If UP button held, move sprite up
	btst   #pad_button_up, d0
	beq    @NoUp
	subi.w #sprite_move_speed_y, d3
	@NoUp:

	; If DOWN button held, move sprite down
	btst   #pad_button_down, d0
	beq    @NoDown
	addi.w #sprite_move_speed_y, d3
	@NoDown:

	; If LEFT button held, move sprite left
	btst   #pad_button_left, d0
	beq    @NoLeft
	subi.w #sprite_move_speed_x, d2
	@NoLeft:

	; If RIGHT button held, move sprite right
	btst   #pad_button_right, d0
	beq    @NoRight
	addi.w #sprite_move_speed_x, d2
	@NoRight:

	; Store updated values back in RAM for next frame
	move.w d2, ram_sprite_pos_x
	move.w d3, ram_sprite_pos_y

	; Write updated coordinates to the Sprite Attribute Table in VRAM.
	SetVRAMWrite vram_addr_sprite_table+0x0000	; Sprite 1 Y value
	move.w d3, vdp_data
	SetVRAMWrite vram_addr_sprite_table+0x0006	; Sprite 1 X value
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

DrawPadState:

	; Draws the pad state (in d0) to screen on plane A.
	; See the Hello World sample for what this is up to.

	; Start writing to plane A at pos x,y
	SetVRAMWrite vram_addr_plane_a+(((text_pos_y*vdp_plane_width)+text_pos_x)*size_word)

	; For each button:
	;  - Default to blank tile in d2
	;  - If button pressed, replace d2 with font tile
	;  - Write d2 to plane A

	; UP
	move.w #tile_id_blank, d2
	btst   #pad_button_up, d0
	beq    @NoUp
	move.w #tile_id_u, d2
	@NoUp:
	move.w d2, vdp_data

	; DOWN
	move.w #tile_id_blank, d2
	btst   #pad_button_down, d0
	beq    @NoDown
	move.w #tile_id_d, d2
	@NoDown:
	move.w d2, vdp_data

	; LEFT
	move.w #tile_id_blank, d2
	btst   #pad_button_left, d0
	beq    @NoLeft
	move.w #tile_id_l, d2
	@NoLeft:
	move.w d2, vdp_data

	; RIGHT
	move.w #tile_id_blank, d2
	btst   #pad_button_right, d0
	beq    @NoRight
	move.w #tile_id_r, d2
	@NoRight:
	move.w d2, vdp_data

	; A
	move.w #tile_id_blank, d2
	btst   #pad_button_a, d0
	beq    @NoA
	move.w #tile_id_a, d2
	@NoA:
	move.w d2, vdp_data

	; B
	move.w #tile_id_blank, d2
	btst   #pad_button_b, d0
	beq    @NoB
	move.w #tile_id_b, d2
	@NoB:
	move.w d2, vdp_data

	; C
	move.w #tile_id_blank, d2
	btst   #pad_button_c, d0
	beq    @NoC
	move.w #tile_id_c, d2
	@NoC:
	move.w d2, vdp_data

	; START
	move.w #tile_id_blank, d2
	btst   #pad_button_start, d0
	beq    @NoS
	move.w #tile_id_s, d2
	@NoS:
	move.w d2, vdp_data

	rts

PAD_InitPads:

	; Initialise both gamepad IO ports by writing the latch bit
	; to each pad's control port.
	move.b #pad_byte_latch, pad_ctrl_a
	move.b #pad_byte_latch, pad_ctrl_b

	rts

PAD_ReadPadA:
	; Returns: d0 (word) - pad A state in format 00SA0000 00CBRLDU

	; To read a gamepad, we need to read one byte at a time from
	; address A10003 (gamepad port 1) or A10005 (gamepad port 2).
	; To do this, we write to the port first to tell it whether we
	; we want the first or the second byte of data, then read from it.
	;
	; The first byte contains the Start and A button states (in binary
	; format 00SA0000), and the second byte contains C, B, RIGHT, LEFT,
	; UP, and DOWN button states (in binary format 00CBRLDU).
	;
	; 6-button pads are a little more complex, and are beyond the
	; scope of this sample.
	
	; First, write 0 to the data port for pad A to tell it we want
	; the first byte (clears the "latch" bit).
	move.b  #0x0, pad_data_a

	; Delay by 2 NOPs (opcodes that do nothing) to ensure the
	; request was received before continuing. This was recommended
	; by a SEGA developer bulletin in response to some rare cases
	; where the data port was returning incorrect data.
	nop
	nop

	; Read the first byte of data from the data port
	move.b  pad_data_a, d0

	; Shift the byte into place in register d0 (we are returning
	; both bytes as a single word from this routine).
	lsl.w   #0x8, d0

	; Write the "latch" bit, to tell it we want to read the second
	; byte next.
	move.b  #pad_byte_latch, pad_data_a

	; 2-NOP delay to respond to change
	nop
	nop

	; Read the second byte of data from data port
	move.b  pad_data_a, d0
	
	; Invert and mask all bytes received.
	; The data port returns the button state bits as 1=button up,
	; 0=button down, which doesnt make sense when using it in game code.
	;
	; We also clear any unused bits, so we can determine if ANY buttons
	; are held by checking if the returned word is non-zero.
	neg.w   d0
	subq.w  #0x1, d0
	andi.w  #pad_button_all, d0

	rts

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
; The sprite graphics tiles.
;==============================================================

Tiles:

	; Font tiles - for pad bits UDLRABCS
	include "padfont.asm"

	; Sprite 1 - a red Fuzzl, 2x2 tiles
	include "sprite1.asm"

; The end of ROM
ROM_End:
