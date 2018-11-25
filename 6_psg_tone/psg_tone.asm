;==============================================================
; SEGA MEGA DRIVE/GENESIS - DEMO 6 - PSG TONE SAMPLE
;==============================================================
; by Big Evil Corporation
;==============================================================
; A small, discreet, and complete 1-channel PSG Tone sample,
; with a healthy dose of comments and explanations for beginners.
; Runs on genuine hardware, and (hopefully) all emulators.
;
; I recommend reading and understanding the Sprites sample first.
;
; To assemble this program with ASM68K.EXE:
;    ASM68K.EXE /p psg_tone.asm,psg_tone.bin,psg_tone.map,psg_tone.lst
;
; To assemble this program with SNASM68K.EXE:
;    SNASM68K.EXE /p psg_tone.asm,psg_tone.map,psg_tone.lst,psg_tone.bin
;
; psg_tone.asm = this source file
; psg_tone.bin = the binary file, fire this up in your emulator!
; psg_tone.lst = listing file, shows assembled addresses alongside
;           your source code, open in a text editor
; psg_tone.map = symbol map file for linking (unused)
;==============================================================

;==============================================================
; Usage
;==============================================================
; LEFT = Increase frequency of channel 0
; RIGHT = Decrease frequency of channel 0
; UP = Increase volume of channel 0
; DOWN = Decrease volume of channel 0
;==============================================================

;==============================================================
; PSG overview
;==============================================================
; The PSG is derived from a Texas Instruments SN76489 chip, which
; holds 4 sound channels - three of which generate square waves,
; one generates white noise. The chip runs at a clock cycle of 3579545hz.
;
; Each channel can be configured with a frequency and an attenuation
; value, programmed through the chip's single 8-bit port (at 0xC00011
; from the 68000, and 0x7F11 from the Z80).
;
; The PSG's square waves work by counting down every 16th clock cycle,
; inverting the wave's polarity when the counter hits 0, then resetting
; the counter back to its initial value. To set the frequency, you
; simply provide the initial value to the channel's frequency register.
;
; Frequency = 3579545 / (2 x 16 x reg value)
;
; E.g.: 0xFE = 440hz
;
; The amplitude of the wave is the volume. This is also set via a
; register for each channel, which accepts an attenuation value.
;
; Frequency registers are 10 bits in size, accepting values from 0x1
; (111860hz) to 0x3FF (109hz).
; Attenuation registers are 4 bits in size, accepting values from 0x0
; (full volume) to 0xF (muted).
;
; The PSG has an 8-bit port. The first bit is the latch, which determines
; if the first or second byte is currently being written. Attenuation writes
; only need a single byte written, frequency writes need two bytes.
;
; First byte (latch bit ON):
; ABBCDDDD
; A = Latch (1)
; B = Channel ID (0-3)
; C = Data type (1=attenuation, 0=frequency)
; D - Data bits 0-3
;
; Second byte (latch bit OFF):
; A0BBBBBB
; A = Latch (0)
; 0 = Unused
; B = Data bits 4-9
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

; PSG port address (accessible from 68000)
; The PSG can be accessed from both the 68000 and the Z80, although each has its own
; address from which to do so.
; We'll be accessing the PSG from the 68000 only in this demo for simplicity.
psg_control				equ 0x00C00011	; NEW in this demo - address of PSG control port

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

; Gamepad/IO port addresses
pad_ctrl_a				equ 0x00A10009	; IO port A control port
pad_ctrl_b				equ 0x00A1000B	; IO port B control port
pad_data_a				equ 0x00A10003	; IO port A data port
pad_data_b				equ 0x00A10005	; IO port B data port

; Pad read latch, for fetching second byte from data port
pad_byte_latch			equ 0x40

; Gamepad button bits
pad_button_up			equ 0x0
pad_button_down			equ 0x1
pad_button_left			equ 0x2
pad_button_right		equ 0x3
pad_button_a			equ 0xC
pad_button_b			equ 0x4
pad_button_c			equ 0x5
pad_button_start		equ 0xD
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

; Initial PSG values
initial_psg_vol			equ 0x08 ; (half volume)
initial_psg_freq		equ 0xFE ; (440hz)

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
; MEMORY MAP
;==============================================================
	RSSET 0x00FF0000			; Map from start of RAM
ram_psg_frequency		rs.w 1	; Current PSG frequency (2 bytes)
ram_psg_volume			rs.b 1	; Current PSG volume (1 byte)

; CAUTION! Memory map is now an odd size, don't add words or longwords
; without padding it first!

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

	; Initialise the PSG (mutes all channels)
	jsr    PSG_Init

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
	;==============================================================
	move.w #0x2300, sr

	;==============================================================
	; Set initial PSG channel 0 values
	;==============================================================

	; Initialise PSG values in RAM
	move.b #initial_psg_vol, ram_psg_volume
	move.w #initial_psg_freq, ram_psg_frequency

	; Set PSG channel 0 frequency
	move.b #0x0, d0
	move.b #initial_psg_freq, d1
	jsr    PSG_SetFrequency

	; Set PSG channel 0 volume
	move.b #0x0, d0
	move.b #initial_psg_vol, d1
	jsr    PSG_SetVolume

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

	; Backup to a register unused by PSG_* routines
	move.w d0, d6

	; Fetch current PSG frequency from RAM
	move.w ram_psg_frequency, d1

	; If RIGHT button pressed, increase frequency (decrease PSG timer value)
	btst   #pad_button_right, d0
	beq    @NoRight
	subi.w #0x1, d1
	@NoRight:

	; If LEFT button pressed, decrease frequency (increase PSG timer value)
	btst   #pad_button_left, d0
	beq    @NoLeft
	addi.w #0x1, d1
	@NoLeft:

	; Write new frequency back to RAM
	move.w d1, ram_psg_frequency

	; Write new frequency to PSG channel 0
	move.b #0x1, d0
	jsr    PSG_SetFrequency

	; Restore gamepad from backup
	move.w d6, d0

	; Fetch current PSG volume from RAM
	move.b ram_psg_volume, d1

	; If UP button pressed, increase volume (and clamp to 15)
	btst   #pad_button_up, d0
	beq    @NoUp
	addi.w #0x1, d1
	cmp.b  #0x0F, d1
	ble    @NoClampUp
	move.b #0x0F, d1
	@NoClampUp:
	@NoUp:

	; If DOWN button pressed, decrease volume (and clamp to 0)
	btst   #pad_button_down, d0
	beq    @NoDown
	subi.w #0x1, d1
	cmp.b  #0x0, d1
	bge    @NoClampDown
	move.b #0x0, d1
	@NoClampDown:
	@NoDown:

	; Write new volume back to RAM
	move.b d1, ram_psg_volume

	; Write new volume to PSG channel 0
	move.b #0x1, d0
	jsr    PSG_SetVolume

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

PAD_InitPads:

	; Initialise both gamepad IO ports by writing the latch bit
	; to each pad's control port.
	move.b #pad_byte_latch, pad_ctrl_a
	move.b #pad_byte_latch, pad_ctrl_b

	rts

PAD_ReadPadA:
	; Returns: d0 (word) - pad A state in format 00SA0000 00CBRLDU

	; First, write 0 to the data port for pad A to tell it we want
	; the first byte (clears the "latch" bit).
	move.b  #0x0, pad_data_a

	; 2-NOP delay to respond to change
	nop
	nop

	; Read the first byte of data from the data port
	move.b  pad_data_a, d0

	; Shift the byte into place in register d0
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
	neg.w   d0
	subq.w  #0x1, d0
	andi.w  #pad_button_all, d0

	rts

PSG_Init:
	; Initialises the PSG - sets volumes of all channels to 0

	; Writing one byte per channel

	; Latch bit = 1 (this is the only byte written)
	; Channel = 0
	; Data type = 1 (attenuation)
	; Data = 0xF (fully attenuated)
	move.b #0x9F, psg_control

	; Latch bit = 1 (this is the only byte written)
	; Channel = 1
	; Data type = 1 (attenuation)
	; Data = 0xF (fully attenuated)
	move.b #0xBF, psg_control

	; Latch bit = 1 (this is the only byte written)
	; Channel = 2
	; Data type = 1 (attenuation)
	; Data = 0xF (fully attenuated)
	move.b #0xDF, psg_control

	; Latch bit = 1 (this is the only byte written)
	; Channel = 3
	; Data type = 1 (attenuation)
	; Data = 0xF (fully attenuated)
	move.b #0xFF, psg_control

	rts

PSG_SetVolume:
	; d0.b - Channel index (0 - 3)
	; d1.b - Volume (0 - 15)

	; This is a slow and naive approach, and would be better wrapped up in
	; a macro, but as a teaching tool it's better to be verbose about
	; what's happening.

	; ABBCDDDD
	; A = Latch (1)
	; B = Channel ID (0-3)
	; C = Data type (1=attenuation)
	; D - Attenuation (1 nybble)

	; Invert the volume to convert it to an attenuation
	move.b #0x0F, d2
	sub.b  d1, d2

	; Shift channel index to bits 6-5
	lsl.b  #0x5, d0

	; OR in the attenuation
	or.b   d2, d0

	; Set the type bit (1=attenuation)
	ori.b  #0x10, d0

	; Set the latch bit (only writing 1 byte)
	ori.b  #0x80, d0

	; Write to PSG
	move.b d0, psg_control

	rts

PSG_SetFrequency:
	; d0.b - Channel index (0 - 3)
	; d1.w - Frequency (as timer value, 0x1 - 0x3FF)

	; This is a slow and naive approach, and would be better wrapped up in
	; a macro, but as a teaching tool it's better to be verbose about
	; what's happening.

	; First byte (latch bit ON):
	; ABBCDDDD
	; A = Latch (1)
	; B = Channel ID (0-3)
	; C = Data type (0=frequency)
	; D - Frequency bits 0-3
	;
	; Second byte (latch bit OFF):
	; A0BBBBBB
	; A = Latch (0)
	; 0 = Unused
	; B = Frequency bits 4-9

	; Split the frequency into two bytes
	move.w d1, d2

	; Mask low byte to 4 bits (and sets type bit for 1st byte to 0=frequency)
	andi.b #0x0F, d1

	; Shift upper 6 bits down, mask unused bytes (and sets latch bit for 2nd byte to 0)
	lsr.w  #0x4, d2
	andi.b #0x1F, d2

	; Shift channel index to bits 6-5
	lsl.b  #0x5, d0

	; OR in the low bits
	or.b   d1, d0

	; Set latch bit on 1st byte
	ori.b  #0x80, d0

	; Write byte 1 to PSG (channel ID and lower 4 bits of frequency)
	move.b d0, psg_control

	; Write byte 2 to PSG (upper 6 bits of frequency)
	move.b d2, psg_control

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

; The end of ROM
ROM_End:
