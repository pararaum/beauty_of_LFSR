;;; asm6502 -l /dev/stderr -e -b 0x07ff pseudo.ex.macro.asm
RFEED = $20			;Pointer to feedback terms.
RFEEDEND = $22			;Pointer to end of feedback terms.
DISPLAYPOINTER = $24		;Pointer for sprite hex-display routine.
CONST02 = $02			;Here we store a constant '2'.
RFEEDBACKTERM = $80		;Register for feedbackterm
RFEEDBACKVAL = $82		;Current feedbackterm value
RBITMAPPTR = $84		;Pointer to bitmap
RFEEDCURR = $86			;Register for current LFSR value
PTRVIC = $04			;Pointer used for vic copy routine

SCROLLCOUNTER = $10		;Counter for scroller
SCRTEXTPTR = $11		;Scroll text pointer


RASTERCOUNTER = $13	   	;16Bit
ACTION = $15			;Actionflag
	;; bit 7: 1=action ongoing
	;; bit 6: ?
	;; bits 0-5: action number

JOYSTICKMASK = $18		;See the get_fire_status routine.

FRAMECOUNTER =$A0		;Incremented every frame (32bit)

IDLE_DATA = $8000+$3FFF		;Pattern in the border.
SCRSPRDATA = $8a00		;Data area for sprite scroller ($200 bytes)

;;; Memory Map
;;; 8000-80ff: Buffer for four sprites to display hex
;;; 8100-81ff: ACTION_SPRITE_BUF = $8100	;Buffer for four sprites.
;;; 8200-89ff: Buffer for further sprites
;;; 8a00-8bff: Scroller buffer
;;; 8c00-8fe7: Screen buffer
;;; 9000-9fff: Character Generator ROM
;;; a000-bf3f: Bitmap buffer
;;; bf40-bfff: Destroyed by LFSR...
;;;

;;; 6502 Vectors [https://www.pagetable.com/?p=410, http://6502.org/tutorials/interrupts.html#2.2]:
NMI_VECTOR = $FFFA
RESET_VECTOR = $FFFC
IRQ_VECTOR = $FFFE
;;; Kernal routines
CHROUT = $FFD2

DEBUG := 0

	.org $0801 - 2
	.word $0801

basic:	.word end_of_basic
	.word main
	.byte $9e,$20,$c2
	.byte "(57)",$aa,"256",$ac,$c2,"(58)"
end_of_basic:
	.byte 0,0,0

	INCLUDE "macro.pseudo.inc"

BITMAPPTR = $a000
SCREENPTR = $8c00
	INCLUDE "bitmap_n_screen.inc"

clearscreen:
	P_loadi $60, $a000
	P_loadi $62, $c000
	P_loadi $64, $0000
	P_loadi $5e, $0002
.l2:	P_store $64, $60
	P_add  $5e, $60
	P_transfer $60, $fe
	P_sub $62, $fe
	P_branchNZ $fe, .l2
	lda #$E2
	ldx #0
.l1	sta SCREENPTR,x
	sta SCREENPTR+$100,x
	sta SCREENPTR+$200,x
	sta SCREENPTR+$300,x
	pha
	lda #1
	sta $d800,x
	sta $d900,x
	sta $da00,x
	sta $db00,x
	pla
	dex
	bne .l1
	rts

spriteinit:
	lda #$ff
	sta $d015		;Enable all sprites
	sta $d01d		;double width
	sta $d017		;double height
	sta $d01c		;multicolour mode
	lda #3			;Sprite colour is turquoise.
	ldy #7
.l3:	sta $d027,y
	dey
	bpl .l3
	lda #5			;green
	sta $d025
	lda #13			;light green
	sta $d026
	ldx #00			;Clear sprite data.
	txa
.l2	sta SCRSPRDATA,x
	sta SCRSPRDATA+$100,x
	dex
	bne .l2
	rts

initgfx:lda #$04		;Border is purple
	sta $d020
	lda #$04		;Background purple (you need to take care of the idle pattern).
	sta $d021
	lda #%00111011		;Hires and 25 lines.
	sta $d011
	lda #%00001000		;Hires bitmap mode.
	sta $d016
	jsr initialise_bitmap_and_screenptr
	lda #$7f
        sta $dc0d               ;disable CIA interrupts
        sta $dd0d
        lda $dc0d               ;clear pending interrupts
        lda $dd0d
        lda #%00000001		;Enable raster IRQ
        sta $d01a
        lda #$f9		;IRQ on line:
	sta $d012
	rts

lfsrE:	P_transfer RFEEDCURR, RFEEDBACKTERM
	P_transfer RFEEDCURR, RFEEDBACKVAL
	P_loadi RBITMAPPTR, BITMAPPTR
.l2:
	ldx RFEEDBACKTERM
	P_shiftr RFEEDBACKTERM
	txa
	and #$01
	beq .l1
	P_eor RFEEDBACKVAL, RFEEDBACKTERM
.l1:
	P_transfer RFEEDBACKTERM, RFEEDCURR
	P_shiftr RFEEDCURR
	P_shiftr RFEEDCURR
	P_shiftr RFEEDCURR
	P_add RBITMAPPTR, RFEEDCURR
	lda RFEEDBACKTERM
	and #$07
	tax
	ldy #0
	lda (RFEEDCURR),y
	eor .bitlist,x
	sta (RFEEDCURR),y
	;; Instead of checking all the time if we may overwrite the
	;; idle pattern we just always write a zero.
	sty IDLE_DATA
	P_transfer RFEEDBACKTERM, RFEEDCURR
	P_sub RFEEDBACKVAL, RFEEDCURR
	;;
	jsr get_fire_status
	bne .s1
	jmp main_quit
.s1:	;; 
	P_branchNZ RFEEDCURR, .l2
	lda #$01		;Zero is never reached therefore hardcoded here.
	eor BITMAPPTR
	sta BITMAPPTR
	rts
.bitlist: .byte $01, $02, $04, $08, $10, $20, $40, $80

;;; http://users.ece.cmu.edu/~koopman/lfsr/16.txt
feedbt:	.word $8016, $801C, $801F, $8029, $805E, $806B, $8097, $809E
	.word $80A7, $80AE, $80CB, $80D0, $80D6, $80DF, $80E3, $810A
	.word $810C, $8112, $8117, $812E, $8136, $8142, $8148, $8150
	.word $8172, $818E, $81A5, $81B4, $81B8, $81C3, $81C6, $81CF
	.word $81D1, $81EE, $81FC, $8214, $822B, $8233, $8241, $8244
	.word $8248, $825F, $8260, $8299, $82A3, $82B4, $82C3, $82E1
	.word $82EE, $82F5, $8320, $8325, $8329, $8345, $8361, $83B5
	.word $83B6, $83BC, $83C1, $83F8, $8406, $8430, $845F, $846A
	.word $846F, $8471, $8478, $847D, $849C, $84BE, $84C5, $84D2
	.word $84D7, $84E1, $84E2, $84F3, $84F9, $853E, $8540, $855D
	.word $8562, $8580, $8589, $858A, $85A8, $85AE, $85E6, $85E9
	.word $85F2, $8607, $860E, $8610, $8634, $8638, $863D, $8646
	.word $864A, $8651, $8657, $8679
feedbt_end:

;;; Copy VIC registers (destroys A/Y)
;;; PTRVIC points VIC registers
;;; Up to control register 2 ($D016) is copied
copy_vic_data:
	ldy #$16
.l1:	lda (PTRVIC),y
	sta $d000,y
	dey
	bpl .l1
	rts

display:			;rFE value
	P_push RFEEDCURR		;Push value
	P_push RFEEDCURR		;Push value
	P_push RFEEDCURR		;Push value
	;; X---
	lda RFEEDCURR+1
	lsr
	lsr
	lsr
	lsr
	sta RFEEDCURR
	P_loadi $f8, $8000
	jsr .copy
	P_pull RFEEDCURR
	;; -X--
	lda RFEEDCURR+1
	sta RFEEDCURR
	P_loadi $f8, $8000+64
	jsr .copy
	P_pull RFEEDCURR
	;; --X-
	lda RFEEDCURR
	lsr
	lsr
	lsr
	lsr
	sta RFEEDCURR
	P_loadi $f8, $8000+2*64
	jsr .copy
	P_pull RFEEDCURR
	;; ---X
	P_loadi $f8, $8000+3*64
	jsr .copy
	rts
.copy:
	P_loadi DISPLAYPOINTER, $000f
	P_and RFEEDCURR, DISPLAYPOINTER
	P_shiftl DISPLAYPOINTER		;times 64
	P_shiftl DISPLAYPOINTER
	P_shiftl DISPLAYPOINTER
	P_shiftl DISPLAYPOINTER
	P_shiftl DISPLAYPOINTER
	P_shiftl DISPLAYPOINTER
	P_loadi $fa, spritenumberfont
	P_add $fa, DISPLAYPOINTER
	ldy #63
.l1	lda (DISPLAYPOINTER),y
	sta ($f8),y
	dey
	bpl .l1
	rts

increment_frame_counter:
	inc FRAMECOUNTER
	bne .out
	inc FRAMECOUNTER+1
	bne .out
	inc FRAMECOUNTER+2
	bne .out
	inc FRAMECOUNTER+3
	bne .out
.out: rts

irq00:	jsr increment_frame_counter
	lda #<.vicdata
	sta PTRVIC
	lda #>.vicdata
	sta PTRVIC+1
	jsr copy_vic_data
	;; Set sprite pointers
	ldx #0
.l2	txa
	sta SCREENPTR+1024-8,x
	inx
	cpx #8
	bne .l2
	lda #$ff
	sta $d01c		;multicolour mode
	lda #$0f
	sta $d01d		;X-Expansion
	sta $d017		;Y-Expansion
	lda #5			;green
	sta $d025
	lda #3			;Sprite colour is turquoise.
	sta $d027
	sta $d028
	sta $d029
	sta $d02A
	jsr perform_scroller
	lda #<irq33
	sta myirqvector
	lda #>irq33
	sta myirqvector+1
	rts
.vicdata:
	.byte 76, $0d		;sprite positions
	.byte 132, $0d
	.byte 196, $0d
	.byte 241, $0d
	.byte 255, $0d
	.byte 255, $0d
	.byte 255, $0d
	.byte 255, $0d
	.byte 0			;MSBs of sprites
	.byte %00111011		;Hires and 25 lines.
	.byte $33		;next IRQ on line:
	.word $DEAD		;Light pen
	.byte %00001111		;sprite enabled
	.byte %00011000		;MCM, 40 columns, 0 pixels offset


irq33:	lda #<irqEB
	sta myirqvector
	lda #>irqEB
	sta myirqvector+1
	lda #$eb
	sta $d012
	lda #%11110000		;Enable other sprites, not digits.
	sta $d015
	bit ACTION
	bpl .noaction
	jsr action_run
	jmp .out
.noaction:
	inc RASTERCOUNTER
	bne .out
	inc RASTERCOUNTER+1
	lda RASTERCOUNTER+1
	IF DEBUG == 0
	cmp #$02
	ELSE
	cmp #$01
	ENDIF
	bne .out
	jsr action_init
	ldx #$0
	stx RASTERCOUNTER+1
.out:
	IF DEBUG != 0
	inc $d020
	ENDIF
	jsr muzak_data+3	;play muzak
	IF DEBUG != 0
	dec $d020
	ENDIF
	rts
.counter:
	.word $FFFD


irqEB:	lda #<.vicdata
	sta PTRVIC
	lda #>.vicdata
	sta PTRVIC+1
	jsr copy_vic_data
	;; Set sprite pointers
	ldx #0
	ldy #(SCRSPRDATA & $3FFF)/64
.l2	tya
	sta SCREENPTR+1024-8,x
	inx
	iny
	cpx #8
	bne .l2
	lda #<irqF8
	sta myirqvector
	lda #>irqF8
	sta myirqvector+1
	rts
.vicdata:
SPRSCRMINX = -8
	.byte $F0, $FD		;sprite positions
	.byte SPRSCRMINX+1*48, $FD
	.byte SPRSCRMINX+2*48, $FD
	.byte SPRSCRMINX+3*48, $FD
	.byte SPRSCRMINX+4*48, $FD
	.byte SPRSCRMINX+5*48, $FD
	.byte SPRSCRMINX+6*48, $FD
	.byte SPRSCRMINX+7*48, $FD
	.byte (($1F0 > 256)<<0)|((SPRSCRMINX+1*48 > 256)<<1)|((SPRSCRMINX+2*48 > 256)<<2)|((SPRSCRMINX+3*48 > 256)<<3)|((SPRSCRMINX+4*48 > 256)<<4)|((SPRSCRMINX+5*48 > 256)<<5)|((SPRSCRMINX+6*48 > 256)<<6)|((SPRSCRMINX+7*48 > 256)<<7)	;MSBs of sprites
	.byte %00111011		;Hires and 25 lines.
	.byte $f8		;next IRQ on line:
	.word $DEAD		;Light pen
	.byte %11111111		;sprite enabled
	.byte %00011000		;MCM, 40 columns, 0 pixels offset



irqF8:	lda #%00110011		;Hires and 24 lines.
	sta $d011
	lda #$00
	sta $d01c		;no sprite multicolour mode
	lda #$ff
	sta $d01d		;X-Expansion
	sta $d017		;Y-Expansion
	lda #$03		;Reset colour of sprite 4..7
	sta $d02b
	sta $d02c
	sta $d02d
	sta $d02e
	lda #$FC
	sta $d012		;Next irq on line $FC.
	lda #<irqFB
	sta myirqvector
	lda #>irqFB
	sta myirqvector+1
	IF DEBUG != 0
	lda $d012
.smc:	sta $0400
	inc .smc+1
	ENDIF
	rts

irqFB:	jsr copper_run
	lda #$00
	sta $d012		;Next irq on line 0.
	lda #<irq00
	sta myirqvector
	lda #>irq00
	sta myirqvector+1
	rts

myirq:	pha
	txa
	pha
	tya
	pha
	asl $d019		;Acknowledge interrupt
	lda #>(.back-1)
	pha
	lda #<(.back-1)
	pha
	IF DEBUG != 0
	inc $d020
	ENDIF
	jmp (myirqvector)
.back:
	IF DEBUG != 0
	dec $d020
	ENDIF
	pla
	tay
	pla
	tax
	pla
	rti
myirqvector:
	.word irq00

	IF DEBUG != 0
debug_code:
	lda #<.debugmsg
	ldx #>.debugmsg
	jsr .print
	ldy #0
	ldx #>main
	lda #<main
	jsr .prnum
	ldy #0
	ldx #>_EOF_main
	lda #<_EOF_main
	jsr .prnum
	lda #13
	jsr CHROUT
	ldy #0
	ldx #>_EOF_DATA
	lda #<_EOF_DATA
	jsr .prnum
	lda #13
	jsr CHROUT
	ldy #0
	ldx #>ZZZFINAL_END
	lda #<ZZZFINAL_END
	jsr .prnum
	lda #13
	jsr CHROUT
	ldx #0
	ldy #0
.w1:	dec $d020
	inc $d020
	dex
	bne .w1
	dey
	bne .w1
.m1:	lda #$60		;RTS for self-modifying code.
	sta .m1
	jmp .w1
.prnum:
	sec
	jsr $af87
	jsr $af7e
	jsr $bddd
	lda #$00
	ldx #$01
.print:
	sta .l1+1
	stx .l1+2
	ldx #0
.l1:	lda .debugmsg,x
	beq .eos
	jsr CHROUT
	inx
	bcc .l1
.eos:	rts
.debugmsg:
	.byte "DEBUG!\r"
	.byte 18, 150		;Reverse and light red.
	.byte "DO NOT SPREAD THIS VERSION,\r"
	.byte 18, 150, "IT IS FOR TESTING ONLY!\r"
	.byte 154, 0		;Light blue, End Of String
	ENDIF

splash_image:
	;; Copy colour information (skip bitmap data).
	ldx #0
.l1:	lda splash_image_data+$1f40+$0000,x
	sta SCREENPTR+$0000,x
	lda splash_image_data+$1f40+$0100,x
	sta SCREENPTR+$0100,x
	lda splash_image_data+$1f40+$0200,x
	sta SCREENPTR+$0200,x
	lda splash_image_data+$1f40+$0300,x
	sta SCREENPTR+$0300,x
	dex
	bne .l1
	rts


wait_some_time:
	lda #7
	tax
	tay
.l1:	nop
	cld
	nop
	dex
	bne .l1
	dey
	bne .l1
	sec
	sbc #1
	bcs .l1
	rts

lfsr_feedback_loop:
	P_loadi RFEED, feedbt
	P_loadi RFEEDEND, feedbt_end
	P_loadi CONST02, 2
.l1:	P_load RFEED, RFEEDCURR
	nop
	jsr display
	P_load RFEED, RFEEDCURR
	nop
	jsr lfsrE
	P_add CONST02, RFEED
	P_transfer RFEEDEND, RFEEDCURR
	P_sub RFEED, RFEEDCURR
	lda #$00		;Reactivate joystick.
	sta JOYSTICKMASK
	P_branchNZ RFEEDCURR, .l1
	rts

outro:	sei			;Stop interrupts
	ldx #0			;Stop sound.
	stx $d418
	stx $d011		;Disable video.
	stx $d015		;Turn off the sprites.
.l3:	lda outro_text,x		;Copy screen data
	sta SCREENPTR,x
	lda outro_text+$100,x
	sta SCREENPTR+$100,x
	lda outro_text+$200,x
	sta SCREENPTR+$200,x
	lda outro_text+$300,x
	sta SCREENPTR+$300,x
	lda charset,x		;Copy charset
	sta $a000,x
	lda charset+$100,x
	sta $a000+$100,x
	lda charset+$200,x
	sta $a000+$200,x
	dex
	bne .l3
	lda #%00011011		;Default http://unusedino.de/ec64/technical/aay/c64/vic17.htm
	sta $d011
	lda #%00001000		;Default http://unusedino.de/ec64/technical/aay/c64/vic22.htm
	sta $d016
	nop
	jsr wait_some_time
	lda #$1
	sta $d020
	sta $d021
	rts

;;; Get status of fire buttons (port 1 and 2) or space key.
;;; This routine reads a mask from the JOYSTICKMASK and or's it to the
;;; value. Any non-zero value there will disable the joystick
;;; test.
;;; It has a timeout...
;;; Destroys: A
;;; Return: Z=0: Fire pressed
get_fire_status:
	lda $dc00		;gameport 2
	and $dc01		;gameport 1
	and #$10		;Fire
	ora JOYSTICKMASK
	rts

wait_for_fire:
	pha
	lda #$04
	sta .count
	sta .count+1
	sta .count+2
.l1:	jsr get_fire_status
	beq .out
	dec .count
	bne .l1
	dec .count+1
	bne .l1
	dec .count+2
	beq .out
	jmp .l1
.out:
	pla
	rts
.count:
	.word $AAAA
	.word $AAAA
	
main:
	IF DEBUG != 0
	jsr debug_code
	ENDIF
	sei
	lda #$35   ;Turn off the BASIC and KERNAL rom.
        sta $01
	ldx #$ff		;Reset stack.
	txs
	txa
.l2:	sta $00,x
	sta $0100,x
	dex
	cpx #1
	bne .l2
	nop
	jsr initgfx
 	jsr splash_image
	lda #0
	sta JOYSTICKMASK
	jsr wait_for_fire
	lda #$ff
	sta JOYSTICKMASK	;Now disable the joystick test.
	nop
	lda #0			;Disable video.
	sta $d011
	jsr spriteinit
	jsr clearscreen
	jsr init_scroller
	jsr copper_init
	lda #$00
	jsr muzak_data
	lda #<myirq
	sta $FFFE
	lda #>myirq
	sta $FFFF
	lda #$00		;Currently no action.
	sta ACTION
	cli			;Reenable interrupts.
	nop
	jsr lfsr_feedback_loop
	nop
main_quit:
	jsr outro
	nop
	lda #$37		;Turn on ROMs.
	sta $01
	jmp (RESET_VECTOR)
_EOF_main:
	if * > $1000 - $7c - 2
	ERROR No Space!
	endif
	.org $1000 - $7c - 2
muzak_file:
	INCBIN "bananas-01.sid"
muzak_data SET muzak_file + $7C + 2
	INCLUDE "scroller.inc"
	INCLUDE "spritenumberfont.inc"
	INCLUDE "actions.inc"
	INCLUDE "copper_bars.inc"
	if * >= SCREENPTR
	ERROR Overwriting SCREENPTR!
	endif
outro_text:
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 20202020202020205408010e0b20190f1520060f12201701140308090e07
	.HEX 212020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202d2d2d2d2d2d2d2d
	.HEX 2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2020
	.HEX 202054080520420501151419200f06204c090e0501122046050504020103
	.HEX 0b2020202020202020202020202020202020202020202020202020202020
	.HEX 202020530809061420520507091314051213202020202d2d2d2d2d2d2d2d
	.HEX 2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 202020202020202020202020202020500112011201150d202f2054374420
	.HEX 202020202020202020202020202020202020202020202020202020202020
	.HEX 20202020202020202020

_EOF_DATA:
	.org SCREENPTR
	.byte "SCREEN!💾"
	.org BITMAPPTR-2
splash_image_data_file:	
	INCBIN "splash_image.c64"
splash_image_data := splash_image_data_file+2
ZZZFINAL_END:
