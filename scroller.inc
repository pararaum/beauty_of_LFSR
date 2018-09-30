;;; -*- mode: asm -*-

SCRSPRBUFLINE := 1		;Top line in the sprite buffer

SPROFF := SCRSPRBUFLINE * 3
;;; Buffer for storing character data.
SCRCHRBUF:
	.ds 8

init_scroller:
	lda #<scroll_text
	sta SCRTEXTPTR
	lda #>scroll_text
	sta SCRTEXTPTR+1
	lda #$00
	ldx #$07
.l1:	sta SCRCHRBUF,x
	dex
	bpl .l1
	lda #9
	sta SCROLLCOUNTER
	rts

perform_scroller:
	dec SCROLLCOUNTER
	bpl .noscl		;no new scroller character
	lda #9
	sta SCROLLCOUNTER
	ldx #0
	lda (SCRTEXTPTR,x)
	bne .notextend
	jsr init_scroller
	lda #' '
.notextend:
	tax
	;; 	ldx #'|'
	lda tab_petscii2screencode,x
	inc SCRTEXTPTR
	bne .nocarry
	inc SCRTEXTPTR+1
.nocarry:
	sta .calc
	lda #0
	sta .calc+1
	asl .calc
	rol .calc+1
	asl .calc
	rol .calc+1
	asl .calc
	rol .calc+1
	clc
	lda .calc
	adc #<charset
	sta .l2+1
	lda .calc+1
	adc #>charset
	sta .l2+2
	ldy #7
.l2	lda charset,y
	sta SCRCHRBUF,y
	dey
	bpl .l2
.noscl:	ldy #0
	ldx #0
.l1:	txa
	pha
	tya
	tax
	asl SCRCHRBUF,x
	pla
	tax
	rol SCRSPRDATA+SPROFF+$1c2,x		;7
	rol SCRSPRDATA+SPROFF+$1c1,x
	rol SCRSPRDATA+SPROFF+$1c0,x
	rol SCRSPRDATA+SPROFF+$182,x		;6
	rol SCRSPRDATA+SPROFF+$181,x
	rol SCRSPRDATA+SPROFF+$180,x
	rol SCRSPRDATA+SPROFF+$142,x		;5
	rol SCRSPRDATA+SPROFF+$141,x
	rol SCRSPRDATA+SPROFF+$140,x
	rol SCRSPRDATA+SPROFF+$102,x		;4
	rol SCRSPRDATA+SPROFF+$101,x
	rol SCRSPRDATA+SPROFF+$100,x
	rol SCRSPRDATA+SPROFF+$0c2,x		;3
	rol SCRSPRDATA+SPROFF+$0c1,x
	rol SCRSPRDATA+SPROFF+$0c0,x
	rol SCRSPRDATA+SPROFF+$082,x		;2
	rol SCRSPRDATA+SPROFF+$081,x
	rol SCRSPRDATA+SPROFF+$080,x
	rol SCRSPRDATA+SPROFF+$042,x		;1
	rol SCRSPRDATA+SPROFF+$041,x
	rol SCRSPRDATA+SPROFF+$040,x
	rol SCRSPRDATA+SPROFF+$002,x		;0
	rol SCRSPRDATA+SPROFF+$001,x
	rol SCRSPRDATA+SPROFF+$000,x
	txa
	clc
	adc #3
	tax
	iny
	cpy #8
	bne .l1
	rts
.calc:	.word 0

tab_petscii2screencode:
	;; Based on
	;; http://codebase64.org/doku.php?id=base:petscii_to_screencode
	;; But replaces PETSCII with ASCII
										;PETSCII RANGE
	.byte $80,$81,$82,$83,$84,$85,$86,$87, $88,$89,$8a,$8b,$8c,$8d,$8e,$8f	;$00-...
	.byte $90,$91,$92,$93,$94,$95,$96,$97, $98,$99,$9a,$9b,$9c,$9d,$9e,$9f	;...-$1f
	.byte $20,$21,$22,$23,$24,$25,$26,$27, $28,$29,$2a,$2b,$2c,$2d,$2e,$2f	;$20-...
 	.byte $30,$31,$32,$33,$34,$35,$36,$37, $38,$39,$3a,$3b,$3c,$3d,$3e,$3f	;...-$3f
	.byte $40,$41,$42,$43,$44,$45,$46,$47, $48,$49,$4a,$4b,$4c,$4d,$4e,$4f	;$60-...
 	.byte $50,$51,$52,$53,$54,$55,$56,$57, $58,$59,$5a,$1b,$1c,$1d,$1e,$1f	;...-$7f
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$40-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$5b,$5c,$5d,$5e,$5f	;...-$5f
	.byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7, $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf	;$80-...
 	.byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7, $d8,$d9,$da,$db,$dc,$dd,$de,$df	;...-$9f
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$a0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$7f	;...-$bf
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$c0-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$1b,$1c,$1d,$1e,$1f	;...-$df
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$e0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$5e	;...-$ff

charset_file:
	;; Trueman font [https://csdb.dk/release/?id=120855] by Flex / Artline Design.
	incbin "font.pbm"
charset = charset_file+11	;Skip PBM header

scroll_text:
	.byte "  The Beauty of LFSR!   Can you enjoy the beautiful neverending Art of Linear Feedback Shift Registers?"
	.byte " If you look very carefully you may see strange things creeping in your peripherial vision."
	.byte " The hex number on top will show you the current feedback term producing the nice blocky pictures. The feedback terms have been selected to have the maximum period available."
	.byte " For a list have a look at Koopman's webpage: http://users.ece.cmu.edu/~koopman/lfsr/ or read the scroller till the end."
	.byte " This is a simple example of the multicolour mode. Many strange things can happen, especially in 2018 when this release for the Evoke 2018 was prepared."
	.byte " After the first time the screen has filled you may press Space or the Fire button to exit this demo..."
	.byte "  Credits: Code by Pararaum/T7D, Muzak by fieserWolf/The Abyss Connection, Scroller Font by Flex/Artline Design, Hex Digit Font ripped from Grafx2, Jupiter animation based on a gif by SexyEeveeQuice."
	.byte "    |||  ` 2018 by T7D |||    "
	.byte " Greetings (in no particular order) go to: Jack Beatmaster, Meepster, Roz, Strobo, zake, Paul Holt, Tez, Sebastian L., Kylearan/Cluster."
	.byte "                    "
	.byte "Due to popular demand here are all the LFSR feedback terms:"
	.byte "$8016, $801c, $801f, $8029, $805e, $806b, $8097, $809e, "
	.byte "$80a7, $80ae, $80cb, $80d0, $80d6, $80df, $80e3, $810a, "
	.byte "$810c, $8112, $8117, $812e, $8136, $8142, $8148, $8150, "
	.byte "$8172, $818e, $81a5, $81b4, $81b8, $81c3, $81c6, $81cf, "
	.byte "$81d1, $81ee, $81fc, $8214, $822b, $8233, $8241, $8244, "
	.byte "$8248, $825f, $8260, $8299, $82a3, $82b4, $82c3, $82e1, "
	.byte "$82ee, $82f5, $8320, $8325, $8329, $8345, $8361, $83b5, "
	.byte "$83b6, $83bc, $83c1, $83f8, $8406, $8430, $845f, $846a, "
	.byte "$846f, $8471, $8478, $847d, $849c, $84be, $84c5, $84d2, "
	.byte "$84d7, $84e1, $84e2, $84f3, $84f9, $853e, $8540, $855d, "
	.byte "$8562, $8580, $8589, $858a, $85a8, $85ae, $85e6, $85e9, "
	.byte "$85f2, $8607, $860e, $8610, $8634, $8638, $863d, $8646, "
	.byte "$864a, $8651, $8657, $8679.  "
	.byte "That is all, folks!"
	.byte "                  ", 0

