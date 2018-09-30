;;; -*- mode: asm -*-

COPPER_BUFFER_SIZE := 32
COPPER_MAX_LINES := 18
COPPER_BAR_SPEED := 4
	
copper_init:
	lda #$3
	ldx #0
	stx copper_ptr
.l1:	sta copper_buffer,x
	inx
	cpx #COPPER_BUFFER_SIZE
	bne .l1
	rts

copper_move:
	lda copper_ptr
	clc
	pha
	adc #1
	pha
	adc #1
	pha
	ldy #0
	pla
	jsr .store_bar
	pla
	jsr .store_bar
	pla
	jsr .store_bar
	ldx copper_ptr
	inx
	lda copper_colours,x
	bpl .s1
	ldx #0
.s1:	stx copper_ptr
	rts
.store_bar:
	tax
	lda copper_colours,x
	sta copper_buffer,y
	inx
	iny
	lda copper_colours,x
	sta copper_buffer,y
	inx
	iny
	lda copper_colours,x
	sta copper_buffer,y
	inx
	iny
	lda copper_colours,x
	sta copper_buffer,y
	inx
	iny
	lda copper_colours,x
	sta copper_buffer,y
	inx
	iny
	lda copper_colours,x
	sta copper_buffer,y
	iny
	rts
	
copper_ptr:	.byte 0

	.align 256
copper_run:
	ldx #0
	lda copper_buffer,x	;4
	ldy $d012
.l1:	cpy $d012		;4
	beq .l1			;2/3(taken)
	ldy #0			;2, do not bork that value!
	EOR ($EA,x)		;6
	EOR ($EA,x)		;6
	EOR ($EA,x)		;6
	.if debug = 0
	inc .dummy
	.endif
	inc .dummy		;6
	inc .dummy		;6
	inc .dummy		;6
	;; ;46-49 cycles per raster if 8 sprites are displayed
.l2:
	.if 0
	sta $d021,y		;5
	sta $d021		;4
	sta $d021		;4
	sta $d021		;4
	sta $d021		;4
	sta $d021		;4
	sta $d021		;4
	sta $d021		;4
	.else
	sta $d027,y		;5
	sta $d028		;4
	sta $d029		;4
	sta $d02a		;4
	sta $d02b		;4
	sta $d02c		;4
	sta $d02d		;4
	sta $d02e		;4
	.endif
	inx			;2
	lda copper_buffer,x	;4
	cpx #COPPER_MAX_LINES	;2
	bne .l2			;2/3(taken)
	dec .skip
	bpl .out
	jsr copper_move
	lda #COPPER_BAR_SPEED
	sta .skip
.out:
	.if 1
	lda #4
	sta $d021
	.endif
	rts
.skip:	.byte 0
.dummy: .byte 0

	.align 256
copper_buffer:
	.ds COPPER_BUFFER_SIZE
copper_colours:
	;; 	.byte 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0
	.BYTE 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
	.BYTE 3,3,3,5,5,13,13,7,7,7,13,13,5,5,3,3
	.BYTE 3,3,7,7,8,8,9,9,9,8,8,7,7,3,3,3
	.BYTE 3,5,5,13,13,1,1,1,13,13,5,5
	.byte 0,0
	.byte 11,12,15,1,15,12,11,0
	.byte 0,11,11,12,12,15,15,1,1,15,15,12,12,11,11,0,0
	.byte 6,6,14,14,1,1,14,14,6,6,3
copper_colours_end:
	.byte $83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83

