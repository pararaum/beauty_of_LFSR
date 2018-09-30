;;; -*- mode: asm -*-

ACTION_SPRITE_BUF = $8100	;Buffer for four sprites.

sprite_x:
	.word $BEEF
sprite_x_speed:
	.byte 0
sprite_y:
	.word 100
sprite_animationmask:
	sed			;Dummy value ($F8)


copy_4_sprites:
	sty .var
	ldy #9
	sta .l1+1
	stx .l1+2
	lda #>ACTION_SPRITE_BUF
	sta .p1+2
	ldx #0
.l1:	lda $0100,x
.p1:	sta ACTION_SPRITE_BUF,x
	dex
	bne .l1
	inc .l1+2
	inc .p1+2
	dey
	bne .l1
	ldy .var
	rts
.var:	nop

action_init:
	lda ACTION		;Enable current action
	ora #$80
	sta ACTION
	and #%00001111
	tay
	lda .animationmask,y
	sta sprite_animationmask
	lda .iny,y
	sta sprite_y
	lda .inspeedx,y
	jsr .set_sprite_x
	ldx .inhigh,y
	lda .inlow,y
	cli			;Danger, enable interrupts.
				;This is OK for the initialisation phase!
	;; Copying takes quite a while and can be interrrupted!
	jsr copy_4_sprites
	rts
.set_sprite_x:
	;; Y contains index
	sta sprite_x_speed
	beq .b2
	bpl .b1
	lda #85
	sta sprite_x
	lda #1
	sta sprite_x+1
	rts
.b1:
	lda #0
	sta sprite_x
	sta sprite_x+1
	rts
.b2:
	;; Y still contains the index, see set_sprite_x
	;; No, we do not provide the MSB.
	lda .inx,y
	sta sprite_x
	lda #0
	sta sprite_x+1
	rts
.inspeedx:
	.byte -1, 0, 1, -1
	.byte 1, 0, -1, -1
	.byte 0, 1, 1, -1
	.byte 0, -1, -1, 1
.iny:
	.byte 230-21,180,75,61
	.byte 129,190,230-21,54
	.byte 100,170,120,200
	.byte 160,170,120,200
.inx:
	.byte 0,200,0,0
	.byte 0,162,0,0
	.byte 89,0,0,0
	.byte 255-24,0,0,0
.inhigh:
	.byte >pacman_1, >jupiter1998, >wheel_1, >shoe_1
	.byte >heartbeat_1, >smoking_girl, >pacman_1, >shoe_1
	.byte >jupiter1998,>laser_1,>wheel_1,>heartbeat_1
	.byte >smoking_girl,>pacman_1,>wheel_1,>heartbeat_1
.inlow:
	.byte <pacman_1, <jupiter1998, <wheel_1, <shoe_1
	.byte <heartbeat_1, <smoking_girl, <pacman_1, <shoe_1
	.byte <jupiter1998,<laser_1,<wheel_1,<heartbeat_1
	.byte <smoking_girl,<pacman_1,<wheel_1,<heartbeat_1
.animationmask:
	;; Remember: 00????00
	.byte 0, %00011100, 0, 0
	.byte 0, %00001100, 0, 0
	.byte %00011100, 0, 0, 0
	.byte %00001100, 0, 0, 0

;;; Move the sprite horizontally
;;; Destroys: A/X
;;; Returns: Carry = 0: sprite movement ok, Carry = 1: end sprite movement
move_sprite:
	ldx #0		     ;This will be sign extended later on
	lda sprite_x_speed	;Load speed
	php			;We later need to know if it was negative
	bpl .b1
	dex		     ;Fill "upper" bits with ones
.b1:	clc		     ;16 Bit addition
	adc sprite_x
	sta sprite_x
	txa
	adc sprite_x+1
	sta sprite_x+1
	plp			;Get speed sign again
	bpl .b2			;Sprite is running to the right
	lda sprite_x+1		;Is the sprite x-position $FF??
	cmp #$ff
	bne .b3
	sec
	rts
.b3:	clc
	rts
.b2:	lda sprite_x+1		;Load sprite position high byte
	cmp #1			;Is it $01??
	bne .b3			;No, so clear carry and return
	lda sprite_x		;Get sprite position low byte
	cmp #$55		;Carry will be set if position is greater than $0155
	rts


action_run:
	lda #$02		;Multicol 1 is red
	sta $d025
	lda #$0E		;Multicol 2 is light blue
	sta $d026
	lda #$01		;Individual is white
	sta $d02b
	sta $d02c
	sta $d02d
	sta $d02e
	lda sprite_x_speed
	bne .move
	jmp .nomove
.move:
	lda sprite_x
	sta $d008
	lda #0
	ldx sprite_x+1
	beq .nomsb
	ora #%00010000
.nomsb:
	sta $d010
	lda sprite_y
	sta $d009
	;;
	lda sprite_x
	lsr
	lsr
	and #%00000011
	clc
	adc #(ACTION_SPRITE_BUF-$8000)/64
	sta SCREENPTR+1024-8 + 4
	tax
	inx
	stx SCREENPTR+1024-8 + 5
	inx
	stx SCREENPTR+1024-8 + 6
	inx
	stx SCREENPTR+1024-8 + 7
	;;
	jsr move_sprite
	bcc .b1
	jsr action_stop
.b1:
	rts
.nomove:
	lda #$00		;No sprite in multicolour mode
	sta $d01c
	lda sprite_x
	sta $d008
	sta $d00c
	clc
	adc #24
	sta $d00a
	sta $d00e
	lda sprite_y
	sta $d009
	sta $d00b
	clc
	adc #21
	sta $d00d
	sta $d00f
	ldx .counter
	inx
	stx .counter
	cpx #255
	bne .s1
	jsr action_stop
.s1:	txa
	lsr
	and sprite_animationmask
	clc
	adc #(ACTION_SPRITE_BUF-$8000)/64
	tax
	stx SCREENPTR+1024-8 + 4
	inx
	stx SCREENPTR+1024-8 + 5
	inx
	stx SCREENPTR+1024-8 + 6
	inx
	stx SCREENPTR+1024-8 + 7
	rts
.counter:
	.byte 0

action_stop:
	lda ACTION		;Load current action
	clc			;Increment by one
	adc #1
	and #%00111111		;Mask out higher bits
	sta ACTION		;Store next action without activating it.
	rts


shoe_1:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$01,$10,$00,$01,$50
	.byte $00,$01,$50,$00,$01,$50,$00,$01
	.byte $50,$00,$01,$50,$00,$01,$50,$00
	.byte $01,$50,$00,$fd,$5c,$03,$ff,$fc
	.byte $0f,$ff,$fc,$3f,$ff,$fc,$2f,$ff
	.byte $fc,$2b,$ea,$a8,$0a,$a0,$a0,$81

shoe_2:
	.byte $00,$00,$00,$00,$01,$10,$00,$01
	.byte $50,$00,$01,$50,$00,$01,$50,$00
	.byte $01,$50,$00,$01,$50,$00,$01,$50
	.byte $00,$01,$50,$00,$01,$50,$00,$01
	.byte $50,$00,$01,$50,$00,$fd,$5c,$03
	.byte $ff,$fc,$0f,$ff,$fc,$3f,$ff,$fc
	.byte $2f,$ff,$fc,$2b,$ea,$a8,$0a,$a0
	.byte $a0,$00,$00,$00,$00,$00,$00,$81

shoe_3:
	.byte $00,$01,$50,$00,$01,$50,$00,$01
	.byte $50,$00,$01,$50,$00,$01,$50,$00
	.byte $01,$50,$00,$01,$50,$00,$01,$50
	.byte $00,$01,$50,$00,$fd,$5c,$03,$ff
	.byte $fc,$0f,$ff,$fc,$3f,$ff,$fc,$2f
	.byte $ff,$fc,$2b,$ea,$a8,$0a,$a0,$a0
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

shoe_4:
	.byte $00,$00,$00,$00,$01,$10,$00,$01
	.byte $50,$00,$01,$50,$00,$01,$50,$00
	.byte $01,$50,$00,$01,$50,$00,$01,$50
	.byte $00,$01,$50,$00,$01,$50,$00,$01
	.byte $50,$00,$01,$50,$00,$fd,$5c,$03
	.byte $ff,$fc,$0f,$ff,$fc,$3f,$ff,$fc
	.byte $2f,$ff,$fc,$2b,$ea,$a8,$0a,$a0
	.byte $a0,$00,$00,$00,$00,$00,$00,$81

laser_1:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$f0,$00,$03,$c0,$00
	.byte $0f,$00,$00,$3c,$00,$00,$f0,$00
	.byte $00,$f0,$00,$00,$c0,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

laser_2:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$30,$00,$00,$30,$00
	.byte $00,$30,$00,$00,$30,$00,$00,$30
	.byte $00,$00,$30,$00,$00,$30,$00,$00
	.byte $f0,$00,$00,$f0,$00,$00,$f0,$00
	.byte $00,$f0,$00,$00,$f0,$00,$00,$f0
	.byte $00,$03,$f0,$00,$03,$f0,$00,$81

laser_3:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$0f,$00,$00,$03,$c0
	.byte $00,$00,$f0,$00,$00,$3c,$00,$00
	.byte $0f,$00,$00,$0f,$00,$00,$03,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

laser_4:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$0c,$00,$00,$0c,$00
	.byte $00,$0c,$00,$00,$0c,$00,$00,$0c
	.byte $00,$00,$0c,$00,$00,$0c,$00,$00
	.byte $0f,$00,$00,$0f,$00,$00,$0f,$00
	.byte $00,$0f,$00,$00,$0f,$00,$00,$0f
	.byte $00,$00,$0f,$c0,$00,$0f,$c0,$81

	
heartbeat_1:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$01,$00,$40,$05,$41,$50,$14
	.byte $55,$14,$14,$14,$14,$10,$00,$04
	.byte $10,$00,$04,$10,$00,$04,$14,$00
	.byte $14,$14,$00,$14,$05,$00,$50,$05
	.byte $00,$50,$01,$41,$40,$00,$41,$00
	.byte $00,$55,$00,$00,$14,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

heartbeat_2:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$01,$41,$40,$04,$14,$10
	.byte $04,$14,$10,$04,$00,$10,$01,$00
	.byte $40,$01,$00,$40,$00,$41,$00,$00
	.byte $41,$00,$00,$14,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

heartbeat_3:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$41,$00
	.byte $01,$14,$40,$01,$00,$40,$00,$41
	.byte $00,$00,$41,$00,$00,$41,$00,$00
	.byte $14,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81

heartbeat_4:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$01,$41,$40,$04,$14,$10
	.byte $04,$14,$10,$04,$00,$10,$01,$00
	.byte $40,$01,$00,$40,$00,$41,$00,$00
	.byte $41,$00,$00,$14,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$81
	

	INCLUDE "pacman.inc"

wheel_1:
	.byte $00,$00,$00,$03,$ff,$00,$0c,$01
	.byte $c0,$0c,$01,$c0,$30,$01,$30,$30
	.byte $01,$30,$d0,$04,$0c,$d4,$04,$0c
	.byte $c5,$04,$0c,$c1,$44,$0c,$c0,$50
	.byte $0c,$c0,$14,$0c,$c0,$45,$0c,$c0
	.byte $41,$4c,$c0,$40,$5c,$c0,$40,$1c
	.byte $31,$00,$30,$31,$00,$30,$0d,$00
	.byte $c0,$0d,$00,$c0,$03,$ff,$00,$81


wheel_2:
	.byte $00,$00,$00,$03,$ff,$00,$0c,$00
	.byte $c0,$0c,$00,$c0,$34,$00,$70,$34
	.byte $00,$70,$c1,$01,$0c,$c1,$01,$0c
	.byte $c0,$44,$0c,$c0,$44,$0c,$c0,$10
	.byte $0c,$c0,$10,$0c,$c0,$44,$0c,$c0
	.byte $44,$0c,$c1,$01,$0c,$c1,$01,$0c
	.byte $34,$00,$70,$34,$00,$70,$0c,$00
	.byte $c0,$0c,$00,$c0,$03,$ff,$00,$81

wheel_3:
	.byte $00,$00,$00,$03,$ff,$00,$0d,$00
	.byte $c0,$0d,$00,$c0,$31,$00,$30,$31
	.byte $00,$30,$c0,$40,$1c,$c0,$40,$5c
	.byte $c0,$41,$4c,$c0,$45,$0c,$c0,$14
	.byte $0c,$c0,$50,$0c,$c1,$44,$0c,$c5
	.byte $04,$0c,$d4,$04,$0c,$d0,$04,$0c
	.byte $30,$01,$30,$30,$01,$30,$0c,$01
	.byte $c0,$0c,$01,$c0,$03,$ff,$00,$81

wheel_4:
	.byte $00,$00,$00,$03,$ff,$00,$0c,$10
	.byte $c0,$0c,$10,$c0,$30,$10,$30,$30
	.byte $10,$30,$c0,$10,$0c,$c0,$10,$0c
	.byte $c0,$10,$0c,$c0,$10,$0c,$d5,$55
	.byte $5c,$d5,$55,$5c,$c0,$10,$0c,$c0
	.byte $10,$0c,$c0,$10,$0c,$c0,$10,$0c
	.byte $30,$10,$30,$30,$10,$30,$0c,$10
	.byte $c0,$0c,$10,$c0,$03,$ff,$00,$81

smoking_girl:
	.byte $00, $00, $00 
	.byte $00, $00, $04 
	.byte $00, $00, $0F 
	.byte $00, $00, $3F 
	.byte $00, $00, $7F 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $03, $FF 
	.byte $00, $01, $FF 
	.byte $00, $07, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $0C, $1E, $7F 
	.byte $1F, $3E, $7F 
	.byte $07, $F0, $FF 
	.byte $00, $80, $FF 
	.byte $00, $01, $FF 
	.byte $00, $01, $FF 
	.byte $81
	.byte $00, $01, $00 
	.byte $00, $01, $80 
	.byte $C0, $00, $80 
	.byte $E0, $00, $80 
	.byte $E0, $01, $00 
	.byte $E0, $04, $00 
	.byte $F0, $08, $00 
	.byte $E0, $F0, $00 
	.byte $F0, $E0, $00 
	.byte $FC, $F0, $00 
	.byte $FC, $E0, $00 
	.byte $FC, $E0, $00 
	.byte $FE, $C0, $00 
	.byte $FF, $E0, $00 
	.byte $EF, $C0, $00 
	.byte $FF, $80, $00 
	.byte $FF, $00, $00 
	.byte $FF, $80, $00 
	.byte $FD, $80, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $81
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $7F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $1F 
	.byte $00, $00, $1F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $1F 
	.byte $81
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FE, $00, $00 
	.byte $81

	.byte $00, $00, $00 
	.byte $00, $00, $04 
	.byte $00, $00, $0F 
	.byte $00, $00, $3F 
	.byte $00, $00, $7F 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $03, $FF 
	.byte $00, $01, $FF 
	.byte $00, $07, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $0C, $1E, $7F 
	.byte $1F, $3E, $7F 
	.byte $07, $F0, $FF 
	.byte $00, $80, $FF 
	.byte $00, $01, $FF 
	.byte $00, $01, $FF 
	.byte $81
	.byte $00, $00, $80 
	.byte $00, $01, $00 
	.byte $C0, $01, $00 
	.byte $E0, $01, $00 
	.byte $E0, $01, $00 
	.byte $E0, $04, $00 
	.byte $F0, $08, $00 
	.byte $E0, $F0, $00 
	.byte $F0, $E0, $00 
	.byte $FC, $F0, $00 
	.byte $FC, $E0, $00 
	.byte $FC, $E0, $00 
	.byte $FE, $C0, $00 
	.byte $FF, $E0, $00 
	.byte $EF, $C0, $00 
	.byte $FF, $80, $00 
	.byte $FF, $00, $00 
	.byte $FF, $80, $00 
	.byte $FD, $80, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $81
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $7F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $1F 
	.byte $00, $00, $1F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $1F 
	.byte $81
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FE, $00, $00 
	.byte $81

	.byte $00, $00, $00 
	.byte $00, $00, $04 
	.byte $00, $00, $0F 
	.byte $00, $00, $3F 
	.byte $00, $00, $7F 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $03, $FF 
	.byte $00, $01, $FF 
	.byte $00, $07, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $0C, $1E, $7F 
	.byte $1F, $3E, $7F 
	.byte $07, $F0, $FF 
	.byte $00, $80, $FF 
	.byte $00, $01, $FF 
	.byte $00, $01, $FF 
	.byte $81
	.byte $00, $03, $00 
	.byte $00, $04, $00 
	.byte $C0, $04, $00 
	.byte $E0, $02, $00 
	.byte $E0, $01, $00 
	.byte $E0, $04, $00 
	.byte $F0, $08, $00 
	.byte $E0, $F0, $00 
	.byte $F0, $E0, $00 
	.byte $FC, $F0, $00 
	.byte $FC, $E0, $00 
	.byte $FC, $E0, $00 
	.byte $FE, $C0, $00 
	.byte $FF, $E0, $00 
	.byte $EF, $C0, $00 
	.byte $FF, $80, $00 
	.byte $FF, $00, $00 
	.byte $FF, $80, $00 
	.byte $FD, $80, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $81
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $7F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $1F 
	.byte $00, $00, $1F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $1F 
	.byte $81
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FE, $00, $00 
	.byte $81

	.byte $00, $00, $00 
	.byte $00, $00, $04 
	.byte $00, $00, $0F 
	.byte $00, $00, $3F 
	.byte $00, $00, $7F 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $03, $FF 
	.byte $00, $01, $FF 
	.byte $00, $07, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $00, $0F, $FF 
	.byte $0C, $1E, $7F 
	.byte $1F, $3E, $7F 
	.byte $07, $F0, $FF 
	.byte $00, $80, $FF 
	.byte $00, $01, $FF 
	.byte $00, $01, $FF 
	.byte $81
	.byte $00, $01, $00 
	.byte $00, $01, $00 
	.byte $C0, $02, $00 
	.byte $E0, $02, $00 
	.byte $E0, $01, $00 
	.byte $E0, $04, $00 
	.byte $F0, $08, $00 
	.byte $E0, $F0, $00 
	.byte $F0, $E0, $00 
	.byte $FC, $F0, $00 
	.byte $FC, $E0, $00 
	.byte $FC, $E0, $00 
	.byte $FE, $C0, $00 
	.byte $FF, $E0, $00 
	.byte $EF, $C0, $00 
	.byte $FF, $80, $00 
	.byte $FF, $00, $00 
	.byte $FF, $80, $00 
	.byte $FD, $80, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $81
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $FF 
	.byte $00, $00, $7F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $3F 
	.byte $00, $00, $1F 
	.byte $00, $00, $1F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $0F 
	.byte $00, $00, $1F 
	.byte $81
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FE, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $F8, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FC, $00, $00 
	.byte $FE, $00, $00 
	.byte $81

jupiter1998:	
	include "jupiter1998.inc"
