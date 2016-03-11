
	processor 6502
	include vcs.h
	include macro.h

;-------------------------Constants Below---------------------------------



;-------------------------COLOR CONSTANTS (NTSC)--------------------------

GRAY		=	$00
GOLD		=	$10
ORANGE		=	$20
BURNTORANGE	=	$30
RED		=	$40
PURPLE		=	$50
PURPLEBLUE	=	$60
BLUE		=	$70
BLUE2		=	$80
LIGHTBLUE	=	$90
TURQUOISE	=	$A0
GREEN		=	$B0
BROWNGREEN	=	$C0
TANGREEN	=	$D0
TAN		=	$E0
BROWN		=	$F0

;--------------------------TIA CONSTANTS----------------------------------

	;--NUSIZx CONSTANTS
	;	player:
ONECOPYNORMAL		=	$00
TWOCOPIESCLOSE		=	$01
TWOCOPIESMED		=	$02
THREECOPIESCLOSE	=	$03
TWOCOPIESWIDE		=	$04
ONECOPYDOUBLE		=	$05
THREECOPIESMED		=	$06
ONECOPYQUAD		=	$07
	;	missile:
SINGLEWIDTHMISSILE	=	$00
DOUBLEWIDTHMISSILE	=	$10
QUADWIDTHMISSILE	=	$20
OCTWIDTHMISSILE		=	$30

	;---CTRLPF CONSTANTS
	;	playfield:
REFLECTEDPF		=	%00000001
SCOREPF			=	%00000010
PRIORITYPF		=	%00000100
	;	ball:
SINGLEWIDTHBALL		=	SINGLEWIDTHMISSILE
DOUBLEWIDTHBALL		=	DOUBLEWIDTHMISSILE
QUADWIDTHBALL		=	QUADWIDTHMISSILE
OCTWIDTHBALL		=	OCTWIDTHMISSILE

	;---HMxx CONSTANTS
LEFTSEVEN		=	$70
LEFTSIX			=	$60
LEFTFIVE		=	$50
LEFTFOUR		=	$40
LEFTTHREE		=	$30
LEFTTWO			=	$20
LEFTONE			=	$10
NOMOVEMENT		=	$00
RIGHTONE		=	$F0
RIGHTTWO		=	$E0
RIGHTTHREE		=	$D0
RIGHTFOUR		=	$C0
RIGHTFIVE		=	$B0
RIGHTSIX		=	$A0
RIGHTSEVEN		=	$90
RIGHTEIGHT		=	$80

	;---AUDCx CONSTANTS (P Slocum's naming convention)
SAWSOUND		=	1
ENGINESOUND		=	3
SQUARESOUND		=	4
BASSSOUND		=	6
PITFALLSOUND		=	7
NOISESOUND		=	8
LEADSOUND		=	12
BUZZSOUND		=	15

	;---SWCHA CONSTANTS (JOYSTICK)
J0RIGHT		=	%10000000
J0LEFT		=	%01000000
J0DOWN		=	%00100000
J0UP		=	%00010000
J1RIGHT		=	%00001000
J1LEFT		=	%00000100
J1DOWN		=	%00000010
J1UP		=	%00000001

	;---SWCHB CONSTANTS (CONSOLE SWITCHES)
P1DIFF		=	%10000000
P0DIFF		=	%01000000
BWCOLOR		=	%00001000
SELECT		=	%00000010
RESET		=	%00000001

;-------------------------End Constants-----------------------------------

;-----------------------------Macros--------------------------------------

	MAC FILLER
		REPEAT {1}
		.byte {2}
		REPEND
	ENDM
	


;------------------------------Variables----------------------------------

	SEG.U Variables
   	org $80
    
bank_shadow ds 1
mode_shadow ds 1

Counter ds 2

music_pos ds 1
music_temp ds 1

menu_line ds 1
menu_pos  ds 1
menu_temp ds 1

bg_color    ds 1
text_color  ds 1

fade_color  ds 1
info_page   ds 1

state       ds 1 ; 00 - rom picker, 01 - fade out, 02 - copy ram code and execute

TextLineCounter ds 1

TextBlockPointer ds 2

Char1Ptr ds 2
Char2Ptr ds 2
Char3Ptr ds 2
Char4Ptr ds 2
Char5Ptr ds 2
Char6Ptr ds 2
Char7Ptr ds 2
Char8Ptr ds 2
Char9Ptr ds 2
Char10Ptr ds 2
Char11Ptr ds 2
Char12Ptr ds 2


Temp ds 1


;-------------------------End Variables-----------------------------------
	
	SEG Bank0
	org $F000
  byte 0,0,0,0,"TDI!"
  
;--- dummy
  org $f008
;---
  
Start
	CLEAN_START

;--any initial setup

	lda #GRAY+14
	sta COLUP0
	sta COLUP1

	lda #0
	sta COLUBK

        lda #$70
        sta PF0
        lda #$00
        sta PF1
        lda #0
        sta PF2
        lda #1
        sta CTRLPF

  lda #$92
  sta bg_color
  lda #$0e
  sta text_color

	lda #<TextBlock1
	sta TextBlockPointer
	lda #>TextBlock1
	sta TextBlockPointer+1

;-------------------------------------------------------------------------
;--------------GAME MAIN LOOP---------------------------------------------
;-------------------------------------------------------------------------

MainGameLoop

	jsr VBLANKRoutine
	jsr KernelRoutine
	jsr OverscanRoutine
	jmp MainGameLoop

;-------------------------------------------------------------------------
;-------------------VBLANK Routine----------------------------------------
;-------------------------------------------------------------------------

VBLANKRoutine
	lda #%00000111
VSYNCLoop
	sta WSYNC
	sta VSYNC
	lsr
	bcs VSYNCLoop

	lda #(36*76)>>6
	sta TIM64T


	dec Counter
  bne nohiclick
  dec Counter+1
nohiclick:

	lda #1
	sta VDELP0
	sta VDELP1

	lda #THREECOPIESMED
	sta NUSIZ0
	sta NUSIZ1

	lda #0
	sta TextLineCounter

	jsr SetTextPointersSubroutine


	lda #$15
	ldx #0
	jsr PositionASpriteSubroutine
	lda #$25
	ldx #1
	jsr PositionASpriteSubroutine


WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd

	sta WSYNC
	sta VBLANK	;turn off VBLANK - it was turned on by overscan

	rts

;-------------------------------------------------------------------------
;----------------------Kernel Routine-------------------------------------
;-------------------------------------------------------------------------

MENU_HEIGHT = 7

KernelRoutine

  lda state
  cmp #2
  bmi titleline
  lda #0
  jmp title2
titleline:
	lda Counter
  lsr
  lsr
  
  and #$0f
  ora #$20
  
title2:
	sta COLUP0
	sta COLUP1

  lda #MENU_HEIGHT
  sta menu_line
  

        ; start timer for display period
        lda     #(191*76)>>6
        sta     TIM64T
  
  lda TextBlockPointer
  pha
	lda #<TitleBlock
	sta TextBlockPointer
  
  lda TextBlockPointer+1
  pha
	lda #>TitleBlock
	sta TextBlockPointer+1
  jsr SetTextPointersSubroutine
  jsr DrawLineOfTextSubroutine
  
;  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC

	lda text_color
	sta COLUP0
	sta COLUP1
	lda bg_color
	sta COLUBK
  lda menu_pos
  sec
  sbc #MENU_HEIGHT/2
  bpl no_empty_on_top
  clc
  eor #$ff
  sta menu_temp
  
	lda #<EmptyBlock
	sta TextBlockPointer
  
	lda #>EmptyBlock
	sta TextBlockPointer+1
  jsr SetTextPointersSubroutine

one_more_on_top:
  jsr DrawLineOfTextSubroutine
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  dec menu_line
  dec menu_temp
  ldx menu_temp
  cpx #$ff
  bne one_more_on_top
no_empty_on_top:
  lda menu_pos
  cmp #(MENU_HEIGHT/2)
  bmi not_empty_top
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
not_empty_top:
  pla
	sta TextBlockPointer+1
  pla
	sta TextBlockPointer

  jsr SetTextPointersSubroutine
  
DrawAnotherLineOfText

  lda menu_line
  cmp #(MENU_HEIGHT/2)+1
  bne no_highlight
  
	lda #$2c
	sta COLUP0
	sta COLUP1

  
no_highlight:
	jsr DrawLineOfTextSubroutine
	lda text_color
	sta COLUP0
	sta COLUP1

;  ldy #12
;	lda (TextBlockPointer),Y
;	cmp #$FF
;	beq menu_ends
  

	lda TextLineCounter
	clc
	adc #16
	sta TextLineCounter
	tay

  dec menu_line
  beq menu_ends

  lda (TextBlockPointer),Y
	cmp #$FF
	beq menu_ends

	jsr SetTextPointersSubroutine

	jmp DrawAnotherLineOfText

menu_ends:
  lda TextBlockPointer
  pha
  lda TextBlockPointer+1
  pha

  lda menu_line
;  inc menu_line
  cmp #$00
  bne empty_lines
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  jmp no_empty_lines  

empty_lines:
  
  lda #$00
	sta TextLineCounter

	lda #<EmptyBlock
	sta TextBlockPointer
  
	lda #>EmptyBlock
	sta TextBlockPointer+1
  jsr SetTextPointersSubroutine
  sta WSYNC
;dec menu_
one_more:
  jsr DrawLineOfTextSubroutine
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC

  dec menu_line
  bne one_more
no_empty_lines:
	lda #0
	sta COLUBK
  
;  sta WSYNC
;  sta WSYNC
;  sta WSYNC
  sta WSYNC
	lda #0
	sta COLUP0
	sta COLUP1

  lda #$00
	sta TextLineCounter

	lda #<(FreeBlock+1)
	sta TextBlockPointer
  
	lda #>(FreeBlock+1)
	sta TextBlockPointer+1
  jsr SetTextPointersSubroutine
  sta WSYNC
	lda text_color
	sta COLUBK


  jsr DrawLineOfTextSubroutine
  sta WSYNC
;  sta WSYNC

   
	lda fade_color
  and #$0f
	sta COLUP0
	sta COLUP1
	lda #0
	sta COLUBK

  lda info_page
  sta TextLineCounter


	lda #<GreetingsBlock
	sta TextBlockPointer
  
	lda #>GreetingsBlock
	sta TextBlockPointer+1
  jsr SetTextPointersSubroutine
  jsr DrawLineOfTextSubroutine
  
	lda TextLineCounter
	clc
	adc #12
	sta TextLineCounter

  jsr SetTextPointersSubroutine
  jsr DrawLineOfTextSubroutine
  
  pla
	sta TextBlockPointer+1
  pla
	sta TextBlockPointer
  jsr SetTextPointersSubroutine

WaitForKernelEnd
	lda INTIM
	bne WaitForKernelEnd

	rts

;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------

OverscanRoutine




	lda #2
	sta WSYNC
	sta VBLANK	;turn on VBLANK
	lda  #34
	sta  TIM64T
  jsr play_music

;------
  lda Counter
  and #$03
  bne no_fade
  
  lda state
  beq work_state
  lda text_color
  and #$0f
  beq fade_bg
  dec text_color
fade_bg:
  lda bg_color
  and #$0f
  beq work_state
  dec bg_color
  lda bg_color
  and #$0f
  bne work_state
  sta bg_color
work_state:
  lda text_color
  bne infoline
  lda #2
  sta state
infoline:  
  lda fade_color
  and #$80
  cmp #$80
  beq fade_out

  lda fade_color
  cmp #$0f
  beq fade_in_end

  inc fade_color
  jmp no_fade
fade_in_end:
  lda #$8f
  sta fade_color
  jmp no_fade
fade_out:
  lda fade_color
  cmp #$80
  beq fade_out_end
  dec fade_color
  jmp no_fade
fade_out_end:
  lda state
  bne no_fade
  
  lda #0
  sta fade_color
  lda info_page
  clc
  adc #24
  cmp #(10*24)
  bne store_it
  lda #0
store_it:
  sta info_page
no_fade:

  lda state
  bne DoneWithTrigger

  lda Counter
  and #$0f
  bne checkFire
  lda SWCHA
  bpl selectRight
  asl
  bpl selectLeft
  asl
  bpl selectDown
  asl
  bpl selectUp
  jmp checkFire
selectRight:
  lda menu_pos
  clc
  adc #MENU_HEIGHT
  cmp $f80d
  bmi sr2
  lda $f80d
sr2:
  sta menu_pos
  jmp calcOffset
selectLeft:
  lda menu_pos
  sec
  sbc #MENU_HEIGHT
  bpl sl2
  lda #0
sl2:
  sta menu_pos
  jmp calcOffset
selectDown:
  lda menu_pos
  cmp $f80d
  beq checkFire
  inc menu_pos
  jmp calcOffset
selectUp:
    lda menu_pos
    beq checkFire
   dec menu_pos
;   jmp checkFire
calcOffset:
	lda #<TextBlock1
	sta TextBlockPointer
	lda #>TextBlock1
	sta TextBlockPointer+1
  lda menu_pos
  sec
  sbc #MENU_HEIGHT/2)+1
  bmi no_offset_change
  tax
offset_change:
  dex
  cpx #$ff
  beq no_offset_change
  lda TextBlockPointer
  clc
  adc #16
  sta TextBlockPointer
  bcc offset_change
  inc TextBlockPointer+1
  jmp offset_change  
no_offset_change:
   
checkFire:
	bit INPT4
	bmi TriggerNotHit
  ;-----------
  lda #1
  sta state
  ;-----------
	jmp DoneWithTrigger
TriggerNotHit
DoneWithTrigger
  lda state
  cmp #2
  bne not_execute
  
  ldx #romCode_size
romcopy:
  lda romCode,x
  sta $00b0,x
  dex
  cpx #$ff
  bne romcopy
  jmp $00b0 
    
  
not_execute:

WaitForOverscanEnd
	lda INTIM
	bne WaitForOverscanEnd
	rts

;-------------------------------------------------------------------------
;----------------------------End Main Routines----------------------------
;-------------------------------------------------------------------------


;*************************************************************************

;-------------------------------------------------------------------------
;----------------------Begin Subroutines----------------------------------
;-------------------------------------------------------------------------

	align 256

PositionASpriteSubroutine 
	sec
        sta HMCLR
	sta WSYNC                    ;begin line 1
DivideLoop
	sbc #15
	bcs DivideLoop			;+4/5	 4/ 9.../54

	eor #7				;+2	 6/11.../56
	asl
	asl
	asl
	asl				;+8	14/19.../64

	sta.wx HMP0,X			;+5	19/24.../69
        sta RESP0,X     		;+4     23/28/33/38/43/48/53/58/63/68/73
        sta WSYNC      			;+3      0      begin line 2
        sta HMOVE       		;+3
Ret
        rts             ;+6      9

;-------------------------------------------------------------------------

SetTextPointersSubroutine
	ldy TextLineCounter
	ldx #0
SetCharPtrsLoop
	lda (TextBlockPointer),Y
	sta Char1Ptr,X
	lsr
	bcc OnCharSetPageOne
	lda #>CharSetPageTwo
	sta Char1Ptr+1,X
	bne DoneSettingCharPtrHi
OnCharSetPageOne
	lda #>CharSetPageOne
	sta Char1Ptr+1,X
	nop
DoneSettingCharPtrHi
	inx
	inx
	iny
	cpx #24
	bne SetCharPtrsLoop


	rts

;-------------------------------------------------------------------------



DrawLineOfTextSubroutine

LF303: STA    HMCLR   
       STA    WSYNC   
	SLEEP 36		;+36	36
       LDX    #RIGHTSEVEN
       LDY    #8    		;+4	40
       LDA    Counter
       AND    #1    		;+5	45
       BEQ    SpritesLeft	;+2	47
       JMP    SpritesRight	;+3	50


LF327: STA    GRP1    	;+3	 9
       LDA    (Char5Ptr),Y 
       STA    GRP0    	;+8	17
       LDA    (Char7Ptr),Y 	;+5	22
       STX    HMP0    
       STX    HMP1    	;+6	28
       STA    GRP1    	;+3	31
       LDA    (Char9Ptr),Y 
       STA    GRP0    	;+8	39
       LDA    (Char11Ptr),Y 
       STA    GRP1    	;+8	47
       STA    GRP0    	;+3	50
SpritesRight
	DEY            
       BEQ    LF37D   	;+4	54

       LDA    (Char2Ptr),Y 
       LSR            
       STA    GRP0    	;+10	64
       LDA    (Char4Ptr),Y 
       LSR            
       STA.w  $001C   	;+11	75	GRP1, I assume.
       STA    HMOVE   	;+3	 2	sprites moved right
       LDA    (Char6Ptr),Y 
       LSR            
       STA    GRP0    	;+10	12
       LDA    (Char10Ptr),Y 
       LSR            
       STA    Temp     	;+10	22
       LDA    (Char8Ptr),Y 
       LSR            
       STA    GRP1    	;+10	32
       LDA    Temp     
       STA    GRP0    	;+6	38
       LDA    (Char12Ptr),Y 
       LSR            
       STA    GRP1    	;+10	48
SpritesLeft
	STA    GRP0    	;+3	51
       LDA    #LEFTSEVEN
       STA    HMP0    
       STA    HMP1    	;+8	59
       DEY            
       BEQ    LF387   	;+4	63
       LDA    (Char1Ptr),Y 
       STA    GRP0    	;+8	71
       LDA    (Char3Ptr),Y 	;+5	76
       STA    HMOVE   	;+3	 3	sprites moved left
       JMP    LF327   	;+3	 6

LF37D: STX    HMP0    
       STX    HMP1    
       STA    WSYNC   
       STA    HMOVE   
       BEQ    LF38C   

LF387: STA    WSYNC   
       NOP            
       NOP            
       NOP            

LF38C: LDA    #0    
       STA    GRP0    
       STA    GRP1    
       STA    GRP0    
       RTS            


;*************************************************************************
romCode  org *
  
  lda menu_pos
  sta TextBlockPointer
  clc
  rol TextBlockPointer
  rol TextBlockPointer+1
  clc
  rol TextBlockPointer
  rol TextBlockPointer+1
  clc
  rol TextBlockPointer
  rol TextBlockPointer+1
  clc
  rol TextBlockPointer
  rol TextBlockPointer+1
  lda TextBlockPointer+1
  clc
  adc #$f8
  sta TextBlockPointer+1
  
  ldy #14
  lda (TextBlockPointer),y
  sta $F000                 ; set bank entry point
;--- debug
  sta bank_shadow
;---
  iny
  lda (TextBlockPointer),y
  sta $F001                 ; set bank mode - 
                            ; $00 - bootloader 
                            ; $01 -  2kB 
                            ; $02 -  4kB
                            ; $03 -  8kB F8
                            ; $04 - 16kB F6
                            ; $05 - 32kB F4
                            ; $06 - 
                            ; $07 - 
;--- debug
  sta mode_shadow
;---
  jmp ($fffc)   
romCode_end org *

romCode_size = romCode_end - romCode
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------

; Music data

        align   $100

test_seq0
        dc.b    0,1,1,3,2,3,3,1
        dc.b    0,2,1,3,2,1,3,1

test_audcf0
        dc.b    $00+$1f,$40+$10,$40+$12,$40+$14
        dc.b    $20+$03,$20+$01,$20+$00,$20+$00
        dc.b    $00+$07,$00+$03,$80+$14,$80+$16
        dc.b    $20+$07,$20+$03,$20+$01,$20+$00

test_audv0
        dc.b    $cb,$a9
        dc.b    $42,$10
        dc.b    $b9,$75
        dc.b    $42,$10


test_audf1
        dc.b    $0f,$0f,$0f,$0f
        dc.b    $10,$10,$11,$11
        dc.b    $12,$12,$13,$13
        dc.b    $14,$14,$15,$15

test_audv1
        dc.b    $34,$45
        dc.b    $56,$67
        dc.b    $87,$65
        dc.b    $43,$21

        dc.b    $01,$23
        dc.b    $45,$67
        dc.b    $89,$ab
        dc.b    $cc,$dd

;------
play_music:
        ; music (experimental = ugly mess!)

        lda     music_pos
        and     #$03
        sta     music_temp

        lda     music_pos
        lsr
        lsr
        cmp     #$38
        and     #$07
        bcc     testtune_skip0a
        ora     #$08
testtune_skip0a
        tax
        lda     test_seq0,x
        asl
        asl
        ora     music_temp
        tax

        lda     test_audcf0,x
        sec
        rol
        rol
        rol
        rol
        sta     AUDC0

        lda     test_audcf0,x
        sta     AUDF0

        txa
        lsr
        tax
        lda     test_audv0,x
        bcs     testtune_skip0b
        lsr
        lsr
        lsr
        lsr
testtune_skip0b
        sta     AUDV0


        lda     music_pos
        cmp     #$f0
        bcs     testtune_skip1a
        lda     #$00
testtune_skip1a
        and     #$0f
        tax
        lda     test_audf1,x
        sta     AUDF1

        lda     #$0a
        sta     AUDC1

        lda     music_pos
        cmp     #$f0
        and     #$0f
        bcc     testtune_skip1b
        ora     #$10
testtune_skip1b
        lsr
        tax
        lda     test_audv1,x
        bcs     testtune_skip1c
        lsr
        lsr
        lsr
        lsr
testtune_skip1c
        sta     AUDV1


        lda     Counter
        lsr
        bcc     testtune_skip2
        inc     music_pos
testtune_skip2

        rts


  org $f500
;
;	align 256

;--Character Set data.  Beginning must be aligned to 8-byte boundary (i.e., $F100, $F108, $F150, etc.)
;	so that no individual bitmap crosses a page boundary.


CharSetPageOne
A
       .byte $00 ; |        | $F0E0
       .byte $C6 ; |XX   XX | $F0E1
       .byte $C6 ; |XX   XX | $F0E2
       .byte $FE ; |XXXXXXX | $F0E3
       .byte $C6 ; |XX   XX | $F0E4
       .byte $C6 ; |XX   XX | $F0E5
       .byte $6C ; | XX XX  | $F0E6
       .byte $38 ; |  XXX   | $F0E7
B
       .byte $00 ; |        | $F0E8
       .byte $FC ; |XXXXXX  | $F0E9
       .byte $C6 ; |XX   XX | $F0EA
       .byte $C6 ; |XX   XX | $F0EB
       .byte $FC ; |XXXXXX  | $F0EC
       .byte $C6 ; |XX   XX | $F0ED
       .byte $C6 ; |XX   XX | $F0EE
       .byte $FC ; |XXXXXX  | $F0EF
C
       .byte $00 ; |        | $F0F0
       .byte $3C ; |  XXXX  | $F0F1
       .byte $66 ; | XX  XX | $F0F2
       .byte $C0 ; |XX      | $F0F3
       .byte $C0 ; |XX      | $F0F4
       .byte $C0 ; |XX      | $F0F5
       .byte $66 ; | XX  XX | $F0F6
       .byte $3C ; |  XXXX  | $F0F7
D
       .byte $00 ; |        | $F0F8
       .byte $F8 ; |XXXXX   | $F0F9
       .byte $CC ; |XX  XX  | $F0FA
       .byte $C6 ; |XX   XX | $F0FB
       .byte $C6 ; |XX   XX | $F0FC
       .byte $C6 ; |XX   XX | $F0FD
       .byte $CC ; |XX  XX  | $F0FE
       .byte $F8 ; |XXXXX   | $F0FF
E
       .byte $00 ; |        | $F100
       .byte $FE ; |XXXXXXX | $F101
       .byte $C0 ; |XX      | $F102
       .byte $C0 ; |XX      | $F103
       .byte $F8 ; |XXXXX   | $F104
       .byte $C0 ; |XX      | $F105
       .byte $C0 ; |XX      | $F106
       .byte $FE ; |XXXXXXX | $F107
F
       .byte $00 ; |        | $F108
       .byte $C0 ; |XX      | $F109
       .byte $C0 ; |XX      | $F10A
       .byte $C0 ; |XX      | $F10B
       .byte $FC ; |XXXXXX  | $F10C
       .byte $C0 ; |XX      | $F10D
       .byte $C0 ; |XX      | $F10E
       .byte $FE ; |XXXXXXX | $F10F
G
       .byte $00 ; |        | $F110
       .byte $3E ; |  XXXXX | $F111
       .byte $66 ; | XX  XX | $F112
       .byte $C6 ; |XX   XX | $F113
       .byte $CE ; |XX  XXX | $F114
       .byte $C0 ; |XX      | $F115
       .byte $60 ; | XX     | $F116
       .byte $3E ; |  XXXXX | $F117
H
       .byte $00 ; |        | $F118
       .byte $C6 ; |XX   XX | $F119
       .byte $C6 ; |XX   XX | $F11A
       .byte $C6 ; |XX   XX | $F11B
       .byte $FE ; |XXXXXXX | $F11C
       .byte $C6 ; |XX   XX | $F11D
       .byte $C6 ; |XX   XX | $F11E
       .byte $C6 ; |XX   XX | $F11F
I
       .byte $00 ; |        | $F120
       .byte $78 ; | XXXX   | $F121
       .byte $30 ; |  XX    | $F122
       .byte $30 ; |  XX    | $F123
       .byte $30 ; |  XX    | $F124
       .byte $30 ; |  XX    | $F125
       .byte $30 ; |  XX    | $F126
       .byte $78 ; | XXXX   | $F127
J
       .byte $00 ; |        | $F128
       .byte $7C ; | XXXXX  | $F129
       .byte $C6 ; |XX   XX | $F12A
       .byte $06 ; |     XX | $F12B
       .byte $06 ; |     XX | $F12C
       .byte $06 ; |     XX | $F12D
       .byte $06 ; |     XX | $F12E
       .byte $06 ; |     XX | $F12F
K
       .byte $00 ; |        | $F130
       .byte $CE ; |XX  XXX | $F131
       .byte $DC ; |XX XXX  | $F132
       .byte $F8 ; |XXXXX   | $F133
       .byte $F0 ; |XXXX    | $F134
       .byte $D8 ; |XX XX   | $F135
       .byte $CC ; |XX  XX  | $F136
       .byte $C6 ; |XX   XX | $F137
L
       .byte $00 ; |        | $F138
       .byte $FE ; |XXXXXXX | $F139
       .byte $C0 ; |XX      | $F13A
       .byte $C0 ; |XX      | $F13B
       .byte $C0 ; |XX      | $F13C
       .byte $C0 ; |XX      | $F13D
       .byte $C0 ; |XX      | $F13E
       .byte $C0 ; |XX      | $F13F
M
       .byte $00 ; |        | $F140
       .byte $C6 ; |XX   XX | $F141
       .byte $C6 ; |XX   XX | $F142
       .byte $D6 ; |XX X XX | $F143
       .byte $FE ; |XXXXXXX | $F144
       .byte $FE ; |XXXXXXX | $F145
       .byte $EE ; |XXX XXX | $F146
       .byte $C6 ; |XX   XX | $F147
N
       .byte $00 ; |        | $F148
       .byte $C6 ; |XX   XX | $F149
       .byte $CE ; |XX  XXX | $F14A
       .byte $DE ; |XX XXXX | $F14B
       .byte $FE ; |XXXXXXX | $F14C
       .byte $F6 ; |XXXX XX | $F14D
       .byte $E6 ; |XXX  XX | $F14E
       .byte $C6 ; |XX   XX | $F14F
O
       .byte $00 ; |        | $F150
       .byte $7C ; | XXXXX  | $F151
       .byte $C6 ; |XX   XX | $F152
       .byte $C6 ; |XX   XX | $F153
       .byte $C6 ; |XX   XX | $F154
       .byte $C6 ; |XX   XX | $F155
       .byte $C6 ; |XX   XX | $F156
       .byte $7C ; | XXXXX  | $F157
P
       .byte $00 ; |        | $F158
       .byte $C0 ; |XX      | $F159
       .byte $C0 ; |XX      | $F15A
       .byte $FC ; |XXXXXX  | $F15B
       .byte $C6 ; |XX   XX | $F15C
       .byte $C6 ; |XX   XX | $F15D
       .byte $C6 ; |XX   XX | $F15E
       .byte $FC ; |XXXXXX  | $F15F
Q
       .byte $00 ; |        | $F160
       .byte $76 ; | XXX XX | $F161
       .byte $CC ; |XX  XX  | $F162
       .byte $DA ; |XX XX X | $F163
       .byte $C6 ; |XX   XX | $F164
       .byte $C6 ; |XX   XX | $F165
       .byte $C6 ; |XX   XX | $F166
       .byte $7C ; | XXXXX  | $F167
R
       .byte $00 ; |        | $F168
       .byte $CE ; |XX  XXX | $F169
       .byte $DC ; |XX XXX  | $F16A
       .byte $F8 ; |XXXXX   | $F16B
       .byte $CE ; |XX  XXX | $F16C
       .byte $C6 ; |XX   XX | $F16D
       .byte $C6 ; |XX   XX | $F16E
       .byte $FC ; |XXXXXX  | $F16F
S
       .byte $00 ; |        | $F170
       .byte $7C ; | XXXXX  | $F171
       .byte $C6 ; |XX   XX | $F172
       .byte $06 ; |     XX | $F173
       .byte $7C ; | XXXXX  | $F174
       .byte $C0 ; |XX      | $F175
       .byte $CC ; |XX  XX  | $F176
       .byte $78 ; | XXXX   | $F177
T
       .byte $00 ; |        | $F178
       .byte $30 ; |  XX    | $F179
       .byte $30 ; |  XX    | $F17A
       .byte $30 ; |  XX    | $F17B
       .byte $30 ; |  XX    | $F17C
       .byte $30 ; |  XX    | $F17D
       .byte $30 ; |  XX    | $F17E
       .byte $FC ; |XXXXXX  | $F17F
U
       .byte $00 ; |        | $F180
       .byte $7C ; | XXXXX  | $F181
       .byte $C6 ; |XX   XX | $F182
       .byte $C6 ; |XX   XX | $F183
       .byte $C6 ; |XX   XX | $F184
       .byte $C6 ; |XX   XX | $F185
       .byte $C6 ; |XX   XX | $F186
       .byte $C6 ; |XX   XX | $F187
V
       .byte $00 ; |        | $F188
       .byte $10 ; |   X    | $F189
       .byte $38 ; |  XXX   | $F18A
       .byte $7C ; | XXXXX  | $F18B
       .byte $EE ; |XXX XXX | $F18C
       .byte $C6 ; |XX   XX | $F18D
       .byte $C6 ; |XX   XX | $F18E
       .byte $C6 ; |XX   XX | $F18F
W
       .byte $00 ; |        | $F190
       .byte $C6 ; |XX   XX | $F191
       .byte $EE ; |XXX XXX | $F192
       .byte $FE ; |XXXXXXX | $F193
       .byte $FE ; |XXXXXXX | $F194
       .byte $D6 ; |XX X XX | $F195
       .byte $C6 ; |XX   XX | $F196
       .byte $C6 ; |XX   XX | $F197
XX
       .byte $00 ; |        | $F198
       .byte $C6 ; |XX   XX | $F199
       .byte $EE ; |XXX XXX | $F19A
       .byte $7C ; | XXXXX  | $F19B
       .byte $38 ; |  XXX   | $F19C
       .byte $7C ; | XXXXX  | $F19D
       .byte $EE ; |XXX XXX | $F19E
       .byte $C6 ; |XX   XX | $F19F
YY
       .byte $00 ; |        | $F1A0
       .byte $30 ; |  XX    | $F1A1
       .byte $30 ; |  XX    | $F1A2
       .byte $30 ; |  XX    | $F1A3
       .byte $78 ; | XXXX   | $F1A4
       .byte $CC ; |XX  XX  | $F1A5
       .byte $CC ; |XX  XX  | $F1A6
       .byte $CC ; |XX  XX  | $F1A7
Z
       .byte $00 ; |        | $F1A8
       .byte $FE ; |XXXXXXX | $F1A9
       .byte $E0 ; |XXX     | $F1AA
       .byte $70 ; | XXX    | $F1AB
       .byte $38 ; |  XXX   | $F1AC
       .byte $1C ; |   XXX  | $F1AD
       .byte $0E ; |    XXX | $F1AE
       .byte $FE ; |XXXXXXX | $F1AF
_
       .byte $00 ; |        | $F1B0
       .byte $00 ; |        | $F1B1
       .byte $00 ; |        | $F1B2
       .byte $00 ; |        | $F1B3
       .byte $00 ; |        | $F1B4
       .byte $00 ; |        | $F1B5
       .byte $00 ; |        | $F1B6
       .byte $00 ; |        | $F1B7
Zero
       .byte $00 ; |        | $F1B8
       .byte $7C ; | XXXXX  | $F1B9
       .byte $C6 ; |XX   XX | $F1BA
       .byte $E6 ; |XXX  XX | $F1BB
       .byte $D6 ; |XX X XX | $F1BC
       .byte $CE ; |XX  XXX | $F1BD
       .byte $C6 ; |XX   XX | $F1BE
       .byte $7C ; | XXXXX  | $F1BF
One
       .byte $00 ; |        | $F1C0
       .byte $FC ; |XXXXXX  | $F1C1
       .byte $30 ; |  XX    | $F1C2
       .byte $30 ; |  XX    | $F1C3
       .byte $30 ; |  XX    | $F1C4
       .byte $30 ; |  XX    | $F1C5
       .byte $70 ; | XXX    | $F1C6
       .byte $30 ; |  XX    | $F1C7
Two
       .byte $00 ; |        | $F1C8
       .byte $FE ; |XXXXXXX | $F1C9
       .byte $E0 ; |XXX     | $F1CA
       .byte $78 ; | XXXX   | $F1CB
       .byte $3C ; |  XXXX  | $F1CC
       .byte $0E ; |    XXX | $F1CD
       .byte $C6 ; |XX   XX | $F1CE
       .byte $7C ; | XXXXX  | $F1CF
Three
       .byte $00 ; |        | $F1D0
       .byte $7C ; | XXXXX  | $F1D1
       .byte $C6 ; |XX   XX | $F1D2
       .byte $06 ; |     XX | $F1D3
       .byte $3C ; |  XXXX  | $F1D4
       .byte $18 ; |   XX   | $F1D5
       .byte $0C ; |    XX  | $F1D6
       .byte $7E ; | XXXXXX | $F1D7
Four
       .byte $00 ; |        | $F1D8
       .byte $0C ; |    XX  | $F1D9
       .byte $0C ; |    XX  | $F1DA
       .byte $FE ; |XXXXXXX | $F1DB
       .byte $CC ; |XX  XX  | $F1DC
       .byte $6C ; | XX XX  | $F1DD
       .byte $3C ; |  XXXX  | $F1DE
       .byte $1C ; |   XXX  | $F1DF

	align 256

	.byte 0		;		so following data is offset by one byte

CharSetPageTwo

Five
       .byte $00 ; |        | $F1E0
       .byte $7C ; | XXXXX  | $F1E1
       .byte $C6 ; |XX   XX | $F1E2
       .byte $06 ; |     XX | $F1E3
       .byte $06 ; |     XX | $F1E4
       .byte $FC ; |XXXXXX  | $F1E5
       .byte $C0 ; |XX      | $F1E6
       .byte $FC ; |XXXXXX  | $F1E7
Six
       .byte $00 ; |        | $F1E8
       .byte $7C ; | XXXXX  | $F1E9
       .byte $C6 ; |XX   XX | $F1EA
       .byte $C6 ; |XX   XX | $F1EB
       .byte $FC ; |XXXXXX  | $F1EC
       .byte $C0 ; |XX      | $F1ED
       .byte $60 ; | XX     | $F1EE
       .byte $3C ; |  XXXX  | $F1EF
Seven
       .byte $00 ; |        | $F1F0
       .byte $30 ; |  XX    | $F1F1
       .byte $30 ; |  XX    | $F1F2
       .byte $30 ; |  XX    | $F1F3
       .byte $18 ; |   XX   | $F1F4
       .byte $0C ; |    XX  | $F1F5
       .byte $C6 ; |XX   XX | $F1F6
       .byte $FE ; |XXXXXXX | $F1F7
Eight
       .byte $00 ; |        | $F1F8
       .byte $7C ; | XXXXX  | $F1F9
       .byte $C6 ; |XX   XX | $F1FA
       .byte $C6 ; |XX   XX | $F1FB
       .byte $7C ; | XXXXX  | $F1FC
       .byte $C6 ; |XX   XX | $F1FD
       .byte $C6 ; |XX   XX | $F1FE
       .byte $7C ; | XXXXX  | $F1FF
Nine
       .byte $00 ; |        | $F200
       .byte $78 ; | XXXX   | $F201
       .byte $0C ; |    XX  | $F202
       .byte $06 ; |     XX | $F203
       .byte $7E ; | XXXXXX | $F204
       .byte $C6 ; |XX   XX | $F205
       .byte $C6 ; |XX   XX | $F206
       .byte $7C ; | XXXXX  | $F207
Multiply
       .byte $00 ; |        | $F208
       .byte $00 ; |        | $F209
       .byte $44 ; | X   X  | $F20A
       .byte $28 ; |  X X   | $F20B
       .byte $10 ; |   X    | $F20C
       .byte $28 ; |  X X   | $F20D
       .byte $44 ; | X   X  | $F20E
       .byte $00 ; |        | $F20F
Divide
       .byte $00 ; |        | $F210
       .byte $00 ; |        | $F211
       .byte $10 ; |   X    | $F212
       .byte $00 ; |        | $F213
       .byte $FE ; |XXXXXXX | $F214
       .byte $00 ; |        | $F215
       .byte $10 ; |   X    | $F216
       .byte $00 ; |        | $F217
Add
       .byte $00 ; |        | $F218
       .byte $10 ; |   X    | $F219
       .byte $10 ; |   X    | $F21A
       .byte $10 ; |   X    | $F21B
       .byte $FE ; |XXXXXXX | $F21C
       .byte $10 ; |   X    | $F21D
       .byte $10 ; |   X    | $F21E
       .byte $10 ; |   X    | $F21F
Subtract
       .byte $00 ; |        | $F220
       .byte $00 ; |        | $F221
       .byte $00 ; |        | $F222
       .byte $00 ; |        | $F223
       .byte $FE ; |XXXXXXX | $F224
       .byte $00 ; |        | $F225
       .byte $00 ; |        | $F226
       .byte $00 ; |        | $F227
Equal
       .byte $00 ; |        | $F228
       .byte $00 ; |        | $F229
       .byte $00 ; |        | $F22A
       .byte $FE ; |XXXXXXX | $F22B
       .byte $00 ; |        | $F22C
       .byte $FE ; |XXXXXXX | $F22D
       .byte $00 ; |        | $F22E
       .byte $00 ; |        | $F22F
GreaterThan
       .byte $00 ; |        | $F230
       .byte $20 ; |  X     | $F231
       .byte $10 ; |   X    | $F232
       .byte $08 ; |    X   | $F233
       .byte $04 ; |     X  | $F234
       .byte $08 ; |    X   | $F235
       .byte $10 ; |   X    | $F236
       .byte $20 ; |  X     | $F237
LessThan
       .byte $00 ; |        | $F238
       .byte $08 ; |    X   | $F239
       .byte $10 ; |   X    | $F23A
       .byte $20 ; |  X     | $F23B
       .byte $40 ; | X      | $F23C
       .byte $20 ; |  X     | $F23D
       .byte $10 ; |   X    | $F23E
       .byte $08 ; |    X   | $F23F
LeftArrow
       .byte $00 ; |        | $F240
       .byte $00 ; |        | $F241
       .byte $20 ; |  X     | $F242
       .byte $40 ; | X      | $F243
       .byte $FE ; |XXXXXXX | $F244
       .byte $40 ; | X      | $F245
       .byte $20 ; |  X     | $F246
       .byte $00 ; |        | $F247
Comma
        .byte #%00000000
        .byte #%01100000
        .byte #%00110000
        .byte #%00110000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
Period
        .byte #%00000000
        .byte #%01100000
        .byte #%01100000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
RightParens
       .byte $00 ; |        | $F258
       .byte $20 ; |  X     | $F259
       .byte $10 ; |   X    | $F25A
       .byte $08 ; |    X   | $F25B
       .byte $08 ; |    X   | $F25C
       .byte $08 ; |    X   | $F25D
       .byte $10 ; |   X    | $F25E
       .byte $20 ; |  X     | $F25F
LeftParens
       .byte $00 ; |        | $F260
       .byte $08 ; |    X   | $F261
       .byte $10 ; |   X    | $F262
       .byte $20 ; |  X     | $F263
       .byte $20 ; |  X     | $F264
       .byte $20 ; |  X     | $F265
       .byte $10 ; |   X    | $F266
       .byte $08 ; |    X   | $F267
       .byte $00 ; |        | $F268

EmptyBlock
  .byte _,_,_,_,_,_,_,_,_,_,_,_
TitleBlock
  .byte A,Two,Six,F,L,A,S,H,C,A,R,T
;------------
;USE JOYSTICK
;TO PICK GAME
;------------
;HIT FIRE TO 
;   START FUN  
;------------
;GREETINGS   
;  GOING TO
;------------

GreetingsBlock
  .byte U,S,E,_,J,O,YY,S,T,I,C,K
  .byte T,O,_,P,I,C,K,_,G,A,M,E
  
  .byte H,I,T,_,F,I,R,E,_,T,O,_
  .byte _,_,_,S,T,A,R,T,_,F,U,N

  .byte G,R,E,E,T,I,N,G,S,_,_,_
  .byte _,_,G,O,I,N,G,_,T,O,_,_

  .byte F,E,R,YY,XX,_,_,J,V,A,S,_
  .byte A,T,A,R,I,A,N,_,A,D,T,_

  .byte R,A,W,B,I,T,S,_,V,O,YY,_
  .byte _,G,A,M,B,L,E,R,One,Seven,Two,_
  
  .byte G,O,R,G,H,_,T,R,E,B,O,R
  .byte F,L,A,S,H,J,A,Z,Z,C,A,T
  
  .byte W,I,E,C,Z,O,R,_,L,U,C,YY
  .byte _,V,I,T,I,_,_,Z,I,O,N,A

  .byte _,A,O,L,_,_,A,Period,A,R,E,A
  
  .byte A,Period,A,G,E,_,A,B,B,U,C,_
;  .byte A,L,L,_,F,O,R,G,O,T,E,N
  
  .byte Two,Zero,One,Six,Period,Zero,Two,Period,Two,Seven,_,_
  .byte B,U,D,A,P,E,S,T,Comma,H,U,N
  
  .byte _,_,_,_,_,_,_,_,_,_,_,_
  .byte T,D,I,D,I,D,I,T,Period,C,O,M
       org $f800
	align 256

;--text data.  arrange in twelve-character lines.  terminate with $FF

;	I don't *think* it matters how this data is aligned with page boundaries.

TextBlock1                                    ; +13 amount of roms in menu
                                              ; +14 entry bank     +15 bs mode
  .byte T,E,N,N,I,S,_,_,_,_,_,_,0,127,  2,  1 ;                       2kB
  .byte C,O,M,B,A,T,_,_,_,_,_,_,0,0,    3,  1 ;                       4kB
  .byte R,I,V,E,R,_,R,A,I,D,_,_,0,0,    4,  2 ;                       8kB
  .byte Q,_,B,E,R,T,_,_,_,_,_,_,0,0,    6,  2 ;                      16kB
;  .byte 255
  .byte P,I,T,F,A,L,L,_,_,_,_,_,0,0,    8,  2 ;                      32kB
;  .byte 255
  .byte P,I,N,B,A,L,L,_,_,_,_,_,0,0,   10,  2
;  .byte 255
  .byte P,A,C,_,M,A,N,_,_,_,_,_,0,0,   12,  2
;  .byte 255
  .byte K,E,YY,S,T,O,N,E,_,K,A,P,0,0,  14,  2
  .byte D,O,N,K,E,YY,_,K,O,N,G,_,0,0,  16,  2
  .byte D,E,F,E,N,D,E,R,_,_,_,_,0,0,   18,  2
  .byte XX,E,V,I,O,U,S,_,_,_,_,_,0,0,  20,  3

  .byte P,O,L,E,_,P,O,S,I,T,I,O,0,0,   24,  3
  .byte M,S,_,P,A,C,_,M,A,N,_,_,0,0,   28,  3
  .byte M,O,O,N,_,P,A,T,R,O,L,_,0,0,   32,  3
  .byte J,U,N,G,L,E,_,H,U,N,T,_,0,0,   36,  3        ;16
  .byte J,O,U,S,T,_,_,_,_,_,_,_,_,_,   40,  3


  .byte G,A,L,A,XX,I,A,N,_,_,_,_,0,0,  44,  3
  .byte D,O,N,K,E,YY,_,J,R,_,_,_,0,0,  48,  3
  .byte C,E,N,T,I,P,E,D,E,_,_,_,0,0,   52,  3
  .byte M,I,L,L,I,P,E,D,E,_,_,_,0,0,   56,  4

  .byte XX,E,N,O,P,H,O,B,E,_,_,_,0,0,  64,  4
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0     ;32

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0    ;48

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Nine,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,Zero,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,One,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,Two,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,One,Three,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,Four,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,Five,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,One,Six,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0   ;64

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0    ;80

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0     ;96

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,One,One,Two,0,0,0,0     ;112

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Two,Four,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,Zero,Zero,Eight,0,0,0,0

  .byte F,R,E,E,_,R,O,M,_,One,Two,Five,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,One,Two,Six,0,0,0,0
  .byte F,R,E,E,_,R,O,M,_,One,Two,Seven,0,0,0,0   ;127

TextBlock2
;	.byte T,H,I,S,_,I,S,_,T,H,E,_
;	.byte E,N,T,I,R,E,_,_,_,_,_,_
;	.byte C,H,A,R,A,C,T,E,R,_,_,_
;	.byte S,E,T,Period,_,_,_,_,_,_,_,_
;	.byte A,B,C,D,E,F,G,H,I,J,K,L
;	.byte M,N,O,P,Q,R,S,T,U,V,W,XX
;	.byte YY,Z,Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine
;	.byte Multiply,Divide,Add,Subtract,Equal,LessThan,GreaterThan,LeftArrow,Comma,Period,LeftParens,RightParens
;	.byte 255




;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------
  org $FFF0
FreeBlock
  .byte 255,F,R,E,E,_,_,_,Four,Four,Six,K

	org $FFFC
	.word Start
	.word Start

