;---------------------------------------------------------
;
;  PET ROM/RAM Flash-Software
;
;
;--------------------------------------------------------- 
;
;
; Flash Buffer : $3000 - $8000
;
;
;---------------------------------------------------------
!source "labels.asm"          
!to "flash.prg",cbm
; Basicstart
*= $0401
!byte $0c,$08,$0a,$00,$9e,$31,$30,$33,$39,$00,$00,$00,$00
; main
*=$040f

start:
          lda #$93
          jsr bsout
          lda #$03
          sta blocks
          lda #$00            ; lo for screen $8000
          sta frm_optn
          sta t_lo         
          lda #$0e
          sta $e84c 
          lda #$00
          sta checkbyte
          lda #$28
          sta temp
          lda #$02
          sta frm_addr
          lda #$08
          sta drive             
;--------------------------------------------------
; Check for BASIC-Version and CRTC 
;-------------------------------------------------    

check:              
          lda irq_v
          cmp #$6b
          bne +
          lda reset_v
          cmp #$38
          bne +
          lda #$01
          sta checkbyte
          sta basic  
          sta drive
          
          
          jmp crtc_chk
        
          
+         lda irq_v
          cmp #$1b
          bne +
          lda reset_v
          cmp #$d1
          bne +
          lda #$02
          sta checkbyte
          sta basic
          jmp crtc_chk
         

+         lda irq_v
          cmp #$42
          bne crtc_chk
          lda reset_v
          cmp #$16
          bne crtc_chk
          lda #$04
          sta checkbyte
          sta basic

crtc_chk:
          ldx #$00
-         lda ed_tab,x
          cmp editor,x
          bne + 
          inx
          cpx #$10
          bne - 
          lda #$00
          sta t_lo
          lda #$08
          sta crtc_col   
          jmp config 
+         lda #$04
          sta crtc_col   
config:
           
          lda basic 
          cmp #$01
          bne +
          sta drive
+         tax
          tay
          lda #$80
          sta t_hi
          lda #<ui
          sta s_lo
          lda #>ui
          sta s_hi
;--------------------------------------------------
; Build Screen
;-------------------------------------------------    
--        ldy #$00 
-         lda (s_lo),y
          sta (t_lo),y
          iny
          cpy #$28
          bne -
;------------------------------------------------
          clc  
          lda s_lo
          adc #$28
          sta s_lo
          bcc +
          inc s_hi
          clc
+         lda t_lo
          adc temp
          sta t_lo
          bcc +
          inc t_hi
+         inx  
          cpx #$1b            
          bne --
+         lda crtc_col  
+         clc         
          adc #$30
          sta $8381                     
          clc
          lda basic
          adc #$30
          sta $8391 
          ldy #$00 
          lda basic
          cmp #$01
          beq +
          cmp #$02
          beq ++
          jmp +++
; delete catalog for basic 1
+         ldy #$00
          lda #$20
-         sta $8267,y         
          iny
          cpy #$09
          bne - 
; show drive 1 for basic 1                   
          lda basic
          cmp #$01
          bne keyboard
          clc
          adc #$30
          sta $8172 
; delete catalog for basic 2
++        ldy #$00
          lda #$20
-         sta $8267,y         
          iny
          cpy #$09
          bne - 
+++       lda #$00 
          sta flash_ofs 
keyboard:   
          jsr get     ; get keyboard
+         cmp #68     ; "D"rive
          bne +
          jmp drve 
+         cmp #65     ; "A"ddress
          bne +
          jmp addr 
+         cmp #82     ; "R"OM
          bne +
          jmp romset
+         cmp #73     ; "I"nfo
          bne +
          jmp info
+         cmp #77     ; "M"ove
          bne +
          jmp move
+         cmp #81     ; "Q"uit
          bne +
          jmp exit
+         cmp #67     ; "C"atalog
          bne +
          jmp catalog
+         cmp #83     ; "S"ave
          bne +
          jmp save
+         cmp #76     ; "L"oad
          bne +
          jmp load
+         cmp #87     ; "W"rite
          bne +
          jmp flash_write
+         jmp keyboard

;--------------------------------------------------
; drive select
;-------------------------------------------------    

drve:    lda checkbyte
		 cmp #$01
         bne b2_4
; Basic 1         
         lda $8172
         cmp #$31
         bne +
         lda #$32
         sta $8172
         lda #$02
         sta drive
         jmp keyboard
+        lda #$31
         sta $8172
         lda #$01
         sta drive
         jmp keyboard
; Basic 2 and 4        
b2_4:
         lda $8172
         cmp #$38
         bne +
         lda #$39
         sta $8172
         lda #$09
         sta drive
         jmp keyboard
+        lda #$38
         sta $8172
         lda #$08
         sta drive
         jmp keyboard
;--------------------------------------------------
; ROM/Firmware Switch
;
; frm_optn: $00/$01
;-------------------------------------------------    
romset: 
         lda frm_optn
         bne + 
         inc frm_optn

         ldy #$00 
-        lda $81c3,y   
         and #$7f
         sta $81c3,y
         iny
         cpy #$08
         bne -
         iny 
-        lda $81c3,y   
         ora #$80
         sta $81c3,y
         iny
         cpy #$0f
         bne -
         jmp _9000

+        dec frm_optn
         ldy #$00 
-        lda $81c3,y   
         ora #$80
         sta $81c3,y
         iny
         cpy #$08
         bne -
         iny 
-        lda $81c3,y   
         and #$7f
         sta $81c3,y
         iny
         cpy #$0f
         bne -
         jmp _b000         
;--------------------------------------------------
; Address Switch
;
; frm_optn: $00/$01 
; 
; frm_addr: $00 : $9000
;           $01 : $a000
;           $02 : $b000
;           $03 : $c000
;
;-------------------------------------------------    
; firmware                            
addr:    lda frm_optn
         bne ++        
         lda frm_addr
         cmp #$03
         beq _b000
         cmp #$02
         beq _c000 
         jmp keyboard         
_c000:
         lda #"C"
         sta $819c
         lda #$03
         sta frm_addr
         jmp keyboard         

; option roms
++
         lda frm_addr
         beq _a000       
         cmp #$01
         beq _b000
         cmp #$02
         beq +
_a000:
         lda #"A"
         sta $819c
         lda #$01
         sta frm_addr
         jmp keyboard         
_b000:
          lda #"B"
          sta $819c
          lda #$02
          sta frm_addr
          jmp keyboard         
_9000:
+         lda #"9"
          sta $819c
          lda #$00
          sta frm_addr
          jmp keyboard         
;--------------------------------------------------
; infoscreen
;-------------------------------------------------    
info:     jsr capture
          jmp showinfo
capture:
          lda #$03
          sta blocks
          lda #<screen_buffer 
          sta t_lo
          lda #>screen_buffer
          sta t_hi
          lda #$f0
          sta s_lo
          lda #$80
          sta s_hi
          jsr copy      
          rts
showinfo:
          lda #$f0 
          sta t_lo
          lda #$80
          sta t_hi
          lda #<infoscreen
          sta s_lo
          lda #>infoscreen
          sta s_hi
          jsr copy 
-         jsr get
          cmp #$20
          bne -  
restore:
          lda #<screen_buffer 
          sta s_lo
          lda #>screen_buffer
          sta s_hi
          lda #$f0
          sta t_lo
          lda #$80
          sta t_hi
          jsr copy
          jmp keyboard
copy:     ldy #$00
          ldx #$00       
-         lda (s_lo),y
          sta (t_lo),y
          iny
          bne -
          inc s_hi
          inc t_hi
          inx 
          cpx blocks
          bne -
          rts  

;--------------------------------------------------
; Firmware/Option ROM Switch
;
; frm_optn: $00/$01  
;
; frm_addr: $00 : $9000
;           $01 : $a000
;           $02 : $b000
;           $03 : $c000
; buffer: $3000
;-------------------------------------------------    

move:     
          lda #$30
          sta t_hi
          lda #$00
          sta s_lo
          sta t_lo
          lda frm_optn
          bne ++
; firmware         
          lda frm_addr
          cmp #$02
          bne +
          jmp cp_b000
+         cmp #$03
          beq cp_c000
cp_b000: 
          lda #$b0
          sta s_hi
          lda #$50
          sta blocks
          jsr copy
-         lda roms_b,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
cp_c000: 
          lda #$c0
          sta s_hi
          lda #$40
          sta blocks
          jsr copy
-         lda roms_c,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
; option roms
++
          lda frm_addr
          bne +
          jmp op_9000
+         cmp #$01
          bne +
          jmp op_a000
+         cmp #$02
          beq op_b000
op_9000:
          lda #$90
          sta s_hi
          lda #$10
          sta blocks
          jsr copy
          ldy #$00
-         lda opt9000,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
op_a000:
          lda #$a0
          sta s_hi
          lda #$10
          sta blocks
          jsr copy
          ldy #$00
-         lda opta000,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
op_b000:
          lda #$b0
          sta s_hi
          lda #$10
          sta blocks
          jsr copy
-         lda optb000,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
exit:     ldy #$00
          lda #$93
          jsr bsout
-         lda _ex,y
          sta $8000,y
          iny
          cpy #$18
          bne -
          rts
save:     lda basic
          cmp #$01
          bne +
          jsr bas1_savefile
+         cmp #$02
          bne +
          jsr bas2_savefile
+         cmp #$04
          bne +
          jsr bas4_savefile
+         jmp keyboard           
load:   
          lda basic
          cmp #$01
          bne +
          jsr bas1_loadfile
+         cmp #$02
          bne +
          jsr bas2_loadfile
+         cmp #$04
          bne +
          jsr bas4_loadfile
          jmp keyboard           

!source "flash.asm"          
!source "drives.asm"          
!source "data.asm"          
*=$1800
flash_ofs: !by $00 
;*=$2C00
screen_buffer:          





