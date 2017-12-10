;---------------------------------------------------------
;
;  PET ROM/RAM Flasher Software cbm 8000
;
;  Basic 4
;
;--------------------------------------------------------- 
;
;
; Flash Buffer : $3000 - $8000
;
; Please report bugs to : info@cbmhardware.de
;
;---------------------------------------------------------
!to "pflash8.prg",cbm
; Basicstart
*= $0400
!byte $00,$0c,$08,$0a,$00,$9e,$31,$30,$33,$39,$00,$00,$00,$00
; main
*=$040f
;--------------------------------------------------
    chrout = $ffd2  ;  print a char on screen
    get    = $ffe4  ;  to get pressed keys 
    screen = $8000  ;  Screen RAM
    buffer = $2c00  ;  Buffer for windowing = 1Kb , >$2f00
;--------------------------------------------------
;File
    BUF    = $2f00 
    FNADR  = $DA 
    FNLEN  = $D1 
    STATUS = $96
    FA     = $D4 
    SA     = $D3 
curcol     = $c6
currow     = $d8
;---------------------------------------------------
;ROM>RAM Copy

    source_lo = $0a  
    source_hi = $0b
    target_lo = $0c
    target_hi = $0d 


;--------------------------------------------------
    bas1 = $e180    ;  "***" in power on message (Basic 1) 
    bas2 = $e1c4    ;  "###" in power on message (Basic 2)
;--------------------------------------------------
; dummy vars for menu itmes
;-------------------------------------------------    
    drive  = $03fa  ;  Drive Address
    dummy1 = $03fb  ;  Option address
    dummy2 = $03fc  ;  Chiptype: 0=at29c512/at29c010 
    dummy3 = $03fd  ;  Firmware adress
    dummy4 = $03fe  ;  ROM: 0=Firmware<>1=Option  
    dummy5 = $03ff  ;  0 Bin<> 1 Prg : 
;--------------------------------------------------    

init
          lda #0
          sta dummy1      
          sta dummy2                  
          sta dummy3     
          sta dummy4
          sta dummy5
          lda #8
          sta drive
          jsr sc 
          jmp detect      ; show detected machine (basic,mem, crtc)
          main
taster    jsr taste
          jmp taster    
;----------------------------------------------------------          
taste :   jsr get     ; get keyboard
          cmp #27
          beq init    ; restart          
          cmp #67     ; "C"
          beq c_type  ;  
          cmp #68
          beq directory
          cmp #70     ; "F"
          beq firm_option_j  ;  use jumppoint
          cmp #65     ; "A"
          beq options_j ;  
          cmp #81     ; "Q"uit
          beq end     ;  
          cmp #83     ; "S"ave
          beq save_j     ;  
          cmp #87     ; "S"ave
          beq flash_j     ;  
          cmp #76     ; "L"oad
          beq load_j
          cmp #77     ; "M"ove ROM to RAM
          beq romram_j
          cmp #79
          beq driveaddr          
          cmp #82     ; "R"OM - Firmware/Option
          beq rom_firm_j ;  
          bne taste
          rts
;--------------------------------------------------                  
end       jmp exit_tool

directory jsr window
          lda #147
          jsr chrout
          ldx drive
          stx $d4
          jsr $d87d ;$d873
diry0     jsr get
          cmp #32
          beq diry1
          bne diry0       
          rts
diry1     lda #147
          jsr chrout
          jsr close_w
          jmp taster

;--------------------------------------------
; jumppoints - we are to far away
;--------------------------------------------
firm_option_j jsr firm_option
              rts 
options_j     jsr option
              rts
rom_firm_j    jsr rom_firm
              rts 
load_j        jsr setload
              rts              
save_j        jmp setsave
romram_j      jmp initrampump               
flash_j       jmp setflash
driveaddr     jmp driveaddress     


;----------------------------------------------------------
c_type       lda dummy2
             cmp #$01      
             beq ctyp2
             cmp #$02
             beq ctyp0
             cmp #$00
             beq ctyp1
             ldx #0
       ctyp0 lda ct1,x
             sta 33059+123,x
             inx
             cpx #17
             bne ctyp0
             lda #0
             sta dummy2
             rts
       ctyp1 ldx #0
      ctyp1a lda ct2,x
             sta 33059+123,x
             inx
             cpx #17
             bne ctyp1a
             lda #1
             sta dummy2
             rts      
       ctyp2 ldx #0 
      ctyp2a lda ct3,x
             sta 33059+123,x
             inx
             cpx #17
             bne ctyp2a
             lda #2
             sta dummy2
             rts      
;----------------------------------------------------------
rom_firm     lda dummy4
             cmp #$01      
             bne wf1
             ldx #0
       rof2  lda rf1,x
             sta 33139,x
             inx
             cpx #15
             bne rof2
             lda #0
             sta dummy4
             ldx #00
       fw1   lda firm_adr,x       ; show firmware address 
             sta 33099,x
             inx
             cpx #4
             bne fw1      
             rts           
       wf1   ldx #0               ; switch to option
       rof1  lda rf2,x            ; Option 
             sta 33139,x
             inx
             cpx #15
             bne rof1
             lda #1
             sta dummy4                        
             ldx #00
       op1   lda opt_adr1,x       ; show option address 1
             sta 33099,x
             inx
             cpx #4
             bne op1      
             rts      
;----------------------------------------------------------
firm_option  lda dummy5
             cmp #$01      
             bne w1
             ldx #0
       fol2  lda fo1,x
             sta 33019,x
             inx
             cpx #7
             bne fol2
             lda #0
             sta dummy5
             rts
       w1    ldx #0
       fol1  lda fo2,x
             sta 33019,x
             inx
             cpx #7
             bne fol1
             lda #1
             sta dummy5
             rts      
option       lda dummy4           ; firmware choosen
             cmp #0         
             bne option2          ; no
             lda dummy3
             cmp #$01
             beq optf1
     optf2   lda firm_adr1,x       ; show firmware address 
             sta 33099,x
             inx
             cpx #4
             bne optf2      
             lda #$01
             sta dummy3            ; feed dummy 
             rts
             ldx #00
     optf1   lda firm_adr,x       ; show firmware address 
             sta 33099,x
             inx
             cpx #4
             bne optf1      
             lda #$00
             sta dummy3            ; feed dummy 
             rts
;----------------------------------------------------------
option2      lda dummy1        
             cmp #$01      
             beq opt2
             cmp #$02
             beq opt0
             cmp #$00
             beq opt1
             ldx #0
        opt0 lda opt_adr1,x
             sta 33099,x
             inx
             cpx #5
             bne opt0
             lda #0
             sta dummy1
             rts
        opt1 ldx #0
       opt1a lda opt_adr2,x
             sta 33099,x
             inx
             cpx #5
             bne opt1a
             lda #1
             sta dummy1
             rts      
        opt2 ldx #0 
       opt2a lda opt_adr3,x
             sta 33099,x
             inx
             cpx #5
             bne opt2a
             lda #2
             sta dummy1
             rts                    
;--------------------------------------------------    
; Build main screen
;--------------------------------------------------    
          sc  lda #14        ; set charmode
              sta 59468
              lda #147       ; clear screen
              jsr chrout
              lda #$0e
              sta r2+2              
              lda #$80
              sta s1+2
              lda #$00
              sta s1+1
              sta r2+1
              jsr tx         ; build main screen
              lda #240
              sta r2+1
              sta s1+1
              jsr tx
              lda #224
              sta r2+1
              sta s1+1
              inc r2+2
              inc s1+2
              jsr tx
              rts
              
          tx  ldy #00         ; counter for text 
          r2  lda $0e00,y     ; point text
          s1  sta screen,y    ; drop some text on the screen 
              iny 
              cpy #240        ; 
              bne r2          ;  
              rts             ; end
;--------------------------------------------------
;
; DANGEROUS "Borg part" ;) 
; 
; Detecting Basic, Memory and CRTC - self-reconfiguring code
;
;--------------------------------------------------
detect
            ldx #$00        ; Basic 1 detect
        l1  lda bas1,x      ; test for "***" in $e180 : "*** Commodore Basic ***"
            cmp #$2a 
            beq g1 
            inx
            cpx #2
            bne l1  
            ldx #$00        ; Basic 2 detect
        l2  lda bas2,x      ; test for "###" in $e1c4 : "### Commodore Basic ###" 
            cmp #$23 
            beq g5 
            inx
            cpx #2
            bne l2  
            ldx #00
        l4  lda $dea4,x     ; Basic 4 detect
            cmp basic4,x
            bne l5
            inx
            cpx #29
            bne l4  
;-------------------------------------
; compare editor rom - crtc version ?
;-------------------------------------
            jsr memtest
        l10 ldx #00
        crt lda crtc,x 
            cmp $e000,x
            bne l9
            inx
            cpx #8
            bne crt
;-------------------------------------
; insert (CBM 8000) in Text
;-------------------------------------
            ldx #00
        l7  lda crtmes,x     
            sta pettext+18,x
            inx
            cpx #10
            bne l7
            jmp l6
;-------------------------------------
; insert (CBM 4000) in Text
;-------------------------------------
        l9  ldx #00
        l8  lda crtno,x     
            sta pettext+18,x
            inx
            cpx #10
            bne l8
;-------------------------------------
; tool setup and configure output routine 
;-------------------------------------
        l6  lda #$4a        ; set (q)uit for basic 4
            sta ex_+1
            lda #$b7
            sta ex_+2       
            lda #52         ; Basic 4 detected 
            sta g4+1        ; modify routine
            jsr g2          ; and drop text in screenram 
            jmp main        
;--------------------------------------------------
        l5  ldy #$00        ; Basic 1 or 2 not detected-unknown, maybe third party or Basic 4.0 machine
        r1  lda unktext,y   ; 
            jsr chrout      ; drop some text on the screen 
            iny 
            cpy #39         ; 40 chars
            bne r1          ; finished ? else loop again. 
            jmp main   
;--------------------------------------------------
        g5 jsr bas2mem
           jmp g6
;--------------------------------------------------

        g1  lda #49
        g3  sta pettext+6    ; insert "1" in text
            lda #$20         ; set cursor to line 20 : dez. 33568 - $8320
        cr1 sta 224
            lda #$83
        cr2 sta 225
            jsr petmem
            jsr tx1          ; and drop message on the screen
            jmp main
;--------------------------------------------------
        g2  jsr memtest      ; Basic 4 Memtest
        g6  lda #196         ; modify addresses for basic 4
            sta cr1+1
            lda #197
            sta cr2+1 
        g4  lda #50          ; insert "2" in text
            jmp g3           ; and use last procedure
;--------------------------------------------------
        

        tx1 ldy #$00         ; counter for text 
        tx2 lda pettext,y    ; point text
            sta $8320,y       ; drop some text on the screen 
            iny 
            cpy #39          ; 40 chars
            bne tx2          ; finished ? else loop again. 
            rts              ; end
;----------------------------------------------------
; Memtest cbm + pet
;----------------------------------------------------
memtest     lda 51
            cmp #63         ; check for 16K
            bne l11
      mem1  lda #49
            sta pettext+29  ; "1"
            lda #54 
            sta pettext+30  ; "6"
       l11  lda 51 
            cmp #127        ; check for 32K
            bne memex    
      mem2  lda #51
            sta pettext+29   ; "3" 
            lda #50 
            sta pettext+30   ; "2"
     memex  lda #75
            sta pettext+31   ; "K"
            rts                 
petmem      lda 133          ; old pet mem test
            cmp #32          ; check for 8K
            bne pmem1
            lda #56
            sta pettext+30   ; "8"
            jmp memex        
     pmem1  lda 133
            cmp #64          ; check for 16K
            bne pmem2
            jmp mem1
     pmem2  lda 133 
            cmp #128         ; check for 32K
            bne memex    
            jmp mem2
bas2mem     lda 51
            cmp #32          ; "8"
            bne bas2m2
            lda #56
            sta pettext+30   ; 
            jmp memex        
    bas2m2  lda 51
            cmp #64          ; "16" 
            bne bas2m3
            jmp mem1
    bas2m3  lda 51
            cmp #128         ; "32"
            bne memex
            jmp mem2        
                      
;-------------------------------------------------------------------------
romramcopy       sei
                 ldx #00
                 ldy #00
        s_lo     lda #03
                 sta source_lo
        s_hi     lda #00
                 sta source_hi
        t_lo     lda #00
                 sta target_lo
        t_hi     lda #30
                 sta target_hi
        repump   ldy #00
          pump   lda (source_lo),Y
                 sta (target_lo),Y
                 iny
                 bne pump
                 inc source_hi
                 inc target_hi
                 inx
     pcounter    cpx #04
                 bne repump    
                 cli
                 rts   
;----------------------------------------------------------------------------                 
initrampump     lda dummy4
                cmp #00
                beq firmpump
                cmp #01
                beq optionpump     

optionpump      lda dummy1
                cmp #01
                beq opta000
                cmp #00
                beq opt9000
                cmp #02
                beq optb000
   opt9000      lda #00       ; move Option ROM from $A000-AFFF to
                sta s_lo+1     ; $3000 - 3FFF 
                sta t_lo+1     ; $00
                lda #$30
                sta t_hi+1     ; $90 
                lda #$90
                sta s_hi+1
                lda #16       ; 16*256 Bytes = 4KBytes 
                sta pcounter+1 
                lda #57
                sta buffer2+12
       optp_ex  jsr romramcopy
                jsr bufmsg2
                jmp taster
   optb000      lda #00
                sta s_lo+1     ; $5000 - 5FFF 
                sta t_lo+1     ; $00
                lda #$50
                sta t_hi+1     ; $a0 
                lda #$b0
                sta s_hi+1
                lda #16       ; 16*256 Bytes = 4KBytes 
                sta pcounter+1 
                lda #66
                sta buffer2+12
                jmp optp_ex         
   opta000      lda #00
                sta s_lo+1     ; $4000 - 4FFF 
                sta t_lo+1     ; $00
                lda #$40
                sta t_hi+1     ; $a0 
                lda #$a0
                sta s_hi+1
                lda #16       ; 16*256 Bytes = 4KBytes 
                sta pcounter+1 
                lda #65
                sta buffer2+12
                jmp optp_ex 

;-----------------------------------------------------------

firmpump         lda #67
                 sta irptx+1
                 lda dummy3
                 cmp #00
                 beq irp1
                 cmp #01
                 beq iramp1
                 
       irp1      lda #$c0          ; $c000
                 sta s_hi+1
                 lda #$00
                 sta s_lo+1
                 sta t_lo+1
                 lda #$30          ; Buffer $3000>
                 sta t_hi+1                 
       irp2      lda #64
                 sta pcounter+1    ; copy 16K
                 jsr romramcopy
       irptx     LDA #67
                 STA buffer1+10
                 jsr bufmsg
                 jmp taster

      iramp1     lda #$b0          ; $b000
                 sta irp1+1        ; reconfig and use last procedure
                 lda #80
                 sta irp2+1
                 LDA #66
                 STA irptx+1
                 jmp irp1
                 
bufmsg           ldy #00
        bmsg_a   lda buffer1,y
                 sta $8170+251,y
                 iny
                 cpy #16
                 bne bmsg_a
                 rts


bufmsg2          ldy #00
        bmsg_2a  lda buffer2,y
                 sta $8170+251,y
                 iny
                 cpy #16
                 bne bmsg_2a
                 rts


buffer1 !PET "firmware $0000  "
buffer2 !PET "option rom $0000"


;------------------------------------------------------------
; Menu-items, text ...
;------------------------------------------------------------
firm_adr !PET "$c000"
firm_adr1 !PET "$b000"
;--------------------------------------------------
opt_adr1 !PET "$9000"
opt_adr2 !PET "$a000"
opt_adr3 !PET "$b000"
;--------------------------------------------------
ct1 !PET "at29c512/at29c010"
ct2 !PET "at29c020/at29c040"
ct3 !PET "at29c256         "
;--------------------------------------------------
mr1 !PET "OFF/on"
mr2 !PET "off/ON"
;--------------------------------------------------
rf1 !PET "FIRMWARE/option"     ; dummy = 1
rf2 !PET "firmware/OPTION"     ; dummy = 0
;--------------------------------------------------
fo1 !PET "BIN/prg"     ; 
fo2 !PET "bin/PRG"     ; 
;--------------------------------------------------
pettext !PET "basic   detected                         "
unktext !PET "basic   unknown.                         "
;--------------------------------------------------

;--------------------------------------------------

driveaddress

          lda drive
          cmp #8
          beq setd9
          lda 33313+5
          sta 33313 
          lda #32
          sta 33313+5          
          lda #8
          sta drive
          rts


setd9     lda #9
          sta drive
          lda 33313
          sta 33313+5           
          lda #32
          sta 33313          
          rts



;-----------------------------------
; close Window
         
close_w  ldy #00
         ldx #00
rew      lda buffer,x
rew1     sta screen,x
         inx
         bne rew           
         iny
         inc rew+2
         inc rew1+2
         cpy #4
         bne rew
         rts        
;-----------------------------------
; buffer


window   lda #$80
         sta rew1+2      ; restore       
         sta rebuf+2
         lda #$2c
         sta rew+2
         sta rebuf1+2

         ldy #00
         ldx #00         ; ersten 3 Zeilen puffern
rebuf    lda screen,x
rebuf1   sta buffer,x
         inx
         bne rebuf
         inc rebuf1+2
         inc rebuf+2
         iny 
         cpy #4
         bne rebuf
         rts

;----------------------------------------------------------------------------
; Flash routines
;----------------------------------------------------------------------------
setflash         lda dummy4           ; firmware =0 / option = 1
                 cmp #00
                 beq firmflash

                 lda dummy1
                 cmp #00
                 beq optfl2
                 cmp #01
                 beq optfl0
                 cmp #02
                 beq optfl1
                  
       optfl0    lda #$00
                 sta source+1
                 sta target+1
                 lda #$40              ; $a000 option ROM located at $4000> 
                 sta source+2
                 lda #$A0
                 sta target+2
                 jmp optf_ex

       optfl1    lda #$00
                 sta source+1
                 sta target+1
                 lda #$50              ; $B000 option ROM located at $5000> 
                 sta source+2
                 lda #$B0
                 sta target+2
                 jmp optf_ex

       optfl2    lda #$00
                 sta source+1
                 sta target+1
                 lda #$30              ; $9000 option ROM located at $3000> 
                 sta source+2
                 lda #$90
                 sta target+2
      optf_ex    lda #$10              ; fix flash-routine for 4K-ROM                 
                 sta start1+1 
                 jsr start             ; start flashing 
                 jmp taster
;                    
; Firmware
;
                 
firmflash        lda dummy3           ; Firmware = 00: $c000 / 01: $b000
                 cmp #01                 
                 beq firmf1 
                 lda #$00
                 sta source+1
                 lda #$00
                 sta target+1
                 lda #$30              ; $C000 ROM located at $3000> 
                 sta source+2
                 lda #$C0
                 sta target+2
                 lda #$40
                 sta start1+1                
                 jsr start
                 jmp taster

        firmf1   lda #$50
                 sta start1+1
                 lda #$00
                 sta source+1
                 lda #$00
                 sta target+1
                 lda #$30              ; $b000 ROM located at $3000> 
                 sta source+2
                 lda #$B0
                 sta target+2
                 jsr start
                 jmp taster
        

start 
        lda dummy5         ; check for bin =0 / prg =1
        cmp #00
        beq start1         ; bin , start with actual addresses
        lda #02            ; prg : fix start-address to skip load-addr.
        sta source+1        
start1  ldx #$40
        sei
prog	   lda #128
        sta page+1
        jsr wpflash
        jsr pgmpage
        jsr waitready
        lda #0
        sta page+1
        jsr wpflash
        jsr testio
        jsr waitready
        inc source+2
        inc target+2
        dex
        bne prog
        cli
        jsr flmsg
        rts

;load 128 bytes to page
pgmpage 	ldy #0
testio	lda target+2
           cmp #$f0
           bcs source
           cmp #$e8
           bcs endprg
source     lda $0000,y
target     sta $0000,y
           iny
  page     cpy #$80
           bne source
endprg     rts

;wait until programming
;is finished
 waitready
                 txa
                 pha
                 tya
                 pha
                 ldx #11
                 ldy #0
        loop     iny
                 bne loop
                 dex
                 bne loop
                 pla
                 tay
                 pla
                 tax
                 rts

;turn off write protect flash
wpflash          nop         ;load data $aa to $5555
                 lda #$aa
                 sta $d555   ;load data $55 to $2aaa
                 lda #$55
                 sta $aaaa
                 lda #$a0    ;load data $a0 to $5555
                 sta $d555
                 rts                 
;--------------------------------------------------
; Message after flashing 
;--------------------------------------------------                                 
                 
flmsg            ldy #00
       flmsg_1   lda fbuffer,y
                 sta $8170+251,y
                 iny
                 cpy #16
                 bne flmsg_1
                 rts

fbuffer !PET " -!- flashed -!-"

;--------------------------------------------------
; exit
;--------------------------------------------------
               
exit_tool        
                                    
                 lda #147
                 jsr chrout
                 jsr ex_msg
ex_               jsr $c38b

ex_msg           ldy #00
        ex_a     lda ex_buf,y
                 sta 32768,y
                 iny
                 cpy #240
                 bne ex_a
                 rts


ex_buf !SCR "Flash Programmer Release : Beta 20060725   Contact : info@cbmhardware.de        "
       !SCR "                                                                                "
       !SCR "run:rem restarts the program ... have a nice day hacker !                       "
                 


;
;
;-----                                                           ----;
;
;
;     
;      
;      Free memory from ?$0900 - $0dff for further expansions         ;
;
;
;
;-----                                                           ----;

*=$0e00
!PET "     flash programmer for cbm basic 4   "
!PET "                                        "
!PET "                                        "
!PET "                                        "
!PET " options:                               "
!PET "                                        "
!PET " (f)ormat  BIN/prg                      "
!PET "                                        "
!PET " (a)ddress $c000                        "
!PET " (r)om     FIRMWARE/option              "
!PET " (c)hiptype : at29c512/at29c010         "
!PET " (m)ove rom to ram                      "
!PET " (w) FLASH                              "
!PET " (l)oad (s)ave   flOppy (*)8 ( )9       "
!PET " (d)irectory                            "
!PET "  buffer($3000)  :  EMPTY               "
!PET "                                        "
!PET " (q)uit program                         "
!PET "                                        "
!PET "                                        "

;----------------------------------------------------
; some text
;----------------------------------------------------
crtmes  !PET "(cbm 8000)"
crtno   !PET "(cbm 4000)"
;-------------------------------------------------- 
; bytes for comparing
;--------------------------------------------------
basic4  !byte $2A,$2A,$2A,$20,$43,$4f,$4D,$4D ;*** comm ; $dea4
        !byte $4F,$44,$4F,$52,$45,$20,$42,$41 ;odore ba 
        !byte $53,$49,$43,$20,$34,$2E,$30,$20 ;sic 4.0 
        !byte $2A,$2A,$2A,$0D,$0D,$00         ;***... 
crtc    !byte $4C,$4B,$E0,$4C,$A7,$E0,$4C,$16,$E1  ; editor rom $e000 (cbm 8000)

;--------------------------------------------------------------
; Filehandling Basic 2 : Load ($3000) Save (Firmware or Option)
;--------------------------------------------------------------
;LOAD            
load             jsr msg
                 lda #$70+12       ; set cursor to line 20 : dez. 33568 - $8320
             cr3 sta 196
                 lda #$83
             cr4 sta 197
         INIT10  LDA #<BUF   
                 STA FNADR   
                 LDA #>BUF  
                 STA FNADR+1     ;PLACE FOR FILE NAME           
                 LDA #8  
                 STA FA          ;DEVICE #8            
                 LDX #0          ;INPUT FILE NAME FROM SCREEN TO BUF  
         INIT20  JSR $FFCF  ;$FFCF  
                 CMP #$D
                 BEQ INIT30
                 STA BUF,X  
                 INX
                 BNE INIT20            
         INIT30  STX FNLEN   
                 LDA #$D
                 JSR $FFD2       ;ECHO CR            
                 LDA FNLEN 
                 BNE INIT40      ;NAME WAS NOT NULL  
                 RTS
         INIT40  lda #$00
                 sta STATUS             
                 
                 lda #15
                 sta $d2
                 ldx drive
                 stx $d4
                 ldy #15
                 sty $d3
                 
                 JSR $F563    ; JSR $F466       ; Open   
                 JSR $F0D5    ; $F0B6           ; Listen
                 LDA SA
                 JSR $F2C1    ; $F128
         lolo    lda #$00     ; Load to $3000 
                 STA $FB
         lohi    lda #$30
                 STA $FC 
                 JSR $F356      ;$F355          
                 lda #15
                 JSR $F2E2


bufmsg3          ldy #00
        bmsg_3a  lda buffer3,y
                 sta $8170+251,y
                 iny
                 cpy #16
                 bne bmsg_3a
                 rts


buffer3 !PET "loaded from disk"
                 
;--------------------------------------------------------             
   msg           ldy #00
         msg_a   lda MSG1,y
                 sta $8370,y
                 iny
                 cpy #39
                 bne msg_a
                 rts
                 
   msg2          ldy #00
         msg2_a  lda #32
                 sta $8370,y
                 iny
                 cpy #39
                 bne msg2_a
                 rts

MSG1 !PET "FILENAME :                              "                     
                
                 
;--------------------------------------------------------             
;SAVE            
 save            jsr msg
                 lda #$70+12       ; set cursor to line 20 : dez. 33568 - $8320
             cr5 sta 196
                 lda #$83
             cr6 sta 197    
         INIT50  LDA #<BUF  
                 STA FNADR  
                 LDA #>BUF  
                 STA FNADR+1     ;PLACE FOR FILE NAME           
                 LDA #8  
                 STA FA          ;DEVICE #8            
                 LDX #0          ;INPUT FILE NAME FROM SCREEN TO BUF  
         INIT60  JSR $FFCF  
                 CMP #$D
                 BEQ INIT70
                 STA BUF,X  
                 INX
                 BNE INIT60            
         INIT70  STX FNLEN   
                 LDA #$D
                 JSR $FFD2       ;ECHO CR      
                 LDA FNLEN 
                 BNE INIT80      ;NAME WAS NOT NULL  
                 RTS
         INIT80  
         
          slo    lda #$00
                 sta $fb
          shi    lda #$30
                 sta $fc
          slo1   lda #$00
                 sta $c9
          shi1   lda #$40
                 sta $ca
                 LDA SA
                 JSR $F2C1    ; 
                 lda #15
                 sta $d2
                 ldx drive
                 stx $d4
                 ldy #15
                 sty $d3
                 JSR $F563   ; Open
                 JSR $F6e3   ; Save                                 
                 lda #15
                 JSR $F2E2   ; Close #15
                 JSR msg
bufmsg4          ldy #00
        bmsg_4a  lda buffer4,y
                 sta $8170+251,y
                 iny
                 cpy #16
                 bne bmsg_4a
                 rts
buffer4 !PET "  saved on disk "
                          



;-------------------------------------------------------------------------
; Check for Firmware or Option and Address
;
;  Option ROM buffer :   $3000>$3fff   -  $4000>$4fff  -   $5000>$5fff 
;            Content :   $9000>$9fff   -  $a000>$afff  -   $b000>$bfff 
;            dummy1  :       #$00             #$01     -       #$02
;
;-------------------------------------------------------------------------

setsave          lda dummy4           ; firmware =0 / option = 1
                 cmp #01
                 bne firmsave
                 lda dummy1
                 cmp #01              ; $a000
                 bne opts1
                 lda #$40             ; $a000 : located in buffer from $4000-4fff
                 sta shi+1
                 lda #$4f             ; $4fff 
                 sta shi1+1
                 lda #$ff
                 sta slo1+1
                 jmp opts_ex
        opts1    lda dummy1
                 cmp #02
                 bne opts2
                 lda #$50             ; $b000 : located in buffer from $5000-5fff
                 sta shi+1
                 lda #$5f             ; $5fff 
                 sta shi1+1
                 lda #$ff
                 sta slo1+1
                 jmp opts_ex
       opts2     lda #$30             ; $9000 : located in buffer from $3000-3fff
                 sta shi+1
                 lda #$3f             ; $5fff 
                 sta shi1+1
                 lda #$ff
                 sta slo1+1
      opts_ex    jsr save
                 jmp taster
;                    
; Firmware
;
                 
 firmsave        lda dummy3           ; Firmware = 01: $c000 / 00: $b000
                 cmp #01                 
                 bne firms1 
                 lda #$80
                 sta shi1+1
                 jsr save
                 jmp taster
        firms1   Lda #$70             ; $B000
                 sta shi1+1         
                 jsr save                  
                 jmp taster                 
                 
;-------------------------------------------------------------------------
; Check for Firmware or Option and Address
;
;  Option ROM buffer :   $3000>$3fff   -  $4000>$4fff  -   $5000>$5fff 
;            Content :   $9000>$9fff   -  $a000>$afff  -   $b000>$bfff 
;            dummy1  :       #$00             #$01     -       #$02
;
;-------------------------------------------------------------------------

setload          lda dummy4           ; firmware =0 / option = 1
                 cmp #01
                 bne firmload
                 lda dummy1
                 cmp #01              ; $a000
                 bne lopts1
                 lda #$40             ; $a000 : located in buffer from $4000-4fff
                 sta lohi+1
                 lda #$00
                 sta lolo+1
                 jmp lopts_ex
       lopts1    lda dummy1
                 cmp #02
                 bne lopts2
                 lda #$50             ; $b000 : located in buffer from $5000-5fff
                 sta lohi+1
                 lda #$00             ; $5fff 
                 sta lolo+1
                 jmp lopts_ex
      lopts2     lda #$30             ; $9000 : located in buffer from $3000-3fff
                 sta lohi+1
                 lda #$00             ; $5fff 
                 sta lolo+1
     lopts_ex    jsr load
                 jmp taster
;                    
; Firmware
;
                 
 firmload        lda dummy3           ; Firmware = 01: $c000 / 00: $b000
                 cmp #01                 
                 bne lfirms1 
                 lda #$30
                 sta lohi+1
                 jsr load
                 jmp taster
       lfirms1   Lda #$30             ; $B000
                 sta lohi+1         
                 jsr load                  
                 jmp taster                 
                 
                 
; end of source : $15a0 , free space up to $2f00

; buffer for screen-windowing : $2c00-$2f00





; 2f00 - 2fff reserved for data storage : filename etc.                     



                                  
                 
                 
                 
                 
                 
