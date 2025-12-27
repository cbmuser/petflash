;-------------------------------------------------    
; frm_optn: $00/$01 
; 
; frm_addr: $00 : $9000  4096 bytes
;           $01 : $a000  4096 bytes
;           $02 : $b000  4096 or 20480 bytes 
;           $03 : $c000  16384 bytes
;
; flash_pg: page-size 64, 128 or 256 byte  
;           set by menu 
;
;-------------------------------------------------    
flash_write: 	   


          lda #$00
          tax 
          sta s_lo
          sta t_lo
          lda #$30
          sta s_hi
          jsr wp              ; relock

          lda frm_optn 
          beq +
          lda frm_addr        ; option rom 
          tax
          lda rom_s,x         ; fetch hi-byte
          sta t_hi
          jmp flash         



+         lda frm_addr        ; rom  
          tax   
          lda rom_s,x         ; fetch hi-byte
          sta t_hi
          inx
          inx                 ; fetch end  from rom_e ? 
flash:    


           




; your code here ! 




            rts






;-------------------------------------------------    
; wait until programming is finished
;-------------------------------------------------    

waitready:                    

          txa
          pha
          ldx #$10
          ldy #$04
-         iny
          bne -
          dex
          bne -
          pla
          tax
          rts
;-------------------------------------------------    
; unlock flash
;-------------------------------------------------    

wp:       nop                 ;load data $aa to $5555
          lda #$aa
          sta $d555           ;load data $55 to $2aaa
          lda #$55
          sta $aaaa
          lda #$a0            ;load data $a0 to $5555
          sta $d555
          rts                 

; ROM start and end

rom_s: !by $90, $a0, $b0, $c0
rom_e: !by $a0, $b0, $c0, $00, $00, $00
