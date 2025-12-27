catalog:
          lda basic
          cmp #$04
          bne +
          jmp dir_b4
+         jmp keyboard                      

;--------------------------------------------------
; Directory / Catalog Basic 4
;-------------------------------------------------    
dir_b4:
          lda #$04
          sta blocks
          lda #<screen_buffer 
          sta t_lo
          lda #>screen_buffer
          sta t_hi
          lda #$00
          sta s_lo
          lda #$80
          sta s_hi
          jsr copy      
          lda #$93
          jsr bsout
          ldx drive
          stx b_drive  
          jsr b4_catalog
-         jsr get
          cmp #$20
          bne -
rebuild:
          lda #<screen_buffer 
          sta s_lo
          lda #>screen_buffer
          sta s_hi
          lda #$00
          sta t_lo
          lda #$80
          sta t_hi
          jsr copy
          jmp keyboard          
;--------------------------------------------------
; Save Basic 1
;
; frm_optn: $00/$01
;
; frm_addr: $00 : $9000
;           $01 : $a000
;           $02 : $b000
;           $03 : $c000
;-------------------------------------------------    
bas1_savefile:
          lda #$04
          sta blocks
          lda #<screen_buffer 
          sta t_lo
          lda #>screen_buffer
          sta t_hi
          lda #$00
          sta s_lo
          lda #$80
          sta s_hi
          jsr copy      
          lda #$93
          jsr bsout
           
          lda #<fn_buf   
          sta bas1_fnadr
          lda #>fn_buf  
          sta bas1_fnadr+1 
          lda drive  
          sta bas1_dn

          ldx #$00
          stx bas1_start
          sta bas1_end
          lda #$30
          sta bas1_start+1
          
          ldy #$00
          
-         lda fn,y
          jsr bsout
          iny
          cpy #$0a
          bne -

          ldx #$00
-         jsr basin
          cmp #$0d
          beq +
          sta fn_buf,x
          inx
          bne -
+         stx bas1_fnlen 

          lda frm_optn
          beq +
          lda #$40
          sta bas1_end+1
          jsr bas1_save 
          jsr rebuild
          rts
;firmware
+         lda frm_addr
          cmp #$02            ; $b000
          bne +
          lda #$80
          sta bas1_end+1
          jsr bas1_save 
          jsr rebuild
          rts
+                    
          lda #$70            ; $c000
          sta bas1_end+1
          jsr bas1_save 
          jsr rebuild
          rts
;-----------------------------------------------------------------------
; Basic 2 Disk-Save
;-----------------------------------------------------------------------
bas2_savefile:
          jsr save_file
          lda #$60
          sta bas2_sa     
          jsr bas2_open     
          jsr bas2_talk                      
          jsr bas2_save                      
          rts
;-----------------------------------------------------------------------
; Basic 4 Disk-Save
;-----------------------------------------------------------------------
bas4_savefile:
          jsr save_file 
          jsr bas4_open
          jsr bas4_save
          lda #$0f
          jsr bas4_close
          ldy #$00
-         lda #$20
          sta $83ba+10,y
          iny
          cpy #$14
          bne -
          rts 
;-----------------------------------------------------------------------
; Save Setups
;-----------------------------------------------------------------------
save_file:
          lda drive  
          sta bas4_dn
          ldx #$00
          stx bas4_start
          sta bas4_end
          lda #$30
          sta bas4_start+1
          ldy #$00
          jsr bas4_input
          lda #$00
          sta bas4_start          
          sta bas4_end
          lda #$30
          sta bas4_start+1
          lda frm_optn
          beq +
          lda #$40            ; 4kB Option-ROM
          sta bas4_end+1
          jmp ++ 
+         lda frm_addr
          cmp #$02
          beq +
          lda #$70
          sta bas4_end+1
          jmp ++
+         lda #$80
          sta bas4_end+1
++        lda bas4_sa
          lda #$0f
          sta bas4_lf
          ldx drive
          stx bas4_dn
          ldy #$0f
          sty bas4_sa
          rts 
;-----------------------------------------------------------------------
; Basic 1 Tapeload
;-----------------------------------------------------------------------        
bas1_loadfile:
          lda #$04
          sta blocks
          lda #<screen_buffer 
          sta t_lo
          lda #>screen_buffer
          sta t_hi
          lda #$00
          sta s_lo
          lda #$80
          sta s_hi
          jsr copy      
          lda #$93
          jsr bsout
          lda drive  
          sta bas1_dn
          ldy #$00
          stx bas1_start
          lda #$30
          sta bas1_start+1
          lda #<fn_buf   
          sta bas1_fnadr
          lda #>fn_buf  
          sta bas1_fnadr+1 
          lda drive  
          sta bas1_dn
-         lda fn,y
          jsr bsout
          iny
          cpy #$0a
          bne -
          ldx #$00
-         jsr basin
          cmp #$0d
          beq +
          sta fn_buf,x
          inx
          bne -
+         stx bas1_fnlen 
          lda #$00
          sta bas1_status
          lda #$01 
          sta bas1_direct     
          ldx #$00            
          jsr $f362
          lda #<screen_buffer 
          sta s_lo
          lda #>screen_buffer
          sta s_hi
          lda #$00
          sta t_lo
          lda #$80
          sta t_hi
          lda #$04
          sta blocks
          jsr copy
          ldy #$00          
-         lda c2nload,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
          jmp keyboard
;-----------------------------------------------------------------------
; BASIC 2 Load File
;-----------------------------------------------------------------------
bas2_loadfile:

          lda #$00
          sta status          
          jsr bas4_input
          lda drive
          sta b_drive
          lda #$60
          sta sa 
          jsr $f466
          jsr $F0B6          
          LDA sa
          jsr $F128
          lda #$00           
          sta $FB
          lda #$30
          sta $FC 
          jsr $F355             
          jsr msg  
          jmp keyboard
;-----------------------------------------------------------------------
; BASIC 4 Load File
;-----------------------------------------------------------------------

bas4_loadfile:
          lda #$00
          sta status          
          jsr bas4_input        
          lda #$0f
          sta bas4_lf
          ldx drive
          stx bas4_dn
          ldy #$0f
          sty bas4_sa           
          jsr bas4_open                              
          jsr bas4_listen
          lda bas4_sa         
          jsr bas4_ftab         
          lda #$00     
          sta bas4_start 
          lda #$30
          sta bas4_start+1    ; Load to $3000               
          jsr bas4_load 
          lda #$0f
          jsr bas4_close
          jsr msg          
          jmp keyboard
;-----------------------------------------------------------------------
; Filename Input for BASIC 2 und 4
;-----------------------------------------------------------------------
        
bas4_input:         
          lda #<fn_buf   
          sta bas4_fnadr
          lda #>fn_buf  
          sta bas4_fnadr+1 
          lda #$ce
          sta bas4_cur
          lda #$83
          sta bas4_cur+1
          ldy #$00
          sty fnlen 
-         lda fn,y
          sta $83ba+10,y
          iny
          cpy #$14
          bne -
          lda #$ce
          sta bas4_cur
          lda #$83
          sta bas4_cur+1                
          ldx #$01
          stx bas4_col
          dex
          stx fnlen 
-         jsr basin
          cmp #$0d
          beq +
          sta fn_buf,x
          inx
          bne -
+         stx fnlen 
          lda #$0d
          sta bsout
          lda fnlen
          beq +           
          rts
+         jsr clrmsg 
          jmp keyboard         
;-----------------------------------------------------------------------
; Text Output and Clear
;-----------------------------------------------------------------------        
msg:      ldy #$00
-         lda diskload,y
          sta $82b9,y
          iny
          cpy #$10
          bne -
clrmsg:   lda #$20
          sta $83ba-11,y
          iny
          cpy #$34
          bne clrmsg
          rts
