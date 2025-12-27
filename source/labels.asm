;-----------------------------------------------------------------------
; Basic 1
;-----------------------------------------------------------------------

    bas1_fnadr  = $f9 
    bas1_start  = $f7    
    bas1_end    = $e5
    bas1_load   = $f362
    bas1_save   = $f6b1
    bas1_dn     = $f1       
    bas1_fnlen  = $ee       
    bas1_status = $020c
    bas1_direct = $ca


;-----------------------------------------------------------------------
; Basic 2
;-----------------------------------------------------------------------

    bas2_fnlen  = $d1
    bas2_acptr  = $f18c
    bas2_chkin  = $ffc6
    bas2_chkout = $ffc9
    bas2_ciout  = $f167  
    bas2_close  = $f2ac
    bas2_listn  = $f0ba
    bas2_load   = $f3c9
    bas2_loadop = $f322
    bas2_open   = $f466    
    bas2_secnd  = $f128
    bas2_sett   = $f299
    bas2_talk   = $f0b6
    bas2_twait  = $f8e6
    bas2_unlsn  = $f183
    bas2_untlk  = $f17f 
    bas2_intout = $dcd9
    bas2_setcur = $e25d 
    bas2_save   = $f6a4
    bas2_fnadr  = $da    
    bas2_lvflag = $020b
    bas2_fnlen  = $d1 
    bas2_lf     = $d2
    bas2_sa     = $d3
    bas2_dn     = $d4   
   
;-----------------------------------------------------------------------
; Basic 2 and 4
;-----------------------------------------------------------------------

    status = $96
   b_drive = $d4 
    fnadr  = $da 
    fnlen  = $d1 
    sa     = $d3     
    
;-----------------------------------------------------------------------
; Basic 1,2 and 4
;-----------------------------------------------------------------------

    bsout = $ffd2 
    get   = $ffe4
    basin = $ffcf   
    screen = $8000  
    editor = $e000
    irq_v  = $fffe 	
    reset_v = $fffc 

;-----------------------------------------------------------------------
; Basic 4
;-----------------------------------------------------------------------

    b4_catalog  = $d87d
    bas4_cur    = $c4
    bas4_col    = $c6
    bas4_ftab   = $f2c1
    bas4_open   = $f563
    bas4_load   = $f356 
    bas4_save   = $f6e3  
    bas4_close  = $f2e2  
    bas4_listen = $f0d5
    bas4_fnlen  = $d1 
    bas4_lf     = $d2
    bas4_sa     = $d3
    bas4_dn     = $d4   
    bas4_fnadr  = $da 
    bas4_start  = $fb
    bas4_end    = $c9    

;-----------------------------------------------------------------------
; Setup
;-----------------------------------------------------------------------

    fn_buf    = $033a
    buf       = $3000 
    checkbyte = $0e
    temp      = $0f
    crtc_col  = $18    
    flash_pg  = $07     
    drive     = $a2    
    frm_addr  = $b3
    frm_optn  = $b7
    blocks    = $ff 

;---------------------------------------------------
;ROM>RAM Copy

         s_lo = $0a  
         s_hi = $0b
         t_lo = $0c
         t_hi = $0d
