ui:
!PET "   ------------------------------------ "
!PET "                                        "
!PET "  - flash-programmer for cbm and pet -  "
!PET "                                        "
!PET " ------------------------------------   "
!PET "                                        "
!PET "                                        "
!PET "                                        "
!PET " (f)lash memory                         "
!PET " (d)rive <8>                            "
!PET " (a)ddress $b000                        "
!PET " (r)om     FIRMWARE/option              "
!PET " (i)nfo-screen to dip-switches          "
!PET " (m)ove rom to ram                      "
!PET " (w)rite flash                          "
!PET " (l)oad (s)ave (c)atalog                "
!PET "                                        "
!PET "  buffer($3000):    EMPTY               "
!PET "                                        "
!PET " (q)uit program                         "
!PET "                                        "
!PET "                                        "
!PET "   computer: cbm  0 column basic        "
!PET "                                        "
!PET "                                        "
!PET "                           -------------"

infoscreen: 
!PET "  rom-ram adapter dip-switches          "
!PET "                                        "
!PET "  1 :  enable flash-mode                "
!PET "  2 :  enable rom                       "
!PET "  3 :  enable ram                       "
!PET "  4 :  bit 0 ram 00: 4kb 01: 8kb        "
!PET "  5 :  bit 1 ram 10:16kb 11:32kb        "
!PET "  6 :  option rom   bit 0               "
!PET "  7 :               bit 1               "
!PET "  8 :  main rom     bit 0               "
!PET "  9 :               bit 1               "
!PET " 10 :  if exists one more bit for main  "
!PET "       rom bank                         "
!PET "                                        "
!PET "use switches 4+5, 6+7 and 8+9 (and 10)  "
!PET "            together                    "
!PET "                                        "
!PET "        press space to return           "
!PET "                                        "
!PET "                                        "          
roms_b:  !PET "roms $b000-$ffff"
roms_c:  !PET "roms $c000-$ffff"

opt9000: !PET "option rom $9000"
opta000: !PET "option rom $a000"
optb000: !PET "option rom $b000"

_ex !PET"have a nice day hacker !"          

ed_tab:
!by $4c,$4b,$e0,$4c,$a7,$e0,$4c,$16,$e1,$4c,$02,$e2,$4c,$42,$e4,$4c

opt_addr: !by $00
bin_prg:  !by $00
basic     !by $00

fn: !PET " filename:"
    !PET "          "
flashed:
    !PET "buffer flashed < press space >"
c2nload: !PET " file from tape "
diskload: !PET " file from disk "

    
    
    
