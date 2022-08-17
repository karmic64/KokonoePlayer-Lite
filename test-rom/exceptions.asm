BGABASE = $a000
BGBBASE = $c000
WINDOWBASE = $e000
SATBASE = $f000
HSCROLLBASE = $f400

SCREENWIDTH = 64
SCREENWIDTH_SHIFT = 7

SCREENHEIGHT = 32
    
    
init_vdp_regs:
    lea VDPCTRL,a0
    SetVDPRegsA 4,0, $24,1, (a0)
    SetVDPRegsA (BGABASE>>10),2, (BGBBASE>>13),4, (a0)
    SetVDPRegsA (WINDOWBASE>>10),3, (SATBASE>>9),5, (a0)
    SetVDPRegsA 0,6, 0,7, (a0)
    SetVDPRegsA 0,8, 0,9, (a0)
    SetVDPRegsA 0,11, $81,12, (a0)
    SetVDPRegsA (HSCROLLBASE>>10),13, 0,14, (a0)
    SetVDPRegsA 2,15, $01,16, (a0)
    SetVDPRegsA 0,17, 0,18, (a0)
    rts
    
    
    
    
    
init_vdp_transfer:
    lea VDPDATA,a1
    lea 4(a1),a0
    rts
    
    
wait_vdp_dma:
    btst.b #1,1(a0)
    bne wait_vdp_dma
    rts
    
    
    
    
clear_vram:
    bsr init_vdp_transfer
    
    moveq #0,d0
    
    
    SetVDPAddrA 0,VRAM_WRITE,(a0)
    move.w #($10000/4)-1,d7
.vram:
    move.l d0,(a1)
    dbra d7,.vram
    
    
    SetVDPAddrA 0,CRAM_WRITE,(a0)
    move.w #($80/4)-1,d7
.cram_2:
    move.l d0,(a1)
    dbra d7,.cram_2
    
    
    
    SetVDPAddrA 0,VSRAM_WRITE,(a0)
    moveq #($50/4)-1,d7
.vsram:
    move.l d0,(a1)
    dbra d7,.vsram
    rts
    
    
    
    
    
init_cram:
    bsr init_vdp_transfer
    
    SetVDPAddrA 0,CRAM_WRITE, (a0)
    
    moveq #0,d0
.l1 move.w d0,(a1)
    addi.w #$1111,d0
    bcc .l1
    
    moveq #0,d0
.l2 move.w d0,(a1)
    addi.w #$1001,d0
    bcc .l2
    
    moveq #0,d0
.l3 move.w d0,(a1)
    addi.w #$1010,d0
    bcc .l3
    
    moveq #0,d0
.l4 move.w d0,(a1)
    addi.w #$1100,d0
    bcc .l4
    
    rts
    
    
    
    
    
    
upload_font:
    bsr init_vdp_transfer
    
    SetVDPAddrA 0,VRAM_WRITE, (a0)
    
    move.w #($80*8)-1,d7
    lea font_data,a2
    
.byteloop:
    move.b (a2)+,d0
    moveq #0,d1
    moveq #8-1,d6
.pixloop:
    lsl.l #4,d1
    add.b d0,d0
    bcc .skippix
    ori.b #$0f,d1
.skippix:
    
    dbra d6,.pixloop
    
    move.l d1,(a1)
    dbra d7,.byteloop
    
    rts
    
    
    
    
    
    ; in: A2=string ptr, D0=VRAM ptr (0 to skip), D1=nametable OR value, D2+ =format parameters
    ;out: A0=VDPCTRL, A1=VDPDATA, A2=string end ptr
FMT_CHAR = $80
FMT_NIB = $81
FMT_BYTE = $82
FMT_WORD = $83
FMT_24 = $84
FMT_LONG = $85
FMT_DEC = $86
output_string_normal:
    moveq #0,d1
output_string:
    movem.l d0/d2-d7,-(a7)
    
    bsr init_vdp_transfer
    
    tst.w d0
    beq .skip_addr
    ConvVDPAddr VRAM_WRITE,d0
    move.l d0,(a0)
.skip_addr:
    
.loop:
    moveq #0,d0
    move.b (a2)+,d0
    bmi .format
    beq .end
.normal:
    or.w d1,d0
    move.w d0,(a1)
    bra .loop
.format:
    cmpi.b #FMT_NIB,d0
    beq .nib
    cmpi.b #FMT_BYTE,d0
    beq .byte
    cmpi.b #FMT_WORD,d0
    beq .word
    cmpi.b #FMT_24,d0
    beq ._24
    cmpi.b #FMT_LONG,d0
    beq .long
    cmpi.b #FMT_DEC,d0
    beq .dec
    bsr .get_next ;char
    bra .normal
    
.nib:
    bsr .get_next
    bsr output_nib
    bra .loop
.byte:
    bsr .get_next
    bsr output_byte
    bra .loop
.word:
    bsr .get_next
    bsr output_word
    bra .loop
._24:
    bsr .get_next
    bsr output_24
    bra .loop
.long:
    bsr .get_next
    bsr output_long
    bra .loop
.dec:
    bsr .get_next
    bsr output_dec
    bra .loop
    
.end:
    movem.l (a7)+, d0/d2-d7
    rts
    
    
.get_next:
    move.l d2,d0
    move.l d3,d2
    move.l d4,d3
    move.l d5,d4
    move.l d6,d5
    move.l d7,d6
    rts
    
    
    ;all of these take the value in D0 and nametable OR in D1
output_byte:
    ror.l #4,d0
    bsr output_nib
    rol.l #4,d0
    bra output_nib
    
    
output_word:
    swap d0
    bra output_upper_word
    
    
output_24:
    rol.l #8,d0
    bsr output_upper_byte
    bra output_upper_word
    
    
output_long:
    bsr output_upper_word
output_upper_word:
    bsr output_upper_byte
output_upper_byte:
    rol.l #4,d0
    bsr output_nib
    rol.l #4,d0
    ;... fall through ...
output_nib:
    move.l d0,-(a7)
    lea output_num_tbl,a0
    andi.l #$0f,d0
    move.b (a0,d0),d0
    or.w d1,d0
    move.w d0,(a1)
    lea 4(a1),a0
    move.l (a7)+,d0
    rts
    
    
    
    ;(doesn't support anything larger than 99999)
output_dec:
    movem.l d2-d4/a2, -(a7)
    
    moveq #0,d2 ;nonzero flag
    moveq #0,d3
    move.w #10000,d3 ;current divisor
    lea output_num_tbl, a2
    
.loop:
    divu.w d3,d0
    
    ;quotient is the digit to print
    tst.b d2 ;if a nonzero digit has been printed before, always print it
    bne .always
    cmpi.w #1,d3 ;or if the divisor is 1 (this is the last digit)
    beq .always
    tst.w d0
    beq .skip
.always:
    moveq #1,d2
    ;output num in low-word of d0
    moveq #0,d4
    move.b d0,d4
    move.b (a2,d4), d4
    or.w d1,d4
    move.w d4,(a1)
.skip:
    clr.w d0 ;remainder is the new dividend
    swap d0
    
    divu.w #10,d3 ;end?
    bne .loop
    
    movem.l (a7)+, d2-d4/a2
    rts
    
    
output_num_tbl:
    db "0123456789ABCDEF"
    
    
    
    
full_init_vdp:
    bsr init_vdp_regs
    bsr clear_vram
    bsr init_cram
    bra upload_font
    
    
    
    
    ;exceptions
ex_bus_error:
    move.l #em_bus_error,-(a7)
    bra exception_main
ex_address_error:
    move.l #em_address_error,-(a7)
    bra exception_main
ex_illegal_instruction:
    move.l #em_illegal_instruction,-(a7)
    bra exception_main
ex_division_by_zero:
    move.l #em_division_by_zero,-(a7)
    bra exception_main
ex_chk:
    move.l #em_chk,-(a7)
    bra exception_main
ex_trapv:
    move.l #em_trapv,-(a7)
    bra exception_main
ex_privilege_violation:
    move.l #em_privilege_violation,-(a7)
    bra exception_main
ex_trace:
    move.l #em_trace,-(a7)
    bra exception_main
ex_unimplemented_a:
    move.l #em_unimplemented_a,-(a7)
    bra exception_main
ex_unimplemented_f:
    move.l #em_unimplemented_f,-(a7)
    bra exception_main
ex_uninitialized:
    move.l #em_uninitialized,-(a7)
    bra exception_main
ex_i0:
    move.l #em_spurious_interrupt,-(a7)
    bra exception_main
    ;abuse the upper byte of the vector for the interrupt level
ex_i1:
    move.l #em_interrupt + (1 << 24),-(a7)
    bra exception_main
ex_i2:
    move.l #em_interrupt + (2 << 24),-(a7)
    bra exception_main
ex_i3:
    move.l #em_interrupt + (3 << 24),-(a7)
    bra exception_main
ex_i4:
    move.l #em_interrupt + (4 << 24),-(a7)
    bra exception_main
ex_i5:
    move.l #em_interrupt + (5 << 24),-(a7)
    bra exception_main
ex_i6:
    move.l #em_interrupt + (6 << 24),-(a7)
    bra exception_main
ex_i7:
    move.l #em_interrupt + (7 << 24),-(a7)
    bra exception_main
ex_trap0:
    move.l #em_trap + (0 << 24),-(a7)
    bra exception_main
ex_trap1:
    move.l #em_trap + (1 << 24),-(a7)
    bra exception_main
ex_trap2:
    move.l #em_trap + (2 << 24),-(a7)
    bra exception_main
ex_trap3:
    move.l #em_trap + (3 << 24),-(a7)
    bra exception_main
ex_trap4:
    move.l #em_trap + (4 << 24),-(a7)
    bra exception_main
ex_trap5:
    move.l #em_trap + (5 << 24),-(a7)
    bra exception_main
ex_trap6:
    move.l #em_trap + (6 << 24),-(a7)
    bra exception_main
ex_trap7:
    move.l #em_trap + (7 << 24),-(a7)
    bra exception_main
ex_trap8:
    move.l #em_trap + (8 << 24),-(a7)
    bra exception_main
ex_trap9:
    move.l #em_trap + (9 << 24),-(a7)
    bra exception_main
ex_trap10:
    move.l #em_trap + (10 << 24),-(a7)
    bra exception_main
ex_trap11:
    move.l #em_trap + (11 << 24),-(a7)
    bra exception_main
ex_trap12:
    move.l #em_trap + (12 << 24),-(a7)
    bra exception_main
ex_trap13:
    move.l #em_trap + (13 << 24),-(a7)
    bra exception_main
ex_trap14:
    move.l #em_trap + (14 << 24),-(a7)
    bra exception_main
ex_trap15:
    move.l #em_trap + (15 << 24),-(a7)
    bra exception_main
    
    
exception_main:
		move #$2700,sr
    movem.l d0-d7/a0-a6,-(a7)
    
    bsr full_init_vdp
    
    
    ;data registers
    move.l #BGABASE+(SCREENWIDTH*2*9)+8,d0
    moveq #'D',d2
    moveq #'0',d3
.dloop:
    lea .str_reg,a2
    move.l (a7)+,d4
    bsr output_string_normal
    addi.l #SCREENWIDTH*2,d0
    addq.b #1,d3
    cmpi.b #'8',d3
    blo .dloop
    
    
    
    ;address registers
    addi.l #SCREENWIDTH*2,d0
    moveq #'A',d2
    moveq #'0',d3
.aloop:
    lea .str_reg,a2
    move.l (a7)+,d4
    bsr output_string_normal
    addi.l #SCREENWIDTH*2,d0
    addq.b #1,d3
    cmpi.b #'7',d3
    blo .aloop
    
    
    ;exception message
    move.l (a7)+,a6
    move.l a6,d0
    move.l a6,d1
    rol.l #8,d0
    
    andi.l #$ffffff,d1
    movea.l d1,a2
    
    moveq #0,d2
    move.b d0,d2
    move.l #(BGABASE+(SCREENWIDTH*2*2)+4),d0
    move.w #$2000,d1
    bsr output_string
    
    
    ;special bus/address error information
    cmpa.l #em_bus_error,a6
    beq .dospecial
    cmpa.l #em_address_error,a6
    bne .skipspecial
.dospecial:
    ;first word
    move.w (a7)+, d7
    
    ;w/r
    move.b #'W',d2
    btst #4,d7
    beq .noread
    move.b #'R',d2
.noread:
    
    ;i/n
    move.b #'I',d3
    btst #3,d7
    beq .nonot
    move.b #'N',d3
.nonot:
    
    ;fn
    moveq #0,d4
    move.b d7,d4
    andi.b #7,d4
    
    ;address
    move.l (a7)+,d5
    
    ;instruction register
    move.w (a7)+,d6
    
    ;print
    move.l #BGABASE+(SCREENWIDTH*2*3)+24, d0
    lea .str_address,a2
    bsr output_string_normal
    
.skipspecial:
    
    
    
    ;status register
    move.l #BGABASE+(SCREENWIDTH*2*6)+6,d0
    move.w (a7)+,d2
    lea .str_sr,a2
    bsr output_string_normal
    
    ;program counter
    move.l #BGABASE+(SCREENWIDTH*2*5)+6,d0
    moveq #'P',d2
    moveq #'C',d3
    move.l (a7)+,d4
    lea .str_reg,a2
    bsr output_string_normal
    
    
    ;stack pointer
    move.l #BGABASE+(SCREENWIDTH*2*25)+8,d0
    moveq #'A',d2
    moveq #'7',d3
    move.l a7,d4
    lea .str_reg,a2
    bsr output_string_normal
    
    
    ;end
    SetVDPReg $44,1
    bra *
    
    
.str_sr:
    db "SR:$",FMT_WORD,0
.str_reg:
    db FMT_CHAR,FMT_CHAR,":$",FMT_LONG,0
.str_address:
    db FMT_CHAR," ",FMT_CHAR," ","Fn:",FMT_NIB," $",FMT_LONG," IR:$",FMT_WORD,0
    
    
    
em_bus_error:
    db "Bus error",0
em_address_error:
    db "Address error",0
em_illegal_instruction:
    db "Illegal instruction",0
em_division_by_zero:
    db "Division by zero",0
em_chk:
    db "CHK out of bounds",0
em_trapv:
    db "TRAPV overflow",0
em_privilege_violation:
    db "Privilege violation",0
em_trace:
    db "Trace",0
em_unimplemented_a:
    db "Unimplemented instruction A",0
em_unimplemented_f:
    db "Unimplemented instruction F",0
em_uninitialized:
    db "Uninitialized interrupt",0
em_spurious_interrupt:
    db "Spurious interrupt",0
em_interrupt:
    db "Interrupt level ",FMT_DEC,0
em_trap:
    db "TRAP #",FMT_DEC,0
    
font_data:
    incbin "font.rom",0,8*$80
    
    align 1