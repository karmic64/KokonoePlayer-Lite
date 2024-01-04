	code
;i/o
VERSION     = $a10001
PORT1DATA   = $a10003
PORT2DATA   = $a10005
EXPDATA     = $a10007
PORT1CTRL   = $a10009
PORT2CTRL   = $a1000b
EXPCTRL     = $a1000d
PORT1TxDATA = $a1000f
PORT1RxDATA = $a10011
PORT1SCTRL  = $a10013
PORT2TxDATA = $a10015
PORT2RxDATA = $a10017
PORT2SCTRL  = $a10019
EXPTxDATA   = $a1001b
EXPRxDATA   = $a1001d
EXPSCTRL    = $a1001f
MEMMODE     = $a11000
TMSSCTRL    = $a14000
VDPDATA     = $c00000
VDPCTRL     = $c00004
HVCOUNTER   = $c00008

Z80BUSREQ = $a11100


;vdpctrl transfer id codes
              ;10......................5432....
VRAM_WRITE  = %01000000000000000000000000000000
CRAM_WRITE  = %11000000000000000000000000000000
VSRAM_WRITE = %01000000000000000000000000010000
VRAM_DMA    = %01000000000000000000000010000000
CRAM_DMA    = %11000000000000000000000010000000
VSRAM_DMA   = %01000000000000000000000010010000
VRAM_READ   = %00000000000000000000000000000000
CRAM_READ   = %00000000000000000000000000100000
VSRAM_READ  = %00000000000000000000000000010000

            ;params: CONSTANT address, transfer type
SetVDPAddr  macro
            if (\1) < 0 || (\1) > $ffff
                fail "VRAM addresses must be from $0000-$ffff"
            endif
            if NARG != 2
                fail "Bad argument count"
            endif
            move.l #(((\1) & $3fff) << 16) | (((\1) & $3c000) >> 14) | (\2), VDPCTRL
            endm
            
            ;params: CONSTANT address, transfer type, addressing mode
SetVDPAddrA macro
            if (\1) < 0 || (\1) > $ffff
                fail "VRAM addresses must be from $0000-$ffff"
            endif
            if NARG != 3
                fail "Bad argument count"
            endif
            move.l #(((\1) & $3fff) << 16) | (((\1) & $3c000) >> 14) | (\2), \3
            endm
            
            ;params: transfer type, data register
ConvVDPAddr macro
            if NARG != 2
                fail "Bad argument count"
            endif
            ;andi.l #$ffff, \2
            lsl.l #2, \2
            lsr.w #2, \2
            swap \2
            if (\1)
            ori.l #(\1), \2
            endif
            endm
            
            ;params: constant value, register number
SetVDPReg   macro
            if NARG != 2
                fail "Bad argument count"
            endif
            move.w #$8000 | ((\2) << 8) | (\1), VDPCTRL
            endm
            ;params: constant value, register number, addressing mode
SetVDPRegA  macro
            if NARG != 3
                fail "Bad argument count"
            endif
            move.w #$8000 | ((\2) << 8) | (\1), \3
            endm
            
            ;this is for setting two vdp registers at once
            ;params: src1,dest1, src2,dest2
SetVDPRegs  macro
            if NARG != 4
                fail "Bad argument count"
            endif
            move.l #(($8000 | ((\2) << 8) | (\1)) << 16) | ($8000 | (\4 << 8) | (\3)), VDPCTRL
            endm
            ;params: src1,dest1, src2,dest2, addressing mode
SetVDPRegsA macro
            if NARG != 5
                fail "Bad argument count"
            endif
            move.l #(($8000 | ((\2) << 8) | (\1)) << 16) | ($8000 | (\4 << 8) | (\3)), \5
            endm
	
	
	
	section .header
	org 0
	
	dl $ffff8000
	dl reset
	
    dl ex_bus_error
    dl ex_address_error
    dl ex_illegal_instruction
    dl ex_division_by_zero
    dl ex_chk
    dl ex_trapv
    dl ex_privilege_violation
    dl ex_trace
    dl ex_unimplemented_a
    dl ex_unimplemented_f
    dcb.b $3c-*,0
    dl ex_uninitialized
    dcb.b $60-*,0
    dl ex_i0
    dl ex_i1
    dl ex_i2
    dl ex_i3
    dl ex_i4
    dl ex_i5
    dl ex_i6
    dl ex_i7
    dl ex_trap0
    dl ex_trap1
    dl ex_trap2
    dl ex_trap3
    dl ex_trap4
    dl ex_trap5
    dl ex_trap6
    dl ex_trap7
    dl ex_trap8
    dl ex_trap9
    dl ex_trap10
    dl ex_trap11
    dl ex_trap12
    dl ex_trap13
    dl ex_trap14
    dl ex_trap15
	
  dcb.b $100-*,0
	db "SEGA MEGA DRIVE "
	db "KARMIC  2022.AUG"
	db "SOUND DEMO                                      "
	db "SOUND DEMO                                      "
	db "SN 00000000-00"
	dw $DEAD
	db "JD              "
	dl 0,rom_end-1
	dl $ff0000,$ffffff
	db "            "
	db "            "
	db "                                        "
	db "U               "
	
	
	
	section .code,code
	
	include "exceptions.asm"
	
	
	pushsection
	section .bss,bss
	
rawjoy
	dcb.b 1
joy
	dcb.b 1
	
song_num
	dcb.b 1
	
	popsection
	
	
	
_start::
reset:
	move #$2700,sr
	
	move.b VERSION,d0
	andi.b #$0f,d0
	beq .skiptmss
	move.l $100,TMSSCTRL
.skiptmss
	
	
	jsr knl_reset
	
	
	lea $a10000,a0
	lea Z80BUSREQ,a1
	move.w #$0100,(a1)
	move.b #$40,d0
	move.b d0,$9(a0)
	move.b d0,$b(a0)
	move.b d0,$3(a0)
	move.b d0,$5(a0)
	clr.w (a1)
	
	clr.b song_num
	st rawjoy
	
	
	bsr full_init_vdp
	SetVDPRegA $44,1, (a0)
	
	
	
mainloop:
	cmpi.b #$10,HVCOUNTER
	bne mainloop
	
	;;; display song num
	bsr init_vdp_transfer
	SetVDPAddrA BGABASE+(24*SCREENWIDTH*2)+(19*2), VRAM_WRITE, (a0)
	move.b song_num,d0
	moveq #0,d1
	bsr output_byte
	
	
	;;; read pad
	move.w #$0100,Z80BUSREQ
	
	lea PORT1DATA,a0
	
	move.b  #$40, (a0)  ; Do 1st step
    nop                 ; D-pad, B and C
    nop
    nop
    nop
    move.b  (a0), d0
    
    move.b  #$00, (a0)  ; Do 2nd step
    nop                 ; A and Start
    nop
    nop
    nop
    move.b  (a0), d1
	
	clr.w Z80BUSREQ
	
	and.b   #$3F, d0    ; Rearrange bits
    and.b   #$30, d1    ; into SACBRLDU
    lsl.b   #2, d1
    or.b    d1, d0
	
	not.b d0
	move.b rawjoy,d1
	move.b d0,rawjoy
	not.b d1
	and.b d1,d0
	move.b d0,joy
	
	
	;; handle song num changes
	
	move.b song_num,d1
	
	move.b d0,d2
	andi.b #9,d2
	beq .noup
	;cmpi.b #23,d1
	;bhs .noup
	addq.b #1,d1
.noup

	move.b d0,d2
	andi.b #6,d2
	beq .nodown
	;tst.b d1
	;beq .nodown
	subq.b #1,d1
.nodown
	
	move.b d1,song_num
	
	
	;; handle init/stop commands
	move.l d0,d7
	
	btst #6,d7 ;A stops the music
	beq .nostop
	jsr knl_reset
.nostop
	
	btst #4,d7 ;B inits music (slot 0)
	beq .noinit0
	moveq #0,d0
	move.b song_num,d0
	move.l d0,-(sp)
	pea 0
	jsr knl_init
	addq.l #8,sp
.noinit0
	
	btst #5,d7 ;C inits music (slot 1)
	beq .noinit
	moveq #0,d0
	move.b song_num,d0
	move.l d0,-(sp)
	pea 1
	jsr knl_init
	addq.l #8,sp
.noinit
	
	
	
	
	
	
	
	
	bra mainloop
	
	
	
	
rom_end