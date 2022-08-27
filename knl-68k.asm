
Z80 = $a00000
Z80BUSREQ = $a11100
Z80RESET = $a11200


z80_comm_index = Z80+$1ef0
z80_base_pointers = Z80+$1ef1

z80_comm_buf = Z80+$1f00



	section .text,code
	
	db "KokonoePlayer-Lite v0.90 coded by karmic"
	align 1
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Reset routine
knl_reset::
	movem.l d2-d7/a2-a6,-(sp)
	
	;;;;; copy code over to z80
	lea Z80,a0
	lea Z80BUSREQ,a1
	lea $100(a1),a2
	
	moveq #0,d0
	move.w #$0100,d1
	
	move.w d0,(a2) ;reset on
	move.w d1,(a1) ;busreq on
	move.w d1,(a2) ;reset off
	
.waitready:
	btst.b #0,(a1)
	bne .waitready
	
	lea z80_blob,a3
	move.w #z80_blob_end-z80_blob-1,d7
.z80copy:
	move.b (a3)+,(a0)+
	dbra d7,.z80copy
	move.w #$2000-(z80_blob_end-z80_blob)-1,d7
.z80clear:
	move.b d0,(a0)+
	dbra d7,.z80clear
	
	
	;tell the z80 where stuff is
	lea z80_base_pointers,a0
	lea z80_pointer_table,a1
	lea z80_pointer_table_end,a2
	
.ptrloop
	move.l (a1)+,d0
	
	move.b d0,(a0)+
	lsr.l #8,d0
	move.l d0,d1
	bset #7,d1
	move.b d1,(a0)+
	lsr.l #7,d0
	move.b d0,(a0)+
	
	cmpa.l a1,a2
	bne .ptrloop
	
	
	
	moveq #0,d0
	move.w d0,(a2) ;reset on
	move.w d0,(a1) ;busreq off

	moveq #$7f,d7
.waitreset:
	dbra d7,.waitreset
	move.w d1,(a2) ;reset off
	
	
	movem.l (sp)+,d2-d7/a2-a6
	rts
	
	
z80_pointer_table:
	dl knl_song_tbl,knl_fm_patch_tbl,knl_sample_tbl
z80_pointer_table_end:
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; 68k -> z80 functions
	
z80_enter:
	lea Z80BUSREQ,a0
	move.w #$0100,d0
	move.w d0,(a0)
.waitloop
	cmp.w (a0),d0
	beq .waitloop
	
	lea z80_comm_buf,a1
	moveq #0,d1
	move.b z80_comm_index,d1
	rts
	
	
	
z80_exit:
	move.b d1,z80_comm_index
	
	clr.w Z80BUSREQ
	rts
	
	
	macro z80_comm
		move.b \1,(a1,d1)
		addq.b #1,d1
	endm
	
	
	
knl_init::
	cargs #4, .arg_song_slot.l, .arg_song_id.l
	bsr z80_enter
	
	z80_comm #0 * 2
	
	z80_comm .arg_song_slot+3(sp)
	
	move.l .arg_song_id(sp),d0
	z80_comm d0
	lsr.w #8,d0
	z80_comm d0
	
	
	bra z80_exit
	
	
	
	
	
knl_stop::
	cargs #4, .arg_song_slot.l
	bsr z80_enter
	
	z80_comm #1 * 2
	
	z80_comm .arg_song_slot+3(sp)
	
	bra z80_exit
	
	
	
	
	
	
	
	section .rodata,data
	
z80_blob:
	incbin "knl-z80.bin"
z80_blob_end:
	
	align 1
	include "KNL-MODULE.asm"
	
