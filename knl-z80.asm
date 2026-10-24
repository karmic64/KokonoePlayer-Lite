
.memorymap
defaultslot 0
slotsize $2000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $2000
banks 1
.endro


.define FM1REG $4000
.define FM1DATA $4001
.define FM2REG $4002
.define FM2DATA $4003

.define BANK $6000

.define PSG $7f11




;value is calculated so the timer overflows at 44100/BASE_INTERVAL hz
.define TIMER_A_VALUE ($400 - ((1.0 / 0.00001877) / (44100.0 / KNL_BASE_INTERVAL)))


.define NTSC_INTERVAL 735
.define PAL_INTERVAL 882



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ram layout

.define STACK_POINTER $1ef8

.define CALL_STACK_DIST 32

.define FM_PATCH_DIST 32


;;;;;; channel struct

.define CHN_DIST $10
.define CHN_SHIFT 4

.struct chn
	timer	db
	ptr	dw
	bank	db
	stack_ptr	dw
	stack_index	db
	.union
		keyon	db
	.nextu
		volume	db
	.endu
	pitch	dw
	patch_ptr	dw	;only used on fm channels
.endst

.assert _sizeof_chn <= CHN_DIST, LDERROR, "chn struct is too long"


;;;;;; song slot struct

.define SS_DIST $10
.define SS_SHIFT 4

.define SS_CHANNELS 10
.define SS_FM_CHANNELS 6
.define SS_PSG_CHANNELS 4
.define SS_CHANNELS_DIST SS_CHANNELS*CHN_DIST

.define FIRST_FM_CHANNEL 0
.define FIRST_PSG_CHANNEL 6
.define FM_CHN3	2
.define FM_DAC_CHANNEL 5
.define PSG_CHANNEL_2 8
.define PSG_NOISE_CHANNEL 9

.define SS_FLAG_ON 7
.define SS_FLAG_DAC 6
.define SS_FLAG_EXT_CHN3 5

.struct ss
	flags	db
	channels_ptr	dw
	lfo	db
	chn3_freqs	dsw 3
	sample_freq	dw
.endst

.assert _sizeof_ss <= SS_DIST, LDERROR, "ss struct is too long"


;;;;;;; 68k communication variables

.enum STACK_POINTER
	m68k_command_buf_end_index	db

	m68k_song_tbl_base	dl
	m68k_sample_tbl_base	dl
.ende

.define m68k_command_buf $1f00


;;;;;;;

	; note: the entire global variables/song slots must be contained in one page
	; the individual other structs should not cross pages, but the total area can

.function align(addr, align) floor(((addr)+(align-1))/align)*align

.define RAM_BASE $0820

.enum RAM_BASE
	m68k_command_buf_index	db

	cur_song_slot_id	db
	cur_channel_id	db
	cur_channel_in_control	db	;0=in control, >0=cannot take over, <0=can take over

	fm_channel_offset	db

	channel_ss	dsb SS_CHANNELS	;song slot in control of each channel
	lfo_ss	db	;song slot in control of lfo

	psg_noise_mode	db

	sample_init_flag	db
	sample_init_id	db
	sample_flags	db
	sample_start	dsb 3
	sample_accum	db
	sample_ptr	dsb 3
.ende

.define song_slots_base	align(RAM_BASE+$20, SS_DIST)
.define channels_base	align(song_slots_base+(KNL_SONG_SLOTS*SS_DIST), CHN_DIST)
.define call_stacks_base	align(channels_base+(KNL_SONG_SLOTS*SS_CHANNELS_DIST), CALL_STACK_DIST)
.define fm_patches_base	align(call_stacks_base+(KNL_SONG_SLOTS*SS_CHANNELS*CALL_STACK_DIST), FM_PATCH_DIST)



.define cur_bank set_bank+1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; macros



	.macro step_hl
		inc l
		call z,step_banked_hl
	.endm

	.macro step_de
		inc e
		.db $28,$ff	;conditional rst 38 when z
	.endm



	.macro wait_fm_ready_hl isolated
-:
	bit 7,(hl)
	jr nz,-
	.endm






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rst-area

	.orga 0
reset:
	di
	ld sp,STACK_POINTER

	xor a
	call set_bank@always

	call reset_sound

	ld hl,FM1REG
	ld d,h
	ld e,<FM1DATA

	ld (hl),$24
	ld a,TIMER_A_VALUE>>2
	ld (de),a
	wait_fm_ready_hl
	ld (hl),$25
	ld a,TIMER_A_VALUE&3
	ld (de),a

main_loop:
	call wait_timer
	call handle_m68k_command_buf
	call play_sound
	jr main_loop



	; step banked pointer in hl after l-overflow
step_banked_hl:
	inc h
	ret nz
	ld h,$80
	jp next_bank




	.orga $38
	; step banked pointer in de after e-overflow
step_banked_de:
	inc d
	ret nz
	ld d,$80
next_bank:
	push af
	ld a,(cur_bank)
	inc a
	call set_bank@always
	pop af
	ret








	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc subroutines

	; set m68k bank id to A
set_bank:
	cp 0
	ret z
@always:
	ld (cur_bank),a
	push hl
	ld hl,BANK
	.rept 7
		ld (hl),a
		rrca
	.endr
	ld (hl),a
	ld (hl),0
	pop hl
	ret






set_song_slot:
	ld (cur_song_slot_id),a
	.rept SS_SHIFT
		rlca
	.endr
	add a,<song_slots_base
	ld ixl,a
	ld ixh,>song_slots_base
	ret





get_channel_fm_regs
	exx
	ld hl,FM1REG
	ld b,h
	ld c,l
	ld d,h
	ld e,<FM1DATA
	cp 3
	jr c,+
	sub 3
	ld c,<FM2REG
	ld e,<FM2DATA
+:
	ld (fm_channel_offset),a
	exx
	ret






mute_fm_channel
	ld a,(fm_channel_offset)
	exx
	ex af,af'
	ld a,$ff
	ex af,af'

	;; set tl
	add a,$40
	.rept 3
		wait_fm_ready_hl
		ld (bc),a
		ex af,af'
		ld (de),a
		ex af,af'
		add a,4
	.endr
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a

	;; set rr
	ex af,af'
	add a,$80-$4c
	.rept 3
		wait_fm_ready_hl
		ld (bc),a
		ex af,af'
		ld (de),a
		ex af,af'
		add a,4
	.endr
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a

	;; key off
	ld a,(cur_channel_id)
	cp 3
	jr c,+
	inc a
+:
	wait_fm_ready_hl
	ld (hl),$28
	ld (FM1DATA),a

	;; if dac channel then stop sample
	ld a,(cur_channel_id)
	sub FM_DAC_CHANNEL
	jr nz,+
	ld (sample_flags),a
	wait_fm_ready_hl
	ld (hl),$2b
	ld (de),a
+:
	exx
	ret






set_fm_channel_keyon
	ld a,(iy+chn.keyon)
	rlca
	rlca
	rlca
	rlca
	ld b,a

	ld a,(cur_channel_id)
	cp 3
	jr c,+
	inc a
+:
	or a,b
	exx
	wait_fm_ready_hl
	ld (hl),$28
	ld (FM1DATA),a
	exx
	ret

set_psg_channel_volume
	ld a,(cur_channel_id)
	sub FIRST_PSG_CHANNEL
@no_sub:
	rrca
	rrca
	rrca
	or a,$90
	ld b,a

	ld a,(iy+chn.volume)
	cpl
	and a,$0f
	or a,b
	ld (PSG),a
	ret





set_fm_channel_pitch
	exx
	ld a,(iy+chn.pitch+1)
	ex af,af'
	ld a,(fm_channel_offset)
	add a,$a4
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	ld a,(iy+chn.pitch+0)
	ex af,af'
	sub 4
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	exx
	ret

set_psg_channel_pitch
	ld a,(cur_channel_id)
	cp PSG_NOISE_CHANNEL
	jr z,set_psg_noise_pitch
	sub FIRST_PSG_CHANNEL
@no_sub:
	rrca
	rrca
	rrca
	or a,$80
	ld b,a
	ld a,(iy+chn.pitch+0)
	ld c,a
	and a,$0f
	or b
	ld (PSG),a

	ld a,(iy+chn.pitch+1)
	.rept 4
		sla c
		rla
	.endr
	ld (PSG),a
	ret

set_psg_noise_pitch
	ld a,(psg_noise_mode)
	ld b,a
	ld a,(iy+chn.pitch)
	or a,$e0
	cp b
	ret z
	ld (psg_noise_mode),a
	ld (PSG),a
	ret










take_over_fm_channel
	call mute_fm_channel

	ld a,(cur_channel_id)	;ok, begin takeover
	add a,<channel_ss
	ld l,a
	ld h,>channel_ss
	ld a,(cur_song_slot_id)
	ld (hl),a

	xor a
	ld (cur_channel_in_control),a

	ld l,(iy+chn.patch_ptr+0)	;refresh patch
	ld h,(iy+chn.patch_ptr+1)
	ld a,(hl)
	ex af,af'
	ld a,(fm_channel_offset)
	add a,$30
@@@patch_loop:
	exx
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	exx
	inc l
	ld a,(hl)
	ex af,af'
	add a,$04
	cp a,$a0
	jr c,@@@patch_loop
	add a,$10
	exx
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	exx
	inc l
	ld a,(hl)
	ex af,af'
	add a,$04
	exx
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	exx

	ld a,(cur_channel_id)
	cp FM_DAC_CHANNEL
	call z,set_fm_dac_mode

	call set_fm_channel_pitch	;refresh other
	jp set_fm_channel_keyon




set_fm_dac_mode:
	ld a,(ix+ss.flags)
	and 1<<SS_FLAG_DAC
	.rept 7-SS_FLAG_DAC
		rlca
	.endr
	ld hl,FM1REG
	wait_fm_ready_hl
	ld (hl),$2b
	ld (FM1DATA),a
	ret











	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; timer wait routine/sample player





wait_timer:
	ld hl,FM1REG

	; acknowledge timer (and set chn3 state)
	ld b,$15
	ld a,(channel_ss+FM_CHN3)
	or a
	jp m,@set_timer_ack
	.rept SS_SHIFT
		rlca
	.endr
	add a,<song_slots_base+ss.flags
	ld e,a
	ld d,>song_slots_base
	ld a,(de)
	and a,1<<SS_FLAG_EXT_CHN3
	jr z,@set_timer_ack
	ld b,$55
@set_timer_ack:
	ld a,b
	wait_fm_ready_hl
	ld (hl),$27
	ld (FM1DATA),a

	ld d,h
	ld e,<FM1DATA
	exx

	; check for sample init
	ld hl,sample_init_flag
	ld a,(hl)
	or a
	jr z,@no_sample_init
	ld (hl),0	;reset init flag

	inc l	;get sample id
	ld e,(hl)
	ld d,0

	inc l	;set play flags
	ld (hl),a

	ex de,hl	;get sample table pointer
	add hl,hl
	add hl,hl
	inc hl
	ld de,(m68k_sample_tbl_base+0)
	ld a,(m68k_sample_tbl_base+2)
	add hl,de
	jr nc,+
	set 7,h
	inc a
+:
	call set_bank

	ld a,(hl)	;get sample base
	step_hl
	ld d,(hl)
	step_hl
	ld e,(hl)
	sla d
	rla
	scf
	rr d
	ld hl,sample_start
	ld (hl),e
	inc l
	ld (hl),d
	inc l
	ld (hl),a
	inc l
	ld (hl),0
	inc l
	ld (hl),e
	inc l
	ld (hl),d
	inc l
	ld (hl),a

@no_sample_init:
	ld a,(sample_flags)
	or a
	jr z,@not_playing_sample

	; sample playing, begin sample loop
@playing_sample:
	exx
	wait_fm_ready_hl	;set dac register
	ld (hl),$2a
	exx

	ld a,(channel_ss+FM_DAC_CHANNEL)	;get freq
	rlca
	rlca
	rlca
	rlca
	add a,(<song_slots_base)+ss.sample_freq
	ld l,a
	ld h,>song_slots_base
	ld c,(hl)
	inc l
	ld e,(hl)
	ld d,0

	ld a,(sample_ptr+2)
	call set_bank
	ld hl,sample_accum	;get pointer
	ld b,(hl)
	inc l
	ld a,(hl)
	inc l
	ld h,(hl)
	ld l,a

@sample_loop:	;begin main loop
	ld a,(hl)	;7
	or a	;4
	jr z,@sample_over	;7
	exx	;4
	bit 0,(hl)	;12
	jr nz,@sample_done	;7
	ld (de),a	;7
	exx	;4
	ld a,b	;4
	add a,c	;4
	ld b,a	;4
	adc hl,de	;15
	jr nc,@sample_loop	;12
@sample_step_bank:
	set 7,h
	ld a,(cur_bank)
	inc a
	call set_bank@always
	jr @sample_loop

@sample_over:
	ld hl,sample_flags
	bit 6,(hl)
	jr z,@stop_sample
	ld b,0
	ld hl,(sample_start+0)
	ld a,(sample_start+2)
	call set_bank
	jr @sample_loop

@sample_done:
	exx
	ld a,b
	ld (sample_accum),a
	ld (sample_ptr+0),hl
	ld a,(cur_bank)
	ld (sample_ptr+2),a
	ret

@stop_sample:
	ld (hl),0

	; no sample, just wait for timer
@not_playing_sample:
	exx
-:
	bit 0,(hl)
	jr z,-

	ret






	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; m68k command handler

handle_m68k_command_buf:
	ld hl,m68k_command_buf_index
	ld b,>m68k_command_buf
	ld c,(hl)
	jp @entry

	;; main command loop
@loop:
	ld a,(bc)
	inc c
	add a,<@cmd_tbl
	ld l,a
	adc a,>@cmd_tbl
	sub l
	ld h,a
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	jp hl

@cmd_tbl:
	.dw @cmd_init,@cmd_pause,@cmd_resume


	;;;; $00 init song - byte song slot id, word song id

@cmd_init:
	ld a,(bc)	;get song slot pointer
	inc c
	call set_song_slot

	;; init song slot variables
	ld (ix+ss.flags),1<<SS_FLAG_ON
	ld (ix+ss.lfo),0

	;; get song header pointer
	ld a,(bc)	;song id
	inc c
	ld l,a
	ld a,(bc)
	inc c
	ld h,a
	push bc

	add hl,hl
	add hl,hl
	inc hl
	ld de,(m68k_song_tbl_base+0)
	ld a,(m68k_song_tbl_base+2)
	add hl,de
	jr nc,+
	set 7,h
	inc a
+:
	call set_bank

	ld a,(hl)	;read pointer into de
	step_hl
	ld d,(hl)
	step_hl
	ld e,(hl)
	sla d
	rla
	scf
	rr d
	ex de,hl
	call set_bank

	;; begin main channel loop
	ld de,channel_ss
	ld a,(ix+ss.channels_ptr+0)
	ld iyl,a
	ld a,(ix+ss.channels_ptr+1)
	ld iyh,a
	xor a
@@chn_loop:
	ld (cur_channel_id),a

	ex af,af'
	ld a,(cur_channel_id)
	cp FIRST_PSG_CHANNEL
	call c,get_channel_fm_regs
	ex af,af'

	cp (hl)	;if the song is using this channel then fully init it, else disable it
	jr z,@@start_chn
@@disable_chn:
	xor a
	ld (iy+chn.ptr+1),a

	ld a,(cur_channel_id)
	cp FIRST_PSG_CHANNEL
	jr nc,@@next_chn

	ex de,hl	;if we have this channel then give it up
	ld a,(cur_song_slot_id)
	cp (hl)
	jr nz,@@no_give_up
	ld (hl),$ff
	call mute_fm_channel
@@no_give_up:
	ex de,hl

	jr @@next_chn

@@start_chn:
	step_hl
	xor a
	ld (iy+chn.timer),a
	ld (iy+chn.stack_index),a
	ld (iy+chn.volume),a
	ld a,(hl)
	step_hl
	ld b,(hl)
	step_hl
	ld c,(hl)
	sla b
	rla
	scf
	rr b
	ld (iy+chn.ptr+0),c
	ld (iy+chn.ptr+1),b
	ld (iy+chn.bank),a
	step_hl

	ld a,(cur_channel_id)
	cp FIRST_PSG_CHANNEL
	jr nc,@@next_chn

	ld a,(de)	;if this channel is not in use then take it over
	or a
	ld b,a
	ld a,(cur_song_slot_id)
	jp m,@@take_over_chn
	cp b	;otherwise, take over the channel if we either already have it, or the current song slot is lower
	jr c,@@no_take_over
@@take_over_chn:
	ld (de),a
	call mute_fm_channel
@@no_take_over:

@@next_chn:
	inc e
	ld bc,CHN_DIST
	add iy,bc
	ld a,(cur_channel_id)
	inc a
	cp SS_CHANNELS
	jr c,@@chn_loop

	;; done
	pop bc
	jr @next_cmd


	;;;; $02 pause song - byte song slot id

@cmd_pause:
	ld a,(bc)	;get song slot pointer
	inc c
	call set_song_slot

	res SS_FLAG_ON,(ix+ss.flags)	;disable song slot

	;; kill fm channels
	ld hl,channel_ss
	ld a,(ix+ss.channels_ptr+0)
	ld iyl,a
	ld a,(ix+ss.channels_ptr+1)
	ld iyh,a
	xor a
@@chn_loop:
	ld (cur_channel_id),a

	call get_channel_fm_regs

	ld a,(cur_song_slot_id)	;if we are in control of this channel then give it up
	cp (hl)
	jr nz,@@next_chn
	ld (hl),$ff
	call mute_fm_channel

@@next_chn:
	inc l
	ld bc,CHN_DIST
	add iy,bc
	ld a,(cur_channel_id)
	inc a
	cp FIRST_PSG_CHANNEL
	jr c,@@chn_loop

	jr @next_cmd


	;;;; $04 resume song - byte song slot id

@cmd_resume:
	ld a,(bc)	;get song slot pointer
	inc c
	call set_song_slot

	set SS_FLAG_ON,(ix+ss.flags)	;enable song slot

	jr @next_cmd


	;; done!
@entry:
@next_cmd:
	ld a,(m68k_command_buf_end_index)
	cp c
	jp nz,@loop
	ld (m68k_command_buf_index),a
	ret







	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; player reset routine

reset_sound:

	;;;;;;;;;;; init ram that wasn't zeroed by the 68k

	;; reset song slot assignments

	ld hl,channel_ss
	ld bc,(((lfo_ss-channel_ss)+1)<<8)|$ff
-:
	ld (hl),c
	inc l
	djnz -

	;; reset song slots

	ld ix,song_slots_base
	ld de,SS_DIST
	exx
	ld hl,channels_base
	ld de,SS_CHANNELS_DIST
	ld b,KNL_SONG_SLOTS
-:
	ld (ix+ss.channels_ptr+0),l
	ld (ix+ss.channels_ptr+1),h
	add hl,de
	exx
	add ix,de
	exx
	djnz -

	;; reset channels

	ld ix,channels_base
	ld hl,call_stacks_base
	ld de,CALL_STACK_DIST
	ld b,KNL_SONG_SLOTS
	exx
	ld hl,fm_patches_base
	ld de,FM_PATCH_DIST
	ld bc,CHN_DIST
	exx
--:
	ld c,0
-:
	ld (ix+chn.stack_ptr+0),l
	ld (ix+chn.stack_ptr+1),h
	add hl,de
	ld a,c
	exx
	cp FIRST_PSG_CHANNEL
	jr nc,+
	ld (ix+chn.patch_ptr+0),l
	ld (ix+chn.patch_ptr+1),h
	add hl,de
+:
	add ix,bc
	exx
	inc c
	ld a,c
	cp SS_CHANNELS
	jr c,-
	djnz --

	ret







	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main play routine

play_sound:

	ld hl,channel_ss+FIRST_PSG_CHANNEL
	ld a,$ff
	.rept 4
		ld (hl),a
		inc l
	.endr
	ld (hl),a

	;;;;;;;;;;;;;;;;;; begin main song slot loop

	ld a,KNL_SONG_SLOTS-1
	ld ix,song_slots_base+((KNL_SONG_SLOTS-1)*SS_DIST)
@song_slot_loop:
	bit SS_FLAG_ON,(ix+ss.flags)
	jp z,@next_song_slot_2

	ld (cur_song_slot_id),a

	;;;;;;;;; begin channel loop

	ld a,(ix+ss.channels_ptr+0)
	ld iyl,a
	ld a,(ix+ss.channels_ptr+1)
	ld iyh,a
	xor a
@chn_loop:
	ld (cur_channel_id),a
	ld e,a

	ld a,(iy+chn.ptr+1)
	or a
	jp z,@next_chn

	dec (iy+chn.timer)
	jp p,@no_song_data_read

	ld a,e
	cp FIRST_PSG_CHANNEL
	jr nc,@@no_fm_control

	call get_channel_fm_regs

	ld a,e
	add a,<channel_ss
	ld l,a
	ld h,>channel_ss
	ld a,(hl)
	or a
	jp m,+
	ld l,<cur_song_slot_id
	sub a,(hl)
+:
@@no_fm_control:
	ld (cur_channel_in_control),a

	;;;;;;;; start reading music data

	ld e,(iy+chn.ptr+0)
	ld d,(iy+chn.ptr+1)
	ld a,(iy+chn.bank)
	call set_bank

@song_data_read_loop:
	ld a,(de)
	step_de
	or a
	jp p,@song_data_read_done
	cp $a0
	jp c,@@fm_patch_param
	cp $f0
	jp nc,@@misc_cmd
	cp $e0
	jp nc,@@volume

@@pitch_dist:
	ld b,0
	sub $c0
	jr nc,+
	dec b
+:
	add a,(iy+chn.pitch+0)
	ld (iy+chn.pitch+0),a
	ld a,b
	adc a,(iy+chn.pitch+1)
	ld (iy+chn.pitch+1),a
	ld a,(cur_channel_in_control)
	or a
	call z,set_fm_channel_pitch
	jp @song_data_read_loop

@@volume:
	ld b,(iy+chn.volume)
	and a,$0f
	ld (iy+chn.volume),a
	ld c,a

	ld a,(cur_channel_id)	;for fm channels, check for takeover on keyoff->keyon
	cp FIRST_PSG_CHANNEL
	jp nc,@song_data_read_loop

	ld a,(cur_channel_in_control)
	or a
	jr nz,@@check_keyon
@@set_volume:
	call set_fm_channel_keyon
	jp @song_data_read_loop
@@check_keyon:
	ld a,b
	cpl
	and c
	and $0f
	jp z,@song_data_read_loop
	ld a,(cur_channel_in_control)
	or a
	jp p,@song_data_read_loop
	call take_over_fm_channel
	jp @song_data_read_loop

@@fm_patch_param:
	and a,$1f
	add a,(iy+chn.patch_ptr+0)
	ld l,a
	ld h,(iy+chn.patch_ptr+1)
	ld a,(de)
	ld (hl),a
	ex af,af'
	step_de
	ld a,(cur_channel_in_control)
	or a
	jp nz,@song_data_read_loop
	ld a,l
	and $1f
	rlca
	rlca
	add a,$30
	cp a,$a0
	jr c,+
	add a,$10
+:
	ld hl,fm_channel_offset
	add a,(hl)
	exx
	wait_fm_ready_hl
	ld (bc),a
	ex af,af'
	ld (de),a
	exx
	jp @song_data_read_loop

@@misc_cmd:
	and a,$0f
	add a,a
	add a,<@@misc_cmd_tbl
	ld l,a
	adc a,>@@misc_cmd_tbl
	sub l
	ld h,a
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	jp hl

@@misc_cmd_tbl:
	.dw @@pitch,@@low_pitch,@@high_pitch
	.dw @@chn_flag_off,@@chn_flag_on
	.dw @@lfo
	.dw @@chn3_pitch_1,@@chn3_pitch_2,@@chn3_pitch_3
	.dw @@sample_freq,@@start_sample,@@start_sample_looped,@@stop_sample
	.dw @@jump,@@call,@@return

@@pitch:
	ld a,(de)
	ld (iy+chn.pitch+0),a
	step_de
@@high_pitch:
	ld a,(de)
	ld (iy+chn.pitch+1),a
	step_de
	ld a,(cur_channel_in_control)
	or a
	call z,set_fm_channel_pitch
	jp @song_data_read_loop
@@low_pitch:
	ld a,(de)
	ld (iy+chn.pitch+0),a
	step_de
	ld a,(cur_channel_in_control)
	or a
	call z,set_fm_channel_pitch
	jp @song_data_read_loop

@@chn_flag_off:
	ld a,(cur_channel_id)
	cp FM_DAC_CHANNEL
	jr z,+
	res SS_FLAG_EXT_CHN3,(ix+ss.flags)
	jp @song_data_read_loop
+:
	res SS_FLAG_DAC,(ix+ss.flags)
	jr @@set_fm_dac_mode
@@chn_flag_on:
	ld a,(cur_channel_id)
	cp FM_DAC_CHANNEL
	jr z,+
	set SS_FLAG_EXT_CHN3,(ix+ss.flags)
	jp @song_data_read_loop
+:
	set SS_FLAG_DAC,(ix+ss.flags)
@@set_fm_dac_mode:
	ld a,(cur_channel_in_control)
	or a
	call z,set_fm_dac_mode
	jp @song_data_read_loop

@@lfo:
	ld a,(de)
	ld (ix+ss.lfo),a
	step_de
	jp @song_data_read_loop

@@chn3_pitch_1:
	ld a,(de)
	ld (ix+ss.chn3_freqs+0),a
	step_de
	ld a,(de)
	ld (ix+ss.chn3_freqs+1),a
	step_de
	ld a,(cur_channel_in_control)
	or a
	jp nz,@song_data_read_loop
	exx
	ld a,$ac
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+1)
	ld (FM1DATA),a
	ld a,$a8
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+0)
	ld (FM1DATA),a
	exx
	jp @song_data_read_loop
@@chn3_pitch_2:
	ld a,(de)
	ld (ix+ss.chn3_freqs+2),a
	step_de
	ld a,(de)
	ld (ix+ss.chn3_freqs+3),a
	step_de
	ld a,(cur_channel_in_control)
	or a
	jp nz,@song_data_read_loop
	exx
	ld a,$ad
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+3)
	ld (FM1DATA),a
	ld a,$a9
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+2)
	ld (FM1DATA),a
	exx
	jp @song_data_read_loop
@@chn3_pitch_3:
	ld a,(de)
	ld (ix+ss.chn3_freqs+4),a
	step_de
	ld a,(de)
	ld (ix+ss.chn3_freqs+5),a
	step_de
	ld a,(cur_channel_in_control)
	or a
	jp nz,@song_data_read_loop
	exx
	ld a,$ae
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+5)
	ld (FM1DATA),a
	ld a,$aa
	wait_fm_ready_hl
	ld (hl),a
	ld a,(ix+ss.chn3_freqs+4)
	ld (FM1DATA),a
	exx
	jp @song_data_read_loop

@@sample_freq:
	ld a,(de)
	ld (ix+ss.sample_freq+0),a
	step_de
	ld a,(de)
	ld (ix+ss.sample_freq+1),a
	step_de
	jp @song_data_read_loop
@@start_sample:
	ld a,(cur_channel_in_control)
	or a
	jr z,+
	jp p,++
	call take_over_fm_channel
+:
	ld a,$80
	ld (sample_init_flag),a
	ld a,(de)
	ld (sample_init_id),a
++:
	step_de
	jp @song_data_read_loop
@@start_sample_looped:
	ld a,(cur_channel_in_control)
	or a
	jr z,+
	jp p,++
	call take_over_fm_channel
+:
	ld a,$c0
	ld (sample_init_flag),a
	ld a,(de)
	ld (sample_init_id),a
++:
	step_de
	jp @song_data_read_loop
@@stop_sample:
	ld a,(cur_channel_in_control)
	or a
	jp nz,@song_data_read_loop
	;xor a
	ld (sample_flags),a
	jp @song_data_read_loop


@@jump:
	ex de,hl
	ld a,(hl)
	step_hl
	ld d,(hl)
	step_hl
	ld e,(hl)
	sla d
	rla
	scf
	rr d
	call set_bank
	jp @song_data_read_loop

@@call:
	ld a,(iy+chn.stack_index)
	ld b,a
	add 3
	ld (iy+chn.stack_index),a
	ld a,b
	add a,(iy+chn.stack_ptr+0)
	ld c,a
	ld b,(iy+chn.stack_ptr+1)

	ex de,hl
	ld a,(hl)
	step_hl
	ld d,(hl)
	step_hl
	ld e,(hl)
	sla d
	rla
	scf
	rr d
	ex af,af'

	inc hl
	ld a,l
	ld (bc),a
	inc c
	ld a,h
	cp $80
	set 7,a
	ld (bc),a
	inc c
	ld a,(cur_bank)
	adc a,0
	ld (bc),a

	ex af,af'
	call set_bank
	jp @song_data_read_loop

@@return:
	ld a,(iy+chn.stack_index)
	or a
	jr z,@@stop
	sub 3
	ld (iy+chn.stack_index),a
	add a,(iy+chn.stack_ptr+0)
	ld l,a
	ld h,(iy+chn.stack_ptr+1)

	ld e,(hl)
	inc l
	ld d,(hl)
	inc l
	ld a,(hl)
	call set_bank
	jp @song_data_read_loop

@@stop:
	ld (iy+chn.ptr+1),0
	ld a,(cur_channel_in_control)	;if we have the channel then give it up
	or a
	jp nz,@next_chn
	ld a,(cur_channel_id)
	add a,<channel_ss
	ld l,a
	ld h,>channel_ss
	ld (hl),$ff
	call mute_fm_channel
	jp @next_chn


	;;;; done
@song_data_read_done:
	ld (iy+chn.timer),a
	ld (iy+chn.ptr+0),e
	ld (iy+chn.ptr+1),d
	ld a,(cur_bank)
	ld (iy+chn.bank),a

	;;;;;;;;; next channel

@no_song_data_read:
	;; if this is a psg channel then update it

	ld a,(cur_channel_id)
	cp FIRST_PSG_CHANNEL
	jr c,@no_psg
	ld b,a
	add a,<channel_ss
	ld l,a
	ld h,>channel_ss
	ld a,(hl)
	or a
	jp p,@no_psg

	ld a,(cur_song_slot_id)
	ld c,a

	ld a,PSG_NOISE_CHANNEL	;special case: if this channel is using tone2 noise mode and that is a different song slot then quit
	cp b
	jr nz,@set_psg
	ld a,(channel_ss+PSG_CHANNEL_2)
	cp c
	jr z,@set_psg
	ld a,(iy+chn.pitch)
	cpl
	and a,3
	jr z,@no_psg
@set_psg:
	ld (hl),c
	call set_psg_channel_volume
	call set_psg_channel_pitch
@no_psg:

@next_chn:
	ld a,(cur_channel_id)
@next_chn_2:
	ld de,CHN_DIST
	add iy,de
	inc a
	cp SS_CHANNELS
	jp c,@chn_loop

	;;;;;;;;; next song slot

	;; check lfo

	ld a,(lfo_ss)
	or a
	jp p,@no_lfo
	ld a,(ix+ss.lfo)
	bit 3,a
	jr z,@no_lfo
	ld hl,FM1REG
	wait_fm_ready_hl
	ld (hl),$22
	ld (FM1DATA),a
	ld a,(cur_song_slot_id)
	ld (lfo_ss),a
@no_lfo:

	;; go to next

@next_song_slot:
	ld a,(cur_song_slot_id)
@next_song_slot_2:
	ld de,-SS_DIST
	add ix,de
	dec a
	jp p,@song_slot_loop

	;;;;; mute unused psg

	ld hl,channel_ss+FIRST_PSG_CHANNEL+3
	ld b,4
@psg_mute_loop:
	ld a,(hl)
	or a
	jp p,@@next
	ld a,b
	dec a
	rrca
	rrca
	rrca
	or a,$9f
	ld (PSG),a
@@next:
	dec l
	djnz @psg_mute_loop

	ret











