
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
.define TIMER_VALUE ($400 - ((1.0 / 0.00001877) / (44100.0 / KNL_BASE_INTERVAL)))


.define NTSC_INTERVAL 735
.define PAL_INTERVAL 882


;;;;;;;;;;;;;;;;;;;; ram structs


;;;; song slot
.define SS_FLAG_ON 7
.define SS_FLAG_CHN3 6
.define SS_FLAG_DAC 5

.enum 0
ss_flags db ;(lower 4 bits are LFO)
ss_ptr dsb 3
ss_timer dw

ss_fm_keyon dsb 6
ss_fm_patch_lo dsb 6
ss_fm_patch_hi dsb 6

ss_psg_volume dsb 4

ss_fm_pitch_lo dsb 6
ss_chn3_pitch_lo dsb 3
ss_psg_pitch_lo dsb 4
ss_fm_pitch_hi dsb 6
ss_chn3_pitch_hi dsb 3
ss_psg_pitch_hi dsb 3 ;only 3! noise doesn't need a high byte

ss_sample_freq dw

ss_size db
.ende






;;;;;;;;;;;;;;;;; private variables
.enum $0a00
k_temp dsb 4
k_comm_index db

k_chn3_state db

k_song_slot db
k_song_slot_ptr dw

k_fm_channel db

;;;;

k_fm_channel_ss dsb 6 ;song slot currently in control of each channel ($ff = none)

k_psg_channel_ss dsb 4

k_lfo_ss db

k_prv_psg_noise db

k_sample_init_flag db
k_sample_init_id db

;;;;

k_sample_active db ;bit 7 = active, bit 6 = loop
k_sample_start dsb 3
k_sample_accum db
k_sample_ptr dsb 3
k_sample_freq dw

;;;;

;note that we keep two copies of each song slot, one actively used and the other saved and restored on loop
k_song_slots dsb ss_size*KNL_SONG_SLOTS
k_song_slots_loop dsb ss_size*KNL_SONG_SLOTS

.ende




;;;;;;;;;;;;;;;; 68k-z80 communication variables

.define STACK_POINTER $1ef0
.enum STACK_POINTER
comm_end_index db

;68k bus pointers
song_tbl_base dsb 3
fm_patch_tbl_base dsb 3
sample_tbl_base dsb 3

.ende

.define comm_buf $1f00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reset handler/rst routines


	;reset stub
	.orga 0
	di
	ld sp,STACK_POINTER
	ld a,$ff
	jr reset
	
	
	
	
	;write byte a to ym register b
	;(assumes hl' is $4000, bc' is the register and de' is the data)
	.orga $8
fm_write:
	ex af,af'
	ld a,b
	exx
	ld (bc),a
	
-:	bit 7,(hl)
	jr nz,-
	
	ex af,af'
	ld (de),a
	exx
	ret
	



	;write byte a to fm reg b, part 1
	;assumes hl' is $4000
	.orga $18
fm_write_1:
	ex af,af'
	ld a,b
	exx
	ld (hl),a
	
-:	bit 7,(hl)
	jr nz,-
	
	ex af,af'
	inc l
	ld (hl),a
	dec l
	exx
	ret
	
	
	
	
	
	;read byte from banked pointer chl into a then step pointer
	.orga $30
get_byte:
	ld a,(hl)
	inc l
	ret nz
	inc h
	ret nz
	ld h,$80
	inc c
	;now we are at $38, another good rst location
	;sets bank to c
set_bank:
	push af
	push hl
	ld hl,BANK
	ld a,c
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	rrca
	ld (hl),a
	ld (hl),0
	pop hl
	pop af
	ret
	
	
	
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reset
	
reset:
	
	ld hl,PSG
	ld (hl),$9f
	ld (hl),$bf
	ld (hl),$df
	;ld a,$ff
	ld (hl),a
	
	
	; ram has already been zeroed by the 68k, so only init nonzero ones
	ld hl,k_fm_channel_ss
	ld b,6
-:
	ld (hl),a
	inc hl
	djnz -
	
	ld (k_prv_psg_noise),a
	
	
	
	
	
	
	
	
	
	
	exx
	ld hl,$4000
	exx
	
	ld a,TIMER_VALUE >> 2
	ld b,$24
	rst fm_write_1
	ld a,TIMER_VALUE & 3
	inc b
	rst fm_write_1
	
	ld a,$15
	ld b,$27
	rst fm_write_1
	
	
	
	
	
	
main_loop:
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; idle/sample loop
	ld hl,$4000
@check_frame_again:
	bit 0,(hl)
	jr nz,@do_frame
	
	ld a,(k_sample_active)
	or a
	jr z,@check_frame_again
	
	;get ready for sample playback
	ld a,$2a
	ld ($4000),a
	
	ld a,(k_sample_accum)
	ld b,a
	ld hl,(k_sample_ptr+0)
	ld a,(k_sample_ptr+2)
	ld c,a
	rst set_bank
	
	ld a,(k_sample_freq+1)
	ld e,a
	ld d,0
	jr @sample_main_loop
	
	
@sample_end:
	ld hl,k_sample_active
	bit 6,(hl)
	jr nz,@loop_sample
	
@stop_sample:
	ld (hl),0
	jr main_loop
	
@loop_sample:
	
	ld b,0
	ld hl,(k_sample_start+0)
	ld a,(k_sample_start+2)
	ld c,a
	rst set_bank
	
@sample_main_loop:
	ld a,(hl)
	or a
	jr z,@sample_end
	ld ($4001),a
	
	ld a,(k_sample_freq)
	add b
	ld b,a
	adc hl,de
	jp nc,+
	set 7,h
	inc c
	rst set_bank
+:
	
	ld a,($4000)
	rrca
	jp nc,@sample_main_loop
	
	;;; save sample state
	
	ld a,b
	ld (k_sample_accum),a
	ld (k_sample_ptr+0),hl
	ld a,c
	ld (k_sample_ptr+2),a
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frame hit, handle comms and play music
	
@do_frame:
	
	
	exx
	ld hl,$4000
	exx
	
	ld a,(k_chn3_state)
	or $15
	ld b,$27
	rst fm_write_1
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; comms
	
	ld a,(k_comm_index)
	ld c,a
	ld a,(comm_end_index)
	cp c
	jr z,@after_comms
	ld b,>comm_buf
	
@comm_loop:
	ld a,(bc)
	inc c
	ld e,a
	ld d,0
	ld hl,@@tbl
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	jp hl
	
@@tbl:
	.dw @@init
	.dw @@stop
	
	;;;;;;;;;;;;;;;;;;; cmd $00 init song: byte - song slot, word - song id
@@init:
	ld a,(bc)
	inc c
	call set_song_slot
	
	ld a,(bc)
	ld e,a
	inc c
	ld a,(bc)
	ld d,a
	inc c
	push bc
	
	ld hl,(song_tbl_base)
	ld a,(song_tbl_base+2)
	call indexed_read_68k_ptr
	
	ld (ix+ss_flags),$80
	ld (ix+ss_ptr+0),l
	ld (ix+ss_ptr+1),h
	ld (ix+ss_ptr+2),c
	
	ld e,ixl
	ld d,ixh
	ld hl,ss_timer
	add hl,de
	ld b,ss_size-ss_timer
	xor a
-:
	ld (hl),a
	inc hl
	djnz -
	
	ld de,k_song_slots_loop-k_song_slots-ss_size
	add hl,de
	ld (hl),a
	
	;this song slot may already have occupied channels, so reset it to a regular state
	call give_up_channels
	
	pop bc
	jr @next_comm
	
	
	;;;;;;;;;;;;;;;;;; cmd $02 stop song slot: byte - song slot
@@stop:
	ld a,(bc)
	inc c
	push bc
	call stop_song_slot
	pop bc
	jr @next_comm

	
@next_comm:
	ld a,(comm_end_index)
	cp c
	jr nz,@comm_loop
	ld (k_comm_index),a
	
@after_comms:










	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; play routine
	
play:
	
	ld hl,k_psg_channel_ss
	ld a,$ff
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	
	ld (k_lfo_ss),a
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main song slot loop
	
	ld a,KNL_SONG_SLOTS-1
	ld ix,k_song_slots+((KNL_SONG_SLOTS-1)*ss_size)
	
@song_slot_loop:
	ld (k_song_slot),a
	ld (k_song_slot_ptr),ix
	
	bit 7,(ix+ss_flags)
	jp z,@@next_song_slot
	
	ld a,(ix+ss_timer+0)
	sub <KNL_BASE_INTERVAL
	ld (ix+ss_timer+0),a
	ld a,(ix+ss_timer+1)
	sbc >KNL_BASE_INTERVAL
	ld (ix+ss_timer+1),a
	jp nc,@@no_vgm_data_read
	
	
	;;;;;;;;;;;;;;;;;;;;;;; read vgm data
	xor a
	ld (k_sample_init_flag),a
	
@@vgm_data_read_again:
	ld l,(ix+ss_ptr+0)
	ld h,(ix+ss_ptr+1)
	ld c,(ix+ss_ptr+2)
	rst set_bank
	
@@vgm_data_read_loop:
	rst get_byte
	cp $60
	jp c,@@@fm_channel
	cp $6c
	jr c,@@@misc_command
	cp $70
	jr c,@@@psg_freq_lo
	cp $80
	jr c,@@@psg_freq_hi
	cp $c0
	jr c,@@@psg_volume
	
	
	;;;;;;;;;;;;;; $c0-$ff
@@@wait_frames:
	push hl
	ld de,frame_timer_tbl
	and $3f
	rlca
	ld l,a
	ld h,0
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop hl
	jp @@after_vgm_data_read
	
	
	
	;;;;;;;;;;;;; $80-$bf
@@@psg_volume:
	ld b,a
	rrca
	rrca
	rrca
	rrca
	and 3
	call iy_equals_ix_plus_a
	ld a,b
	and $0f
	ld (iy+ss_psg_volume),a
	jp @@vgm_data_read_loop
	
	
	;;;;;;;;;;;;; $7x
@@@psg_freq_hi:
	ld b,a
	rrca
	rrca
	and 3
	cp 3
	jr z,@@@@noise
	call iy_equals_ix_plus_a
	ld a,b
	and 3
	ld (iy+ss_psg_pitch_hi),a
	jp @@vgm_data_read_loop
	
@@@@noise:
	ld a,(ix+ss_psg_pitch_lo+3)
	and $fc
	ld e,a
	ld a,b
	and 3
	or e
	ld (ix+ss_psg_pitch_lo+3),a
	jp @@vgm_data_read_loop
	
	
	;;;;;;;;;;;;; $6c-$6f
@@@psg_freq_lo:
	and 3
	call iy_equals_ix_plus_a
	rst get_byte
	ld (iy+ss_psg_pitch_lo),a
	jp @@vgm_data_read_loop
	
	
	;;;;;;;;;;;;; $60-$6b
@@@misc_command:
	push hl
	ld hl,@@@@tbl
	and $f
	rlca
	ld e,a
	ld d,0
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop hl
	push de
	ret
	
@@@@tbl:
	.dw @@@@chn3_normal
	.dw @@@@chn3_extd
	.dw @@@@dac_disable
	.dw @@@@dac_enable
	.dw @@@@lfo
	.dw @@@@sample_freq
	.dw @@@@sample_start
	.dw @@@@sample_loop
	.dw @@@@sample_stop
	.dw @@@@wait
	.dw @@@@set_loop
	.dw @@@@end
	
@@@@chn3_normal:
	res SS_FLAG_CHN3,(ix+ss_flags)
	jp @@vgm_data_read_loop
@@@@chn3_extd:
	set SS_FLAG_CHN3,(ix+ss_flags)
	jp @@vgm_data_read_loop
	
@@@@dac_disable:
	res SS_FLAG_DAC,(ix+ss_flags)
	jp @@vgm_data_read_loop
@@@@dac_enable:
	set SS_FLAG_DAC,(ix+ss_flags)
	jp @@vgm_data_read_loop
	
@@@@lfo:
	rst get_byte
	ld b,a
	ld a,(ix+ss_flags)
	and $f0
	or b
	ld (ix+ss_flags),a
	jp @@vgm_data_read_loop
	
@@@@sample_freq:
	rst get_byte
	ld (ix+ss_sample_freq+0),a
	rst get_byte
	ld (ix+ss_sample_freq+1),a
	jp @@vgm_data_read_loop
	
@@@@sample_start:
	ld b,$80
	jr +
@@@@sample_loop:
	ld b,$c0
+:
	ld (k_sample_init_flag),a
	
	rst get_byte
	ld (k_sample_init_id),a
	
	jp @@vgm_data_read_loop
	
	
@@@@sample_stop:
	jp @@vgm_data_read_loop
	
@@@@wait:
	rst get_byte
	ld e,a
	rst get_byte
	ld d,a
	jp @@after_vgm_data_read
	
@@@@set_loop:
	ld (ix+ss_ptr+0),l
	ld (ix+ss_ptr+1),h
	ld (ix+ss_ptr+2),c
	
	push hl
	ld e,ixl
	ld d,ixh
	ld hl,k_song_slots_loop-k_song_slots
	add hl,de
	
	ld b,ss_size
-:
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz -
	
	pop hl
	
	
	jp @@vgm_data_read_loop
	
@@@@end:
	
	ld e,ixl
	ld d,ixh
	ld hl,k_song_slots_loop-k_song_slots
	add hl,de
	
	bit 7,(hl)
	jr z,@@@@@stop
	
	ld b,ss_size
-:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz -
	
	;update fm with the new state
	ld hl,k_fm_channel_ss
	xor a
@@@@@reset_loop:
	ld (k_fm_channel),a
	ld a,(k_song_slot)
	cp (hl)
	jr nz,@@@@@@next
	
	push hl
	ld a,(k_fm_channel)
	call iy_equals_ix_plus_a
	
	call mute_fm_channel
	call write_fm_patch ;write all sound parameters
	call write_fm_freq
	ld a,(k_fm_channel) ;for channel 3, initialize the aux. freqs too
	cp 2
	jr nz,+
	call write_fm_3freq
	ld hl,k_fm_channel
	dec (hl)
	call write_fm_3freq
	ld hl,k_fm_channel
	dec (hl)
	call write_fm_3freq
	ld a,2
	ld (k_fm_channel),a
+:
	call write_fm_keyon
	
	pop hl
	
@@@@@@next:
	inc hl
	ld a,(k_fm_channel)
	inc a
	cp 6
	jr c,@@@@@reset_loop
	
	
	ld l,(ix+ss_ptr+0)
	ld h,(ix+ss_ptr+1)
	ld c,(ix+ss_ptr+2)
	rst set_bank
	jp @@vgm_data_read_loop
	
	
@@@@@stop:
	call stop_current_song_slot
	jp @@next_song_slot
	
	
	
	
	
	
	
	;;;;;;;;;;;;;; $00-$5f
@@@fm_channel:
	; channel in k_fm_channel, pointer in iy
	ld b,a
	rrca
	rrca
	rrca
	rrca
	and $0f
	ld (k_fm_channel),a
	call iy_equals_ix_plus_a
	
	; get channel song slot info
	push hl
	ld hl,k_fm_channel
	ld e,(hl)
	ld d,0
	ld hl,k_fm_channel_ss
	add hl,de
	ld a,(hl) ;leave a pointer to it in hl
	ld (k_temp),a ;currently active song slot on this channel in k_temp
	ld (k_temp+1),hl ;pointer to it in k_temp+1
	
	; dispatch
	ld a,b
	and $0f
	rlca
	ld de,@@@@tbl
	ld l,a
	ld h,0
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop hl
	push de
	ret
	
@@@@tbl:
	.dw @@@@keyoff_all
	.dw @@@@keyon_all
	.dw @@@@keyon
	.dw @@@@fm_patch_lo
	.dw @@@@fm_patch
	.dw @@@@freq_lo
	.dw @@@@freq_hi
	.dw @@@@freq
	
	.dw @@@@3freq_lo
	.dw @@@@3freq_hi
	.dw @@@@3freq
	
	
	
@@@@keyoff_all:
	xor a
	jr @@@@keyon_try_write
@@@@keyon_all:
	ld a,$f0
	jr @@@@keyon_try_write
@@@@keyon:
	rst get_byte
@@@@keyon_try_write:
	ld d,(iy+ss_fm_keyon)
	ld (iy+ss_fm_keyon),a
	ld e,a
	
	ld a,(k_temp) ;channel is unoccupied, always take over
	cp $ff
	jr z,@@@@@takeover
	ld b,a
	ld a,(k_song_slot)
	cp b
	jr z,@@@@@write ;we're already in control, just set the keyon
	jp c,@@vgm_data_read_loop ;can't take over, higher song slot is in control
	
	ld a,d ;if any operator was newly keyed on, take control
	cpl
	and e
	jp nz,@@vgm_data_read_loop
	
@@@@@takeover:
	push bc
	push hl
	ld a,(k_song_slot)
	ld hl,(k_temp+1)
	ld (hl),a
	call mute_fm_channel
	call write_fm_patch ;write all sound parameters
	call write_fm_freq
	ld a,(k_fm_channel) ;for channel 3, initialize the aux. freqs too
	cp 2
	jr nz,+
	call write_fm_3freq
	ld hl,k_fm_channel
	dec (hl)
	call write_fm_3freq
	ld hl,k_fm_channel
	dec (hl)
	call write_fm_3freq
	ld a,2
	ld (k_fm_channel),a
+:
	call write_fm_keyon
	
	pop hl
	pop bc
	rst set_bank
	jp @@vgm_data_read_loop
	
@@@@@write:
	push bc
	push hl
	call write_fm_keyon
	pop hl
	pop bc
	jp @@vgm_data_read_loop
	
	
@@@@fm_patch_lo:
	rst get_byte
	ld (iy+ss_fm_patch_lo),a
	ld (iy+ss_fm_patch_hi),0
	jp @@@@fm_patch_try_write
@@@@fm_patch:
	rst get_byte
	ld (iy+ss_fm_patch_lo),a
	rst get_byte
	ld (iy+ss_fm_patch_hi),a
	
@@@@fm_patch_try_write:
	ld a,(k_temp) ;if we're not in control, give up
	ld b,a
	ld a,(k_song_slot)
	cp b
	jp nz,@@vgm_data_read_loop
	;we are, write it
	push bc
	push hl
	call write_fm_patch
	pop hl
	pop bc
	rst set_bank
	jp @@vgm_data_read_loop
	
	
@@@@freq_lo:
	rst get_byte
	ld (iy+ss_fm_pitch_lo),a
	jp @@@@freq_try_write
@@@@freq:
	rst get_byte
	ld (iy+ss_fm_pitch_lo),a
@@@@freq_hi:
	rst get_byte
	ld (iy+ss_fm_pitch_hi),a
	
@@@@freq_try_write:
	ld a,(k_temp) ;if we're not in control, give up
	ld b,a
	ld a,(k_song_slot)
	cp b
	jp nz,@@vgm_data_read_loop
	;we are, write it
	push bc
	push hl
	call write_fm_freq
	pop hl
	pop bc
	jp @@vgm_data_read_loop
	
	
@@@@3freq_lo:
	rst get_byte
	ld (iy+ss_chn3_pitch_lo),a
	jp @@@@3freq_try_write
@@@@3freq:
	rst get_byte
	ld (iy+ss_chn3_pitch_lo),a
@@@@3freq_hi:
	rst get_byte
	ld (iy+ss_chn3_pitch_hi),a
	
@@@@3freq_try_write:
	ld a,(k_fm_channel_ss+2) ;if we're not in control, give up (channel is always 3, k_fm_channel is the operator)
	ld b,a
	ld a,(k_song_slot)
	cp b
	jp nz,@@vgm_data_read_loop
	;we are, write it
	push bc
	push hl
	call write_fm_3freq
	pop hl
	pop bc
	jp @@vgm_data_read_loop
	
	
	
	
	
@@after_vgm_data_read:
	;amount of time for the next event is in DE
	
	ld (ix+ss_ptr+0),l
	ld (ix+ss_ptr+1),h
	ld (ix+ss_ptr+2),c
	
	ld l,(ix+ss_timer+0)
	ld h,(ix+ss_timer+1)
	add hl,de
	ld (ix+ss_timer+0),l
	ld (ix+ss_timer+1),h
	jp nc,@@vgm_data_read_again  ;if the delay was less than a frame
	
	
	
	;;;;;;;;;;;; if lfo is on, control it
	
	ld hl,k_lfo_ss
	ld a,(hl)
	cp $ff
	jr nz,@@no_lfo
	
	ld a,(ix+ss_flags)
	bit 3,a
	jr z,@@no_lfo
	
	exx
	ld hl,$4000
	exx
	
	ld b,$22
	rst fm_write_1
	
	ld a,(k_song_slot)
	ld (hl),a
	
@@no_lfo:
	
	
	
	;;;;;;;;;;;;; init sample, if needed
	
	ld a,(k_sample_init_flag)
	or a
	jr z,@@no_sample_init
	
	ld a,(k_song_slot)
	ld b,a
	
	;first check the current state of channel 6
	ld hl,k_fm_channel_ss+5
	ld a,(hl)
	cp $ff
	jr z,@@sample_takeover ;unoccupied, take it over
	cp b
	jr z,@@sample_init ;already in control, init the sample
	jr nc,@@no_sample_init ;higher channel is in control, do nothing
	
@@sample_takeover:
	ld (hl),b ;signal we're in control
	
	ld a,5
	ld (k_fm_channel),a
	call iy_equals_ix_plus_a
	
	call mute_fm_channel
	call write_fm_patch ;this is necessary, the patch contains some parameters that can affect the DAC output
	call write_fm_freq
	call write_fm_keyon
	
@@sample_init:
	ld a,(k_sample_init_flag)
	ld (k_sample_active),a
	
	;set up sample pointer
	ld a,(k_sample_init_id)
	ld e,a
	ld d,0
	ld hl,(sample_tbl_base)
	ld a,(sample_tbl_base+2)
	call indexed_read_68k_ptr
	ld (k_sample_start+0),hl
	ld (k_sample_ptr+0),hl
	ld a,c
	ld (k_sample_start+2),a
	ld (k_sample_ptr+2),a
	
	
	
@@no_sample_init:
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flag psg, if needed
@@no_vgm_data_read:
	
	
	ld hl,k_song_slot
	ld c,(hl)
	
	ld b,4
	
	ld hl,k_psg_channel_ss
	
	ld iy,(k_song_slot_ptr)
-:
	bit 7,(hl)
	jr z,+
	ld a,(iy+ss_psg_volume)
	or a
	jr z,+
	ld (hl),c
	
+:
	inc hl
	inc iy
	djnz -
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;; next song slot
	
@@next_song_slot:	
	ld de,-ss_size
	add ix,de
	
	ld a,(k_song_slot)
	dec a
	jp p,@song_slot_loop
	
	
	
	
	
	
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; after song slot stuff
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; handle special fm params
	
	exx
	ld hl,$4000
	exx
	
	;;;;;;;;;;;;;;; slot using chn3 controls its mode
	
	ld a,(k_fm_channel_ss+2)
	cp $ff
	jr z,@@no_chn3
	call set_song_slot
	
	ld a,(ix+ss_flags)
	and $40
	ld (k_chn3_state),a
	or $05
	ld b,$27
	rst fm_write_1
	
@@no_chn3:
	
	
	;;;;;;;;;;;;;; slot controlling channel 6 controls the dac mode and sample freq
	
	ld a,(k_fm_channel_ss+5)
	cp $ff
	jr z,@@no_dac_mode
	call set_song_slot
	
	ld a,(ix+ss_flags)
	and $20
	rlca
	rlca
	ld b,$2b
	rst fm_write_1
	
	ld l,(ix+ss_sample_freq+0)
	ld h,(ix+ss_sample_freq+1)
	ld (k_sample_freq+0),hl
	
@@no_dac_mode:
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; write psg
	
	ld a,3
	ld hl,k_psg_channel_ss+3
@@psg_write_loop:
	ld (k_temp),a ;channel id in k_temp
	ld (k_temp+1),hl ;song slot id pointer in k_temp+1
	rrca ;psg channel latch mask in k_temp+2
	rrca
	scf
	rra
	ld (k_temp+3),a
	
	ld a,(hl)
	or a
	jp p,@@@do_it
	
	;not used channel, silence it
	ld a,(k_temp+3)
	or $1f
	ld (PSG),a
	jp @@next_psg_channel_2
	
@@@do_it:
	call set_song_slot
	ld a,(k_temp)
	call iy_equals_ix_plus_a
	
	;output volume
	ld a,(k_temp+3)
	or $10
	or (iy+ss_psg_volume)
	xor $f
	ld (PSG),a
	
	
	;either noise mode or period, depending on channel
	ld a,(k_temp)
	cp 3
	ld a,(iy+ss_psg_pitch_lo)
	jr z,@@@is_noise
	
@@@is_tone:
	ld hl,k_temp+3
	ld b,a
	and $0f
	or (hl)
	ld (PSG),a
	
	ld a,b
	ld b,(iy+ss_psg_pitch_hi)
	.rept 4
		rr b
		rra
	.endr
	ld (PSG),a
	
	jr @@@after_freq
	
@@@is_noise:
	ld hl,k_prv_psg_noise
	cp (hl)
	jr z,+
	ld (hl),a
	or $e0
	ld (PSG),a
+:
	
	cpl ;for extended noise mode, disable tone 2 output, and always write tone2 freq of this song slot
	and 3
	jr nz,@@@after_freq
	ld a,$ff
	ld (k_psg_channel_ss+2),a
	dec iy
	ld a,$c0
	ld (k_temp+3),a
	ld a,(iy+ss_psg_pitch_lo)
	jr @@@is_tone
	
	
@@@after_freq:
	
	
	
@@next_psg_channel:
	ld hl,(k_temp+1)
@@next_psg_channel_2:
	dec hl
	ld a,(k_temp)
	dec a
	jp p,@@psg_write_loop
	
	
	
	
	
	jp main_loop
	




	
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fm channel control routines

	;;;;;; most of the following routines expect:
	;;;;;;	the song slot id in k_song_slot
	;;;;;;	the song slot pointer in ix
	;;;;;;	the channel id in k_fm_channel
	;;;;;;	the channel-indexed pointer in iy
	
	
	
	;sets up shadow regs and returns channel index in A
init_fm_channel_write:
	exx
	ld hl,$4000
	ld b,h
	ld d,h
	
	ld c,l
	ld e,1
	
	ld a,(k_fm_channel)
	cp 3
	jr c,+
	ld c,2
	ld e,3
	sub 3
+:
	exx
	ret
	
	
	
	;in A
get_fm_keyoff_index:
	ld a,(k_fm_channel)
	cp 3
	ret c
	inc a
	ret
	
	
	
	
	
	
	;in A
stop_song_slot:
	call set_song_slot
	
stop_current_song_slot:
	ld (ix+ss_flags),0
	
	
give_up_channels:
	;;;; FM channels
	
	ld iy,(k_song_slot_ptr)
	ld hl,k_fm_channel_ss
	xor a
@loop:
	ld (k_fm_channel),a
	
	ld a,(k_song_slot) ; the channel must be ours
	cp (hl)
	jr nz,@next
	
	ld (hl),$ff ;signal this channel is free
	
	call mute_fm_channel
	
	
	ld a,(k_fm_channel) ;for channel 6, stop sample
	sub 5
	jr nz,+
	ld (k_sample_active),a ;0
+:
	
	
@next:
	inc hl
	inc iy
	ld a,(k_fm_channel)
	inc a
	cp 6
	jr c,@loop
	
	
	
	.endasm
	;;;; PSG
	xor a
	ld (ix+ss_psg_volume+0),a
	ld (ix+ss_psg_volume+1),a
	ld (ix+ss_psg_volume+2),a
	ld (ix+ss_psg_volume+3),a
	.asm
	
	
	
	ret
	
	
	
	
	
	
mute_fm_channel:
	;; mute the channel by setting its RRs to $0f and keying it off
	call init_fm_channel_write
	or $80
	ld b,a
	ld a,$ff
	.rept 3
		rst fm_write
		inc b
		inc b
		inc b
		inc b
	.endr
	rst fm_write
	
	call get_fm_keyoff_index
	ld b,$28
	rst fm_write_1
	
	ret
	
	
	
	
	
	
	
	
write_fm_patch:
	ld e,(iy+ss_fm_patch_lo)
	ld d,(iy+ss_fm_patch_hi)
	ld hl,(fm_patch_tbl_base)
	ld a,(fm_patch_tbl_base+2)
	call indexed_read_68k_ptr
	
	call init_fm_channel_write
	or $30
	ld b,a
	
	.rept (4*7)-1
		rst get_byte
		rst fm_write
		inc b
		inc b
		inc b
		inc b
	.endr
	rst get_byte
	rst fm_write
	ld a,b
	add $14
	ld b,a
	rst get_byte
	rst fm_write
	inc b
	inc b
	inc b
	inc b
	rst get_byte
	rst fm_write
	
	ret
	



write_fm_freq:
	call init_fm_channel_write
	or $a4
	ld b,a
	
	ld a,(iy+ss_fm_pitch_hi)
	rst fm_write
	dec b
	dec b
	dec b
	dec b
	ld a,(iy+ss_fm_pitch_lo)
	rst fm_write
	
	ret
	
	
	
	;on entry to this routine, k_fm_channel is the operator, not the channel
write_fm_3freq:
	call init_fm_channel_write
	or $ac
	ld b,a
	
	ld a,(iy+ss_chn3_pitch_hi)
	rst fm_write
	dec b
	dec b
	dec b
	dec b
	ld a,(iy+ss_chn3_pitch_lo)
	rst fm_write
	
	ret
	
	
	
	
write_fm_keyon:
	exx
	ld hl,$4000
	exx
	
	call get_fm_keyoff_index
	or (iy+ss_fm_keyon)
	ld b,$28
	rst fm_write_1
	
	ret











	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc routines

	
	
step_ptr_ahl:
	ld c,a
	;add de to banked pointer chl, then set the bank
step_ptr:
	add hl,de
	jr nc,+
	inc c
	ld de,$8000
	add hl,de
	jr nc,+
	inc c
	set 7,h
+:	jp set_bank



	;read 68k pointer indirect from (banked ahl)+(de*4)+1
indexed_read_68k_ptr:
	ex de,hl
	add hl,hl
	add hl,hl
	inc hl
	ex de,hl
	call step_ptr_ahl
	
	;read 68k pointer from banked pointer chl, convert into chl, then set bank
read_68k_ptr:
	rst get_byte
	ld b,a
	rst get_byte
	ld d,a
	rst get_byte
	ld l,a
	ld a,d
	rlca
	rl b
	set 7,d
	ld h,d
	ld c,b
	jp set_bank





	;to A
set_song_slot:
	ld (k_song_slot),a
	
	rlca
	ld e,a
	ld d,0
	ld hl,song_slot_ptr_tbl
	add hl,de
	
	ld a,(hl)
	ld ixl,a
	inc hl
	ld a,(hl)
	ld ixh,a
	
	ld (k_song_slot_ptr),ix
	ret



	;what do you think?
iy_equals_ix_plus_a:
	add ixl
	ld iyl,a
	ld a,0
	adc ixh
	ld iyh,a
	ret











	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data
	
	
song_slot_ptr_tbl:
	.redefine idx 0
	.rept KNL_SONG_SLOTS
		.dw ss_size*idx + k_song_slots
		.redefine idx idx+1
	.endr
	
	
frame_timer_tbl:
	.redefine idx 1
	.rept $20
		.dw NTSC_INTERVAL*idx
		.redefine idx idx+1
	.endr
	.redefine idx 1
	.rept $20
		.dw PAL_INTERVAL*idx
		.redefine idx idx+1
	.endr




