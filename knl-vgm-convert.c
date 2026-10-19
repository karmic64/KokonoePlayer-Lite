
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <sys/stat.h>



/*

data stream format (per-channel):
	$00-$7f: wait $xx base intervals
	$80-$9f $yy: set fm patch parameter $xx to $yy
	$a0-$df: add to pitch (base $c0)
	$e0-$ef: set psg volume/fm op keyon state
	$f0 $yyyy: set pitch
	$f1 $yy: set low pitch
	$f2 $yy: set high pitch
	$f3: chn.3 normal/dac off
	$f4: chn.3 extd/dac on
	$f5 $yy: set lfo
	$f6-$f8 $yyyy: set chn.3 op pitch
	$f9 $yyyy: set sample output frequency
	$fa $yy: start sample
	$fb $yy: start looped sample
	$fc: stop sample
	$fd $yyyyyy: jump
	$fe $yyyyyy: call
	$ff: return from call, or stop if not in call

*/

const unsigned char command_size_tbl[0x100] = {
	// 00-7f waits
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	// 80-9f fm patch
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	// a0-df add to pitch
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	// e0-ef volume
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	// f0-ff control
	3,2,2,1,1,2,3,3,3,3,2,2,1,4,4,1,
};



#define min(a,b) (((a) < (b)) ? (a) : (b))

#define CHANNELS 10
#define FM_CHANNELS 6
#define PSG_CHANNELS 4
#define FIRST_FM_CHANNEL 0
#define FIRST_PSG_CHANNEL 6
#define FM_CHN3 2
#define FM_DAC_CHANNEL 5
#define PSG_NOISE_CHANNEL 9

/* see vgm spec */
#define NTSC_INTERVAL 735
#define PAL_INTERVAL 882

/* for NTSC, but PAL isn't different enough to worry about it */
#define Z80_CLOCK 3579545.0
#define Z80_CLOCKS_PER_SAMPLE 104.0



/* indexed_read_68k_ptr accepts a 16-bit id, and shifts out the upper 2 bits */
#define MAX_SONGS 0x4000

/* k_sample_init_id is 1 byte long (this could be trivially fixed if a need ever arises */
#define MAX_SAMPLES 0x100


unsigned get16(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8); }
unsigned get24(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16); }
unsigned get32(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16) | (*(p+3)<<24); }



/*************** structs ****************/

/////// fm patch

typedef struct {
	short reg30[4];
	short reg40[4];
	short reg50[4];
	short reg60[4];
	short reg70[4];
	short reg80[4];
	short reg90[4];
	short regb0;
	short regb4;
}	fm_patch_t;

int fm_patch_is_audible(const fm_patch_t * fm) {
	for (int i = 0; i < 4; i++) {
		if (fm->reg40[i] != 0x7f)
			return 1;
	}
	return 0;
}

int fm_patch_uses_lfo(const fm_patch_t * fm) {
	if (!fm_patch_is_audible(fm))
		return 0;

	if (fm->regb4 & 0x07) return 1;	// PMS is on
	if (!(fm->regb4 & 0x30)) return 0;	// AMS is off
	for (int i = 0; i < 4; i++) {	// an operator uses AM
		if ((fm->reg40[i] != 0x7f) && (fm->reg60[i] & 0x80))
			return 1;
	}

	return 0;
}


////////////// sample

typedef struct {
	uint8_t * data;
	size_t size;
}	sample_t;

int sample_data_cmp(sample_t *to_add, sample_t *compared)
{
	if (to_add->size > compared->size) return 1;
	return memcmp(to_add->data, compared->data, to_add->size);
}

int sample_cmp(sample_t *a, sample_t *b)
{
	if (a->size != b->size) return 1;
	return memcmp(a->data, b->data, a->size);
}

unsigned add_sample(sample_t *);	// forward-declare add function



/////// playback state

typedef struct {
	short lo;
	short hi;
} lo_hi_t;

typedef struct {
	fm_patch_t patch;
	lo_hi_t freq;
	int8_t keyon;
} fm_channel_state_t;

typedef struct {
	fm_channel_state_t chn[FM_CHANNELS];
	int16_t extd_freq[3];
	int8_t chn3_mode;
	int8_t dac_enable;
	int8_t lfo;
} fm_state_t;

typedef struct {
	lo_hi_t freq;
	int8_t volume;
} psg_channel_state_t;

typedef struct {
	psg_channel_state_t chn[PSG_CHANNELS];
} psg_state_t;

typedef struct {
	fm_state_t fm;
	psg_state_t psg;

	int queued_wait[CHANNELS];
	int sample_freq;
	// lfo affects all channels, but our datastream is per-channel.
	// so we need to be able to "log" the write until we find an enabled
	// channel using the lfo, where we will put the lfo enable command
	int8_t queued_lfo;
} playback_state_t;

void init_playback_state(playback_state_t *p)
{
	memset(p, -1, sizeof(*p));
	memset(p->queued_wait, 0, sizeof(p->queued_wait));
}

void reset_playback_state(playback_state_t *p) {	// used on loop to disable the same-value-rewrite detection
	memset(&p->fm, -1, sizeof(p->fm));
	memset(&p->psg, -1, sizeof(p->psg));
	p->sample_freq = -1;
}



/////////// song

// temporary command values
#define LOOP_CMD	((unsigned)-5)	// defines song loop point in the uncompressed datastream
#define END_CMD	((unsigned)-4)	// defines end of a song in the uncompressed datastream
#define LAB_CMD	((unsigned)-3)	// used to define label numbers in the compressed datastream
#define NOP_CMD ((unsigned)-2)	// used to hold "holes" in the datastream
#define CALL_CMD	((unsigned)-1)	// used when building the pairs to denote a token id

// flags for the sequences_used member
#define FM_USED_KEYON (1<<0)	// was keyed on
#define FM_USED_NZ_FREQ (1<<1)	// channel set to nonzero freq
#define FM_USED_NZ_TL (1<<2)	// channel operator TL was set to nonzero (actually non-$7f)
#define FM_USED 0x07

#define PSG_USED_NZ_VOL (1<<0)	// volume was set to nonzero (actually non-$0f)
#define PSG_USED_NZ_FREQ (1<<1)	// period was set to nonzero
#define PSG_USED 0x03

typedef struct {	// this struct should always be memset before use to allow safe memcmp'ing
	unsigned cmd;
	unsigned param;
} song_command_t;

typedef struct {
	song_command_t * data;
	size_t length;
	size_t max;	// current allocated length of data array
} song_command_sequence_t;

typedef struct {
	song_command_sequence_t sequences[CHANNELS];
	int sequences_used[CHANNELS];	// these hold flags that indicate if the sequence should be written

	int base_interval;

	int has_loop;
	size_t sequence_label_ids[CHANNELS];
	size_t sequence_loop_label_ids[CHANNELS];
} song_t;

void init_song(song_t * song, int base_interval) {
	memset(song, 0, sizeof(*song));
	for (int i = 0; i < CHANNELS; i++) {
		song->sequences[i].data = malloc(0x1000 * sizeof(*song->sequences[i].data));
		song->sequences[i].max = 0x1000;
	}

	song->base_interval = base_interval;

	memset(song->sequence_label_ids, -1, sizeof(song->sequence_label_ids));
	memset(song->sequence_loop_label_ids, -1, sizeof(song->sequence_loop_label_ids));

	song->sequences_used[PSG_NOISE_CHANNEL] = PSG_USED_NZ_FREQ;	// because no freq is inaudible on this channel
}

void free_song_command_sequence(song_command_sequence_t * seq) {
	free(seq->data);
	seq->data = NULL;
	seq->length = 0;
	seq->max = 0;
}

void free_song(song_t * song) {
	for (int i = 0; i < CHANNELS; i++) {
		free_song_command_sequence(&song->sequences[i]);
		song->sequences_used[i] = 0;
	}
}

int song_command_sequence_cmp(song_command_sequence_t *a, song_command_sequence_t *b)
{
	if (a->length != b->length) return 1;
	return memcmp(a->data, b->data, a->length * sizeof(song_command_t));
}

int song_cmp(song_t * a, song_t * b) {
	// not implemented.....
	return 1;
}

void write_song_command(song_command_sequence_t * seq, unsigned cmd, unsigned param) {
	if (seq->length == seq->max) {
		size_t old_seq_max = seq->max;
		if (!seq->max) seq->max = 0x20000;
		else seq->max *= 2;
		seq->data = realloc(seq->data,seq->max*sizeof(*seq->data));
		memset(&seq->data[old_seq_max], 0, (seq->max-old_seq_max)*sizeof(*seq->data));
	}
	song_command_t * sc = &seq->data[seq->length++];
	sc->cmd = cmd;
	sc->param = param;
}

int song_channel_is_used(const song_t * s, int chn) {
	return s->sequences_used[chn] == (chn < FIRST_PSG_CHANNEL ? FM_USED : PSG_USED);
};

void flush_queued_wait(song_t * s, playback_state_t * state, int chn) {
	int waits = 0;
	while (state->queued_wait[chn] >= s->base_interval) {
		waits++;
		state->queued_wait[chn] -= s->base_interval;
	}

	while (waits) {
		int this_wait = min(waits, 0x80);
		write_song_command(&s->sequences[chn], (waits-1), 0);
		waits -= this_wait;
	}
}

void queue_wait(playback_state_t * state, int wait) {
	for (int i = 0; i < CHANNELS; i++) {
		state->queued_wait[i] += wait;
	}
}

void write_song_chn_command(song_t * song, playback_state_t * state, int chn, unsigned cmd, unsigned param) {
	flush_queued_wait(song, state, chn);
	write_song_command(&song->sequences[chn], cmd, param);
}

void write_song_chn_pitch_change(song_t * song, playback_state_t * state, int chn, int lo, int hi) {
	if (lo < 0 && hi < 0) {	// filter out totally unknown new pitch
		return;
	}

	lo_hi_t * prv = chn < FIRST_PSG_CHANNEL ? &state->fm.chn[chn-FIRST_FM_CHANNEL].freq : &state->psg.chn[chn-FIRST_PSG_CHANNEL].freq;
	int prv_lo_known = prv->lo >= 0;
	int prv_hi_known = prv->hi >= 0;
	int prv_known = prv_lo_known && prv_hi_known;
	int new_lo_known = lo >= 0;
	int new_hi_known = hi >= 0;
	int new_known = (prv_lo_known || new_lo_known) && (prv_hi_known || new_hi_known);
	int lo_changed = new_lo_known && prv->lo != lo;
	int hi_changed = new_hi_known && prv->hi != hi;

	int prv_comb = prv_known ? (prv->lo | (prv->hi<<8)) : -1;
	int comb = new_known
		? (new_lo_known ? lo : prv->lo) | ((new_hi_known ? hi : prv->hi)<<8)
		: -1;
	if (comb > 0) {
		song->sequences_used[chn] |= (chn < FIRST_PSG_CHANNEL ? FM_USED_NZ_FREQ : PSG_USED_NZ_FREQ);
	}

	int diff = prv_known && new_known ? comb - prv_comb : INT_MAX;
	int lo_diff = prv_lo_known && new_lo_known && !hi_changed ? lo - prv->lo : INT_MAX;

	if (diff > -0x20 && diff < 0x20) {
		if (diff) {
			write_song_chn_command(song, state, chn, 0xc0 + diff, 0);
			prv->lo = lo;
			prv->hi = hi;
		}
	} else if (lo_diff > -0x20 && lo_diff < 0x20) {
		if (lo_diff) {
			write_song_chn_command(song, state, chn, 0xc0 + lo_diff, 0);
			prv->lo = lo;
		}
	} else if (lo_changed && hi_changed) {
		write_song_chn_command(song, state, chn, 0xf0, comb);
		prv->lo = lo;
		prv->hi = hi;
	} else if (lo_changed) {
		write_song_chn_command(song, state, chn, 0xf1, lo);
		prv->lo = lo;
	} else if (hi_changed) {
		write_song_chn_command(song, state, chn, 0xf2, hi);
		prv->hi = hi;
	}
}

unsigned add_song(song_t *);	// forward-declare add function







////////// histogram

typedef struct {
	unsigned id;
	size_t count;
} histogram_ent_t;

int histogram_ent_cmp_desc(const void * a, const void * b)
{
	size_t ca = ((const histogram_ent_t*)a)->count;
	size_t cb = ((const histogram_ent_t*)b)->count;
	return -((ca > cb) - (ca < cb));
}





/***************** storage *******************/

sample_t *sample_tbl = NULL;
size_t samples = 0;
size_t samples_max = 0;
size_t samples_size = 0;

song_t *song_tbl = NULL;
size_t songs = 0;
size_t songs_max = 0;
size_t songs_size = 0;


#define MAKE_ADD_FUNC(func_name, type_name, table, counter, max_counter, initial_max, cmp_func) \
unsigned func_name(type_name *p) { \
	for (size_t i = 0; i < counter; i++) \
	{ \
		if (!cmp_func(p, &table[i])) return i; \
	} \
	 \
	if (counter == max_counter) \
	{ \
		if (!max_counter) \
			max_counter = initial_max; \
		else \
			max_counter *= 2; \
		table = realloc(table, sizeof(type_name)*max_counter); \
	} \
	memcpy(&table[counter], p, sizeof(type_name)); \
	return counter++; \
}

MAKE_ADD_FUNC(add_sample, sample_t, sample_tbl, samples, samples_max, 0x20, sample_cmp)
MAKE_ADD_FUNC(add_song, song_t, song_tbl, songs, songs_max, 0x20, song_cmp)

#undef MAKE_ADD_FUNC








/************************* main load_vgm function *************************/

#define GET_VGM_BYTE(var) \
	int var = fgetc(vgm_f); \
	if (var == EOF) { \
		goto vgm_read_error; \
	}

int load_vgm(char * vgm_filename, int base_interval)
{
	int return_val = 1;
	
	
	/********** save old data counts for fail cleanup *************/
	size_t old_samples = samples;
	size_t old_songs = songs;
	size_t old_samples_size = samples_size;
	size_t old_songs_size = songs_size;
	
	
	/******** variables **********/
	struct stat vgm_stat;
	FILE * vgm_f = NULL;
	song_t song;

	int active_stream_id = -1; /* the stream that last started a sample */
	uint8_t stream_active_flags[0x100/8];
	uint8_t stream_bank_tbl[0x100];
	uint8_t stream_step_tbl[0x100];
	uint8_t stream_base_tbl[0x100];
	uint16_t stream_freq_tbl[0x100];
	memset(&stream_active_flags,0,sizeof(stream_active_flags));
	memset(&stream_bank_tbl,0,sizeof(stream_bank_tbl));
	memset(&stream_step_tbl,0,sizeof(stream_step_tbl));
	memset(&stream_base_tbl,0,sizeof(stream_base_tbl));
	memset(&stream_freq_tbl,0,sizeof(stream_freq_tbl));

	unsigned stream_data_blocks = 0;
	uint8_t *stream_data_block_tbl[0x100];
	unsigned stream_data_block_sizes[0x100];
	memset(&stream_data_block_tbl, 0, sizeof(stream_data_block_tbl));
	memset(&stream_data_block_sizes, 0, sizeof(stream_data_block_sizes));
	
	
	/************************** try opening the file **********************/
	printf("Reading %s...",vgm_filename);
	
	if (stat(vgm_filename, &vgm_stat)) {
		printf("can't stat: %s\n", strerror(errno));
		goto load_vgm_fail;
	}
	off_t vgm_size = vgm_stat.st_size;

	unsigned char vgm_header[0x40];
	vgm_f = fopen(vgm_filename,"rb");
	if (!vgm_f)
	{
		printf("can't open: %s\n", strerror(errno));
		goto load_vgm_fail;
	}
	size_t vgm_header_read = fread(vgm_header,1,0x40,vgm_f);
	if (vgm_header_read != 0x40)
	{
		printf("header read error: %s\n", strerror(errno));
		goto load_vgm_fail;
	}
	
	
	
	/********* check file validity ******************/
	if (memcmp(vgm_header,"Vgm ",4))
	{
		puts("not a vgm");
		goto load_vgm_fail;
	}
	unsigned vgm_reported_size = get32(vgm_header+4)+4;
	if (vgm_reported_size != vgm_size)
	{
		printf("bad vgm size (%zu bytes, should be %u)", vgm_size, vgm_reported_size);
		goto load_vgm_fail;
	}
	unsigned vgm_version = get32(vgm_header+8);
	unsigned vgm_loop_offset = get32(vgm_header+0x1c);
	if (vgm_loop_offset) vgm_loop_offset += 0x1c;
	unsigned vgm_data_offset = vgm_version < 0x150 ? 0x40 : get32(vgm_header+0x34)+0x34;
	if (vgm_data_offset >= vgm_size)
	{
		puts("bad data offset");
		goto load_vgm_fail;
	}
	if (vgm_loop_offset >= vgm_size || (vgm_loop_offset && vgm_loop_offset < vgm_data_offset))
	{
		puts("bad loop offset");
		goto load_vgm_fail;
	}
	printf("OK, VGM version %X.%02X.\n", vgm_version>>8, vgm_version&0xff);
	
	
	
	/*********************** read the vgm datastream ***************************/

	init_song(&song, base_interval);
	
	playback_state_t state;
	init_playback_state(&state);
	
	int loop_hit = 0;
	fseek(vgm_f, vgm_data_offset, SEEK_SET);
	while (1) {
		long cmd_offs = ftell(vgm_f);

		// first check for loop
		if (!loop_hit && vgm_loop_offset) {
			if (cmd_offs == vgm_loop_offset) {
				loop_hit = 1;
				song.has_loop = 1;
				for (int i = 0; i < CHANNELS; i++) {
					flush_queued_wait(&song, &state, i);
					write_song_command(&song.sequences[i], LOOP_CMD, 0);
				}
				reset_playback_state(&state);
			} else if (cmd_offs > vgm_loop_offset) {
				printf("WARNING: Passed loop point at $%X. Song will not loop.\n", vgm_loop_offset);
				loop_hit = 1;
			}
		}

		// then start parsing commands
		GET_VGM_BYTE(cmd);
		/******************** END COMMAND ***********************/
		if (cmd == 0x66) {
			for (int i = 0; i < CHANNELS; i++) {
				write_song_chn_command(&song, &state, i, END_CMD, 0);
			}
			break;
		} /********************* WAIT COMMANDS *******************/
		else if (cmd == 0x61) {
			GET_VGM_BYTE(lo);
			GET_VGM_BYTE(hi);
			queue_wait(&state, lo | (hi<<8));
		} else if (cmd == 0x62) {
			queue_wait(&state, 735);
		} else if (cmd == 0x63) {
			queue_wait(&state, 882);
		} else if (cmd >= 0x70 && cmd <= 0x7f) {
			queue_wait(&state, (cmd&0x0f)+1);
		} /******************** DATA BLOCKS *********************/
		else if (cmd == 0x67) {
			fseek(vgm_f, 1, SEEK_CUR);	// skip the 0x66 compatibility command
			GET_VGM_BYTE(type);
			GET_VGM_BYTE(s1);
			GET_VGM_BYTE(s2);
			GET_VGM_BYTE(s3);
			GET_VGM_BYTE(s4);
			unsigned size = s1|(s2<<8)|(s3<<16)|(s4<<24);

			if (type != 0) {
				printf("WARNING: Skipping data block of type %02X at %lX\n", type, cmd_offs);
				fseek(vgm_f, size, SEEK_CUR);
			} else {
				unsigned char * block = malloc(size);
				size_t block_read = fread(block, 1, size, vgm_f);
				if (block_read != size)
					goto vgm_read_error;

				stream_data_block_tbl[stream_data_blocks] = block;
				stream_data_block_sizes[stream_data_blocks] = size;
				stream_data_blocks++;
			}
		} /********************* SAMPLES ***********************/
		else if (cmd == 0x90) {	// stream setup
			GET_VGM_BYTE(sid);
			GET_VGM_BYTE(chip);
			GET_VGM_BYTE(port);
			GET_VGM_BYTE(reg);
			if (chip == 2 && !port && reg == 0x2a) {
				stream_active_flags[sid/8] |= 1 << (sid&7);
			}
		} else if (cmd == 0x91) {	// stream data
			GET_VGM_BYTE(sid);
			GET_VGM_BYTE(bank);
			GET_VGM_BYTE(step);
			GET_VGM_BYTE(base);

			stream_bank_tbl[sid] = bank;
			stream_step_tbl[sid] = step;
			stream_base_tbl[sid] = base;
		} else if (cmd == 0x92) {	// stream frequency
			GET_VGM_BYTE(sid);
			GET_VGM_BYTE(f1);
			GET_VGM_BYTE(f2);
			GET_VGM_BYTE(f3);
			GET_VGM_BYTE(f4);
			int freq = f1|(f2<<8)|(f3<<16)|(f4<<24);

			/* right now we have freq in hz, change it to native value */
			freq = round(freq * (Z80_CLOCKS_PER_SAMPLE / Z80_CLOCK) * 256.0);

			stream_freq_tbl[sid] = freq;

			/* set */
			if ((stream_active_flags[sid/8] & (1 << (sid&7)))) {
				if (freq != state.sample_freq) {
					state.sample_freq = freq;
					write_song_chn_command(&song, &state, FM_DAC_CHANNEL, 0xf9, freq);
				}
			}
		} else if (cmd == 0x94) {	// stop stream
			GET_VGM_BYTE(sid);
			if (sid == active_stream_id) {
				write_song_chn_command(&song, &state, FM_DAC_CHANNEL, 0xfc, 0);
			}
		} else if (cmd == 0x95) {	// fast start stream
			GET_VGM_BYTE(sid);
			GET_VGM_BYTE(b1);
			GET_VGM_BYTE(b2);
			GET_VGM_BYTE(flags);
			unsigned block = b1|(b2<<8);

			if (block < stream_data_blocks && (stream_active_flags[sid/8] & (1 << (sid&7))))
			{
				active_stream_id = sid;

				uint8_t *bp = stream_data_block_tbl[block];
				sample_t new;
				new.data = bp;
				new.size = stream_data_block_sizes[block];

				/* if we're not looping, crop out trailing silence */
				if (!(flags & 1))
				{
					size_t cropped_size = new.size;

					while (cropped_size > 1)
					{
						if (abs(new.data[cropped_size-2]-new.data[cropped_size-1]) > 4) break;

						cropped_size--;
					}
					if (cropped_size < 2) cropped_size = 0;

					new.size = cropped_size;
				}

				if (!new.size)
				{ /* we cropped everything (or the sample is just empty), just stop */
					write_song_chn_command(&song, &state, FM_DAC_CHANNEL, 0xfc, 0);
				}
				else
				{
					/* add it to the sample list */
					size_t expected_id = samples;
					size_t new_id = add_sample(&new);
					if (new_id == expected_id)
					{
						sample_t *new_added = &sample_tbl[new_id];
						new_added->data = malloc(new_added->size);
						memcpy(new_added->data,bp,new_added->size);
						samples_size += new_added->size;
					}

					/* play command */
					write_song_chn_command(&song, &state, FM_DAC_CHANNEL, 0xfa + ((flags & 1) ? 1 : 0), new_id);
					song.sequences_used[FM_DAC_CHANNEL] = FM_USED;	// special case for this...
				}
			}
		} /******************** FM COMMANDS *********************/
		else if (cmd == 0x52 || cmd == 0x53) {
			GET_VGM_BYTE(reg);
			GET_VGM_BYTE(val);
			int part = cmd & 1;

			// main command handler
			if (!part && reg == 0x22) {	// lfo
				state.queued_lfo = val & 0x0f;
			} else if (!part && reg == 0x27) {	// extd. chn3
				int enable = val & 0x40;
				if (enable != state.fm.chn3_mode) {
					write_song_chn_command(&song, &state, FM_CHN3, 0xf3 + (enable ? 1 : 0), 0);
					state.fm.chn3_mode = enable;
				}
			} else if (!part && reg == 0x2b) {	// dac toggle
				int enable = val & 0x80;
				if (enable != state.fm.dac_enable) {
					write_song_chn_command(&song, &state, FM_DAC_CHANNEL, 0xf3 + (enable ? 1 : 0), 0);
					state.fm.dac_enable = enable;
				}
			} else if (!part && reg == 0x28) {	// keyon
				if ((val & 3) != 3) {
					int chn = val & 7;
					if (chn >= 4) chn--;
					int keyon = val >> 4;

					if (keyon) {
						song.sequences_used[chn+FIRST_FM_CHANNEL] |= FM_USED_KEYON;
					}
					if (keyon != state.fm.chn[chn].keyon) {
						write_song_chn_command(&song, &state, chn+FIRST_FM_CHANNEL, 0xe0 + keyon, 0);
						state.fm.chn[chn].keyon = keyon;
					}
				}
			} else if ((reg >= 0x30 && reg < 0x9f) || (reg >= 0xb0 && reg < 0xb7)) {	// fm patch register
				if ((reg & 3) != 3) {
					int chn = (reg & 3) + (part ? 3 : 0);
					int op = (reg & 0x0c) >> 2;
					int p1 = reg & 0xf0;
					int p2 = reg & 0xfc;

					fm_patch_t * p = &state.fm.chn[chn].patch;
					int id;
					short * cur_val = NULL;
					int v2 = val;
					if (p1 == 0x30) {
						cur_val = &p->reg30[op];
						id = 0x00 + op;
						v2 &= 0x7f;
					} else if (p1 == 0x40) {
						cur_val = &p->reg40[op];
						id = 0x04 + op;
						v2 &= 0x7f;
						if (v2 != 0x7f) {
							song.sequences_used[chn+FIRST_FM_CHANNEL] |= FM_USED_NZ_TL;
						}
					} else if (p1 == 0x50) {
						cur_val = &p->reg50[op];
						id = 0x08 + op;
						v2 &= 0xdf;
					} else if (p1 == 0x60) {
						cur_val = &p->reg60[op];
						id = 0x0c + op;
						v2 &= 0x9f;
					} else if (p1 == 0x70) {
						cur_val = &p->reg70[op];
						id = 0x10 + op;
						v2 &= 0x1f;
					} else if (p1 == 0x80) {
						cur_val = &p->reg80[op];
						id = 0x14 + op;
					} else if (p1 == 0x90) {
						cur_val = &p->reg90[op];
						id = 0x18 + op;
						v2 &= 0x0f;
					} else if (p2 == 0xb0) {
						cur_val = &p->regb0;
						id = 0x1c;
						v2 &= 0x3f;
					} else if (p2 == 0xb4) {
						cur_val = &p->regb4;
						id = 0x1d;
						v2 &= 0xf7;
					}

					if (cur_val) {
						if (*cur_val != v2) {
							write_song_chn_command(&song, &state, chn+FIRST_FM_CHANNEL, 0x80 + id, v2);
							*cur_val = v2;
						}
					}
				}
			} else if (reg >= 0xa0 && reg != 0xa3 && reg < 0xa7) {	// frequency
				int chn = (reg & 3) + (part ? 3 : 0);
				int is_hi = (reg & 4);

				int lo = is_hi ? -1 : val;
				int hi = is_hi ? (val & 0x3f) : -1;

				// peek ahead at the file to look for a consecutive frequency command on this channel
				int check_peek_reg = reg & (~4);
				int peek_cmd = EOF;
				int peek_reg = EOF;
				int peek_val = EOF;
				while (1) {
					peek_cmd = fgetc(vgm_f);
					peek_reg = fgetc(vgm_f);
					peek_val = fgetc(vgm_f);

					if (peek_cmd == cmd && (peek_reg & (~4)) == check_peek_reg) {
						if (peek_reg & 4) {
							hi = (peek_val & 0x3f);
						} else {
							lo = peek_val;
						}
					} else {
						break;
					}
				}
				ungetc(peek_val, vgm_f);
				ungetc(peek_reg, vgm_f);
				ungetc(peek_cmd, vgm_f);

				// now change the frequency of this channel
				write_song_chn_pitch_change(&song, &state, chn+FIRST_FM_CHANNEL, lo, hi);
			} else if (reg >= 0xa8 && reg != 0xab && reg < 0xaf) {	// extd chn.3 frequency
				int op = reg & 3;
				int is_hi = reg & 4;

				int lo = is_hi ? -1 : val;
				int hi = is_hi ? (val & 0x3f) : -1;

				// peek ahead at the file to look for a consecutive frequency command on this channel
				int check_peek_reg = reg & (~4);
				int peek_cmd = EOF;
				int peek_reg = EOF;
				int peek_val = EOF;
				while (1) {
					peek_cmd = fgetc(vgm_f);
					peek_reg = fgetc(vgm_f);
					peek_val = fgetc(vgm_f);

					if (peek_cmd == cmd && (peek_reg & (~4)) == check_peek_reg) {
						if (peek_reg & 4) {
							hi = (peek_val & 0x3f);
						} else {
							lo = peek_val;
						}
					} else {
						break;
					}
				}
				ungetc(peek_val, vgm_f);
				ungetc(peek_reg, vgm_f);
				ungetc(peek_cmd, vgm_f);

				// now change the frequency of this operator
				int16_t * prv = &state.fm.extd_freq[op];
				int comb = lo | (hi<<8);
				if (*prv != comb) {
					write_song_chn_command(&song, &state, FM_CHN3, 0xf6+op, comb);
					*prv = comb;
				}
			}

			// handle a queued lfo command
			if (state.queued_lfo >= 0) {
				int found_chn = -1;
				for (int i = 0; i < FM_CHANNELS; i++) {
					if (fm_patch_uses_lfo(&state.fm.chn[i].patch)) {
						found_chn = i;
						break;
					}
				}

				if (found_chn >= 0) {
					if (state.fm.lfo != state.queued_lfo) {
						write_song_chn_command(&song, &state, found_chn+FIRST_FM_CHANNEL, 0xf5, state.queued_lfo);
						state.fm.lfo = state.queued_lfo;
					}
					state.queued_lfo = -1;
				}
			}
		} /******************** PSG COMMANDS ********************/
		else if (cmd == 0x50) {
			GET_VGM_BYTE(val);
			if (!(val & 0x80)) {
				printf("ERROR: stranded PSG data write at %lX\n", cmd_offs);
				goto vgm_read_error;
			}

			int chn = (val & 0x60) >> 5;
			int type = (val & 0x10);
			int data = (val & 0x0f);

			// peek ahead for data writes
			int second_data = -1;
			int peek_cmd = EOF;
			int peek_val = EOF;
			while (1) {
				peek_cmd = fgetc(vgm_f);
				peek_val = fgetc(vgm_f);

				if (peek_cmd == 0x50 && !(peek_val & 0x80)) {
					second_data = peek_val & 0x3f;
				} else {
					break;
				}
			}
			ungetc(peek_val, vgm_f);
			ungetc(peek_cmd, vgm_f);

			// act on the value
			if (type) {	// volume write
				int8_t * prv = &state.psg.chn[chn].volume;
				int new = second_data < 0 ? val : second_data;
				new &= 0x0f;

				if (*prv != new) {
					if (new != 0x0f) {
						song.sequences_used[chn+FIRST_PSG_CHANNEL] |= PSG_USED_NZ_VOL;
					}
					write_song_chn_command(&song, &state, chn+FIRST_PSG_CHANNEL, 0xe0 + new, 0);
					*prv = new;
				}
			} else if (chn == 3) {	// noise period write
				int new = second_data < 0 ? val : second_data;
				new &= 7;
				write_song_chn_pitch_change(&song, &state, PSG_NOISE_CHANNEL, new, -1);
			} else {	// period write
				if (second_data < 0) {	// we cannot support incomplete writes since the register is not split evenly by byte
					printf("ERROR: PSG freq write without data at %lX\n", cmd_offs);
					goto vgm_read_error;
				}

				int new = data | (second_data<<4);
				int lo = new & 0xff;
				int hi = (new>>8) & 3;
				write_song_chn_pitch_change(&song, &state, chn+FIRST_PSG_CHANNEL, lo, hi);
			}
		}
		/********************* BAD COMMANDS *********************/
		else if (cmd >= 0x30 && cmd < 0x51)
			fseek(vgm_f, 1, SEEK_CUR);
		else if (cmd >= 0x51 && cmd < 0x60)
			fseek(vgm_f, 2, SEEK_CUR);
		else if (cmd >= 0xa0 && cmd < 0xc0)
			fseek(vgm_f, 2, SEEK_CUR);
		else if (cmd >= 0xc0 && cmd < 0xe0)
			fseek(vgm_f, 3, SEEK_CUR);
		else if (cmd >= 0xe1)
			fseek(vgm_f, 4, SEEK_CUR);
		else
		{
			printf("Impossible command $%02X at $%lX. Cannot continue.\n",cmd,cmd_offs);
			goto load_vgm_fail;
		}
	}

	
	
	
	
	
	
	/********************************** final ending *****************************/
	
	puts("Success.");
	return_val = 0;
	
	for (int i = 0; i < CHANNELS; i++) {
		if (song_channel_is_used(&song, i)) {
			for (size_t j = 0; j < song.sequences[i].length; j++) {
				unsigned cmd = song.sequences[i].data[i].cmd;
				if (cmd < 0x100) {
					songs_size += command_size_tbl[cmd];
				}
			}
		} else {
			free_song_command_sequence(&song.sequences[i]);
		}
	}
	add_song(&song);
	
	goto load_vgm_cleanup;

vgm_read_error:
	printf("READ ERROR: %s at 0x%lX\n", ferror(vgm_f) ? strerror(errno) : "premature end-of-file", ftell(vgm_f));
	
load_vgm_fail:
	puts("Failed.");
	
	/* discard data generated by a bad vgm */
	free_song(&song);
	for (size_t i = old_samples; i < samples; i++)
		free(sample_tbl[i].data);
	for (size_t i = old_songs; i < songs; i++)
		free_song(&song_tbl[i]);
	samples = old_samples;
	songs = old_songs;
	songs_size = old_songs_size;
	samples_size = old_samples_size;
	
	
load_vgm_cleanup:
	if (vgm_f) {	// close vgm file
		fclose(vgm_f);
	}

	for (unsigned i = 0; i < stream_data_blocks; i++) {	// free data blocks
		free(stream_data_block_tbl[i]);
	}

	return return_val;
}






/********************** song datastream compression **********************/
// the basic idea of the compression is that we want to find sequences of
// commands which regularly appear in the song datastreams and turn these
// into "subroutines" which we only include once and call instead.
// we use a recursive pairing algorithm to find these sequences- for each
// iteration, find the most commonly occurring sequential pair in the datastream
// and turn instances of this into a single "token". repeat until no repeating
// pairs exist.
// this kind of compression is better for our use case as we don't need any kind
// of backbuffer/dictionary in z80 ram, we just need a call stack for each song slot

// a few notes:
// - the "loop point here" and "loop/stop" commands may not be part of a subroutine
// - if a particular subroutine encompasses another subroutine "from the left", we
//   can fold these into a single routine. e.g. if we have a routine "A B C D", we
//   can call the routine in full or call partway starting at "B" to make "B C D"

#define INSTANCE_NONE (~(size_t)0)
#define SEQUENCE_NONE (~(size_t)0)

#define CALL_SIZE 4
#define INLINE_THRESHOLD 4	// subroutines <= this size are never created

#define CALL_NEST_LIMIT (31/3)

/////////////////////// pair (all pairs in data, used when building the dictionary)

typedef struct {
	size_t left;	// left and right are stored separately as there could be a "NOP_CMD" between the pair elements
	size_t right;	// INSTANCE_NONE indicates this entry is no longer valid
} song_command_pair_instance_t;

typedef struct song_command_pair {
	song_command_t pair[2];
	unsigned freq;	// actual amount of instances
	song_command_pair_instance_t * instance_tbl;	// list of instances of this pair
	size_t instances_max;	// allocated space for instances table
	size_t instances_length;	// length of instances table, including null-entries

	struct song_command_pair * prev;	// links within main hashmap
	struct song_command_pair * next;

	struct song_command_pair * prev_freq;	// links within frequency bucket map
	struct song_command_pair * next_freq;
} song_command_pair_t;

typedef struct {
	song_command_pair_t * pair;
	size_t ii;
} song_command_pair_instance_ptr_t;

typedef struct {
	song_command_pair_t ** pairs;	// linked-list hashmap of pairs

	unsigned highest_pair_freq;
	song_command_pair_t ** freq_map;	// array of linked lists, based on the frequency of the pairs
	size_t freq_map_max;	// max freq value being tracked

	song_command_pair_instance_ptr_t * left_instance_tbl;	// table that maps source data -> pairs
	song_command_pair_instance_ptr_t * right_instance_tbl;	// table that maps source data -> pairs
} song_command_pair_list_t;

unsigned hash_song_command(song_command_t * cmd) {
	unsigned hash = 0;

	hash ^= (cmd->cmd) & 0xffff;
	hash ^= (cmd->param>>8) & 0xffff;
	hash ^= (cmd->param<<8) & 0xff00;
	hash ^= (cmd->cmd>>16) & 0xffff;
	hash ^= (cmd->param>>24) & 0xff;

	return hash;
}

unsigned hash_pair(song_command_t * left, song_command_t * right) {
	unsigned hash = hash_song_command(left);

	unsigned right_hash = hash_song_command(right);
	hash ^= (right_hash>>8) & 0xff;
	hash ^= (right_hash<<8) & 0xff00;

	return hash;
}

void remove_pair(song_command_pair_list_t * list, song_command_pair_t * p) {
	unsigned hash = hash_pair(&p->pair[0], &p->pair[1]);
	song_command_pair_t * prev = p->prev;
	song_command_pair_t * next = p->next;

	free(p->instance_tbl);
	free(p);

	if (prev) {
		prev->next = next;
	} else {
		list->pairs[hash] = next;
	}
	if (next) {
		next->prev = prev;
	}
}

void remove_pair_instance(song_command_pair_list_t * list, song_command_pair_instance_ptr_t * ip) {
	song_command_pair_t * p = ip->pair;
	if (p) {
		if (ip->ii < p->instances_length) {
			song_command_pair_instance_t * i = &p->instance_tbl[ip->ii];
			if (i->left != INSTANCE_NONE && i->right != INSTANCE_NONE) {
				// update instance table
				list->left_instance_tbl[i->left].pair = NULL;
				list->right_instance_tbl[i->right].pair = NULL;
				i->left = INSTANCE_NONE;
				i->right = INSTANCE_NONE;

				// update frequency
				song_command_pair_t * prev_freq = p->prev_freq;
				song_command_pair_t * next_freq = p->next_freq;
				p->prev_freq = NULL;
				p->next_freq = NULL;
				if (list->highest_pair_freq == p->freq && !prev_freq && !next_freq) {	// if this is the only entry for this freq and this is the max then decrease the max
					list->highest_pair_freq--;
				}
				if (prev_freq) {	// take this out of the freq list
					prev_freq->next_freq = next_freq;
				} else {
					list->freq_map[p->freq-1] = next_freq;
				}
				if (next_freq) {
					next_freq->prev_freq = prev_freq;
				}

				p->freq--;
				if (!p->freq) {
					remove_pair(list, p);
				} else {
					song_command_pair_t * fp_freq = list->freq_map[p->freq-1];	// add this pair to the new freq list
					if (fp_freq) {
						fp_freq->prev_freq = p;
						p->next_freq = fp_freq;
					}
					list->freq_map[p->freq-1] = p;
				}
			}
		}
	}
}

song_command_pair_t * add_pair_instance(song_command_pair_list_t * list, song_command_t * left, song_command_t * right, size_t left_i, size_t right_i) {
	// try finding an instance of this pair in the list already
	unsigned hash = hash_pair(left, right);
	song_command_pair_t * p = list->pairs[hash];
	while (p) {
		if (!memcmp(left, &p->pair[0], sizeof(song_command_t)) && !memcmp(right, &p->pair[1], sizeof(song_command_t))) {
			break;
		}
		p = p->next;
	}

	// if nothing was found then create a new pair, otherwise add this instance to the old pair
	if (!p) {
		p = malloc(sizeof(*p));
		memcpy(&p->pair[0], left, sizeof(song_command_t));
		memcpy(&p->pair[1], right, sizeof(song_command_t));
		p->freq = 0;
		p->prev_freq = NULL;
		p->next_freq = NULL;
		p->instance_tbl = malloc(0x10 * sizeof(*p->instance_tbl));
		p->instances_length = 0;
		p->instances_max = 0x10;

		song_command_pair_t * fp = list->pairs[hash];
		if (fp) {
			fp->prev = p;
			p->next = fp;
		} else {
			p->next = NULL;
		}
		p->prev = NULL;
		list->pairs[hash] = p;
	} else {
		if (p->instances_length == p->instances_max) {
			p->instances_max *= 2;
			p->instance_tbl = realloc(p->instance_tbl, p->instances_max * sizeof(*p->instance_tbl));
		}
	}

	// set up instance table
	p->instance_tbl[p->instances_length].left = left_i;
	p->instance_tbl[p->instances_length].right = right_i;
	list->left_instance_tbl[left_i].pair = p;
	list->right_instance_tbl[right_i].pair = p;
	list->left_instance_tbl[left_i].ii = p->instances_length;
	list->right_instance_tbl[right_i].ii = p->instances_length;
	p->instances_length++;

	// update frequency
	if (p->freq) {	// take this pair out of the old link
		song_command_pair_t * prev_freq = p->prev_freq;
		song_command_pair_t * next_freq = p->next_freq;
		p->prev_freq = NULL;
		p->next_freq = NULL;

		if (prev_freq) {
			prev_freq->next_freq = next_freq;
		} else {
			list->freq_map[p->freq-1] = next_freq;
		}
		if (next_freq) {
			next_freq->prev_freq = prev_freq;
		}
	}

	p->freq++;	// step the frequency value and assign the new highest if needed
	if (p->freq > list->highest_pair_freq) {
		list->highest_pair_freq = p->freq;
	}

	size_t old_freq_map_max = list->freq_map_max;	// if this freq is too high then expand the array
	while (list->freq_map_max < p->freq) {
		list->freq_map_max *= 2;
	}
	if (list->freq_map_max != old_freq_map_max) {
		list->freq_map = realloc(list->freq_map, list->freq_map_max * sizeof(*list->freq_map));
		memset(&list->freq_map[old_freq_map_max], 0, (list->freq_map_max-old_freq_map_max)*sizeof(*list->freq_map));
	}

	song_command_pair_t * fp_freq = list->freq_map[p->freq-1];	// add this pair to the new freq list
	if (fp_freq) {
		fp_freq->prev_freq = p;
		p->next_freq = fp_freq;
	}
	list->freq_map[p->freq-1] = p;

	return p;
}

void init_pair_list(song_command_pair_list_t * list, song_command_t * data, size_t length) {
	// initialize data structure
	list->pairs = malloc(0x10000 * sizeof(*list->pairs));
	memset(list->pairs, 0, 0x10000 * sizeof(*list->pairs));

	list->highest_pair_freq = 0;
	list->freq_map = malloc(0x100 * sizeof(*list->freq_map));
	list->freq_map_max = 0x100;
	memset(list->freq_map, 0, 0x100 * sizeof(*list->freq_map));

	list->left_instance_tbl = malloc(length * sizeof(*list->left_instance_tbl));
	list->right_instance_tbl = malloc(length * sizeof(*list->right_instance_tbl));
	memset(list->left_instance_tbl, 0, length * sizeof(*list->left_instance_tbl));
	memset(list->right_instance_tbl, 0, length * sizeof(*list->right_instance_tbl));

	// do the first pass through the input data
	for (size_t index = 0; index < length-1; index++) {
		song_command_t * this = &data[index];

		// if either this or the next pair would have an invalid control command then skip
		if (this[0].cmd == LOOP_CMD || this[1].cmd == LOOP_CMD || this[0].cmd == END_CMD || this[1].cmd == END_CMD) {
			continue;
		}

		// otherwise add this pair to the list
		add_pair_instance(list, &this[0], &this[1], index+0, index+1);
	}
}

void free_pair_list(song_command_pair_list_t * list) {
	free(list->freq_map);
	for (size_t i = 0; i < 0x10000; i++) {
		song_command_pair_t * p = list->pairs[i];
		while (p) {
			song_command_pair_t * next = p->next;
			free(p->instance_tbl);
			free(p);
			p = next;
		}
	}
	free(list->pairs);
	free(list->left_instance_tbl);
	free(list->right_instance_tbl);
}


/////////////////////// dictionary (final list of pairs)

typedef struct {
	song_command_t pair[2];
	size_t raw_size;	// size of the two commands if completely expanded
	size_t calls_from_main;	// amount of times this pair is called from main
	size_t left_refs;	// amount of times a call to this pair appears on the left side of another
	size_t left_from;	// if this entry is only referenced to the left once, this is where from
	size_t right_refs;	// amount of times a call to this pair appears on the right side of another
	size_t right_from;	// if this entry is only referenced to the right once, this is where from

	size_t sequence_id;	// sequence id this dict belongs to
	size_t sequence_index;	// index in sequence relative to left
	size_t sequence_size;	// size of the sequence starting from here
	size_t sequence_call_count;	// amount of times the sequence is called from here (NOT inlined)
} song_command_pair_dict_t;

typedef struct {
	size_t left;	// leftmost pair id in dictionary
	size_t right;	// rightmost pair id in dictionary
	size_t length;	// length of the sequence in dictionary entries (command length is this + 1)
	size_t minimum_eligible_index;	// first index of the sequence that can be turned into a subroutine
	size_t call_level;	// how many nested subroutines deep calling this sequence would result in
} song_command_pair_dict_sequence_t;


//////////////////////// main compression procedure

size_t first_label_id = 0;	// first label id that can be used by subroutines

song_command_sequence_t compressed_song_data;

int write_compressed_call_command(song_command_sequence_t * song, song_command_pair_dict_t * dict_tbl, song_command_pair_dict_sequence_t * seq_tbl, size_t di, size_t * size_ptr, int is_end);	// forward-declare

int write_compressed_song_command(song_command_sequence_t * song, song_command_pair_dict_t * dict_tbl, song_command_pair_dict_sequence_t * seq_tbl, unsigned cmd, unsigned param, size_t * size_ptr, int is_end) {
	if (cmd == CALL_CMD) {
		return write_compressed_call_command(song, dict_tbl, seq_tbl, param, size_ptr, is_end);
	} else if (cmd < 0x100) {
		write_song_command(song, cmd, param);
		*size_ptr += command_size_tbl[cmd];
	}
	return 0;
}

int write_compressed_call_command(song_command_sequence_t * song, song_command_pair_dict_t * dict_tbl, song_command_pair_dict_sequence_t * seq_tbl, size_t di, size_t * size_ptr, int is_end) {
	song_command_pair_dict_t * d = &dict_tbl[di];
	song_command_pair_dict_sequence_t * s = &seq_tbl[d->sequence_id];

	if (s->minimum_eligible_index == SEQUENCE_NONE || s->call_level >= CALL_NEST_LIMIT || d->sequence_size <= INLINE_THRESHOLD) {
		// sequence is not subroutine-eligible at all, just inline it
		for (size_t i = d->sequence_index; i < s->length; i++) {
			write_compressed_song_command(song, dict_tbl, seq_tbl, d->pair[0].cmd, d->pair[0].param, size_ptr, 0);
			if (i < s->length-1) {
				d = &dict_tbl[d->pair[1].param];
			}
		}
		write_compressed_song_command(song, dict_tbl, seq_tbl, d->pair[1].cmd, d->pair[1].param, size_ptr, 0);
		return 0;
	} else {
		// subroutine is eligible, inline until we hit the eligible index then emit a call
		for (size_t i = d->sequence_index; i < s->minimum_eligible_index; i++) {
			write_compressed_song_command(song, dict_tbl, seq_tbl, d->pair[0].cmd, d->pair[0].param, size_ptr, 0);
			di = d->pair[1].param;
			d = &dict_tbl[di];
		}
		write_compressed_song_command(song, dict_tbl, seq_tbl, is_end ? 0xfd : 0xfe, di + first_label_id, size_ptr, is_end);
		return 1;
	}
}

void compress_song_data(int max_iterations, unsigned pair_freq_threshold) {
	clock_t start_time = clock();
	printf("Compressing song data...");
	fflush(stdout);

	///////////////////////////////////////////////// PHASE 1: do re-pair algorithm
	// build the complete song datastream
	size_t all_song_data_length = 0;
	for (size_t i = 0; i < songs; i++) {
		song_t * s = &song_tbl[i];
		for (int j = 0; j < CHANNELS; j++) {
			if (song_channel_is_used(s, j)) {
				all_song_data_length += s->sequences[j].length;
				s->sequence_label_ids[j] = first_label_id++;
				if (s->has_loop) {
					s->sequence_loop_label_ids[j] = first_label_id++;
				}
			}
		}
	}
	song_command_t * all_song_data = malloc(all_song_data_length * sizeof(song_command_t));
	size_t all_song_data_index = 0;
	for (size_t i = 0; i < songs; i++) {
		song_t * s = &song_tbl[i];
		for (int j = 0; j < CHANNELS; j++) {
			if (song_channel_is_used(s, j)) {
				song_command_sequence_t * seq = &s->sequences[j];
				memcpy(&all_song_data[all_song_data_index], seq->data, seq->length*sizeof(song_command_t));
				all_song_data_index += seq->length;
			}
		}
	}

	// run through the song data and find initial set of pairs
	printf("initializing...");
	fflush(stdout);

	song_command_pair_list_t pair_list;
	init_pair_list(&pair_list, all_song_data, all_song_data_length);

	// begin main iteration loop of finding most common pair and replacing sequences
	int iter = 0;
	song_command_pair_dict_t * dict_tbl = malloc(0x10000 * sizeof(*dict_tbl));
	size_t dict_max = 0x10000;
	size_t dict_length = 0;
	int fib[3] = {0,1,1};
	while ((max_iterations < 0 || iter < max_iterations) && pair_list.highest_pair_freq >= pair_freq_threshold) {
		iter++;
		if (iter >= fib[2]) {	// iterations get faster so we don't want to print on every one
			printf("%d...", iter);
			fflush(stdout);

			fib[0] = fib[1];
			fib[1] = fib[2];
			fib[2] = fib[0] + fib[1];
		}

		song_command_pair_t * p = pair_list.freq_map[pair_list.highest_pair_freq-1];

		// add the replaced pair to the dictionary
		if (dict_length == dict_max) {
			dict_max *= 2;
			dict_tbl = realloc(dict_tbl, dict_max * sizeof(*dict_tbl));
		}
		size_t di = dict_length++;
		song_command_pair_dict_t * this_dict = &dict_tbl[di];
		memset(this_dict, 0, sizeof(*this_dict));
		memcpy(this_dict->pair, p->pair, sizeof(song_command_t[2]));
		for (int i = 0; i < 2; i++) {
			unsigned cmd = this_dict->pair[i].cmd;
			unsigned param = this_dict->pair[i].param;
			if (cmd == CALL_CMD) {
				song_command_pair_dict_t * ref_dict = &dict_tbl[param];
				this_dict->raw_size += ref_dict->raw_size;
				if (i) {
					ref_dict->right_refs++;
					ref_dict->right_from = di;
				} else {
					ref_dict->left_refs++;
					ref_dict->left_from = di;
				}
			} else {
				this_dict->raw_size += command_size_tbl[cmd];
			}
		}
		this_dict->sequence_id = SEQUENCE_NONE;

		// replace all instances of the pair with a single value and recalculate the pairs
		unsigned instances_left = p->freq;	// keep a separate counter, as the pointer is deleted when all instances are removed
		for (size_t ii = 0; instances_left && ii < p->instances_length; ii++) {
			song_command_pair_instance_t * i = &p->instance_tbl[ii];
			size_t left = i->left;
			size_t right = i->right;
			if (left != INSTANCE_NONE && right != INSTANCE_NONE) {
				song_command_pair_instance_ptr_t * ip = &pair_list.left_instance_tbl[left];

				all_song_data[left].cmd = CALL_CMD;
				all_song_data[left].param = di;
				all_song_data[right].cmd = NOP_CMD;
				all_song_data[right].param = 0;

				// find the new right part of the pair and invalidate any pairs now containing a NOP_CMD
				size_t new_right = right;
				while (new_right < all_song_data_length && all_song_data[new_right].cmd == NOP_CMD) {
					remove_pair_instance(&pair_list, &pair_list.left_instance_tbl[new_right++]);
				}
				if (new_right >= all_song_data_length || all_song_data[new_right].cmd == LOOP_CMD || all_song_data[new_right].cmd == END_CMD)
					new_right = INSTANCE_NONE;

				// update the pair relying on the left side we just replaced, if any
				song_command_pair_instance_ptr_t * left_inst_ptr = &pair_list.right_instance_tbl[left];
				if (left_inst_ptr->pair) {
					song_command_pair_t * left_pair = left_inst_ptr->pair;
					song_command_pair_instance_t * left_pair_i = &left_pair->instance_tbl[left_inst_ptr->ii];
					size_t left_pair_left = left_pair_i->left;
					remove_pair_instance(&pair_list, left_inst_ptr);
					add_pair_instance(&pair_list, &all_song_data[left_pair_left], &all_song_data[left], left_pair_left, left);
				}

				// add the new pair for this replacement
				remove_pair_instance(&pair_list, ip);
				if (new_right != INSTANCE_NONE) {
					add_pair_instance(&pair_list, &all_song_data[left], &all_song_data[new_right], left, new_right);
				}

				instances_left--;
			}
		}

		dict_length++;
	}
	if (pair_list.highest_pair_freq >= pair_freq_threshold) {
		printf("hit %d-iteration limit.\n", iter);
	} else {
		printf("completed in %d iterations.\n", iter);
	}
	free_pair_list(&pair_list);


	//////////////////////////////////////////////////// PHASE 2: using the dictionary find candidates
	printf("Finding subroutine candidates...");
	fflush(stdout);

	// collect some stats on the dictionary
	for (size_t i = 0; i < all_song_data_length; i++) {
		if (all_song_data[i].cmd == CALL_CMD) {
			dict_tbl[all_song_data[i].param].calls_from_main++;
		}
	}

	// extract linear sequences from the pair dictionary
	// these will form the basis of our subroutines
	song_command_pair_dict_sequence_t * seq_tbl = malloc(dict_length * sizeof(*seq_tbl));
	memset(seq_tbl, 0, dict_length * sizeof(*seq_tbl));
	size_t seqs = 0;
	size_t * seq_chain = malloc(0x40 * sizeof(*seq_chain));
	size_t seq_chain_max = 0x40;
	for (size_t di = 0; di < dict_length; di++) {
		song_command_pair_dict_t * d = &dict_tbl[di];
		if (d->sequence_id == SEQUENCE_NONE) {
			size_t si = seqs++;
			song_command_pair_dict_sequence_t * s = &seq_tbl[si];

			// move left through the dictionary to find the leftmost element of this sequence
			size_t left = di;
			size_t length = 1;
			size_t left_length = 1;
			seq_chain[0] = di;
			song_command_pair_dict_t * dl = d;
			while (
				!dl->left_refs	// if we are ever referenced from the left then this is the leftmost element
				&& dl->right_refs >= 1	// must have been referenced from the right at least once, else this is the leftmost
				&& (dl->right_refs == 1 || dict_tbl[dl->right_from].sequence_id == SEQUENCE_NONE)	// either only referenced from the right once, or this is the "canonical" right-reference
			) {
				if (left_length == seq_chain_max) {
					seq_chain_max *= 2;
					seq_chain = realloc(seq_chain, seq_chain_max*sizeof(*seq_chain));
				}

				left = dl->right_from;
				dl = &dict_tbl[left];
				length++;
				seq_chain[left_length++] = left;
			}

			// move right to find the rightmost element
			size_t right = di;
			song_command_pair_dict_t * dr = d;
			while (dr->pair[1].cmd == CALL_CMD) {
				size_t next = dr->pair[1].param;
				dr = &dict_tbl[next];
				if (
					dr->left_refs	// if next is ever referenced from the left, then this is the end of the sequence
					|| dr->right_from != right	// if we're not the "canonical" right-reference then this is the end
				) {
					break;
				}

				right = next;

				dr->sequence_id = si;
				dr->sequence_index = length++;
			}

			// update sequence information for the left side
			size_t li = left_length;
			do {
				li--;
				song_command_pair_dict_t * dl = &dict_tbl[seq_chain[li]];
				dl->sequence_id = si;
				dl->sequence_index = (left_length-1) - li;
			} while (li > 0);

			// update sequence information
			s->left = left;
			s->right = right;
			s->length = length;
		}
	}
	free(seq_chain);
	seq_chain = NULL;
	printf("%zu linear sequences...", seqs);
	fflush(stdout);

	// for each sequence go through and fill in the sizes and call count
	for (size_t si = 0; si < seqs; si++) {
		song_command_pair_dict_sequence_t * s = &seq_tbl[si];
		size_t di = s->right;
		size_t raw_size = 0;
		while (1) {
			song_command_pair_dict_t * d = &dict_tbl[di];
			d->sequence_call_count = d->calls_from_main;
			for (int i = 0; i < (di == s->right ? 2 : 1); i++) {
				unsigned cmd = d->pair[i].cmd;
				if (cmd == CALL_CMD) {
					song_command_pair_dict_t * dref = &dict_tbl[d->pair[i].param];
					if (dref->raw_size <= INLINE_THRESHOLD) {
						raw_size += dref->raw_size;
					} else {
						raw_size += CALL_SIZE;
						dref->sequence_call_count++;
					}
				} else {
					raw_size += command_size_tbl[cmd];
				}
				d->sequence_size = raw_size;
			}

			if (di == s->left) {
				break;
			}
			di = d->right_from;
		}
	}

	// ok, we now have enough to start building our subroutines.
	// we only deal with "eligible" linear sequences (call count > 1, and size over the inline threshold)
	size_t eligible_subroutines = 0;
	for (size_t si = 0; si < seqs; si++) {
		song_command_pair_dict_sequence_t * s = &seq_tbl[si];
		s->minimum_eligible_index = SEQUENCE_NONE;
		size_t di = s->left;
		size_t i = 0;
		while (1) {
			song_command_pair_dict_t * d = &dict_tbl[di];
			if (d->sequence_call_count > 1 && d->sequence_size > INLINE_THRESHOLD) {
				s->minimum_eligible_index = i;
				s->call_level = SEQUENCE_NONE;
				eligible_subroutines++;
				break;
			}
			if (di == s->right) {
				break;
			}
			di = d->pair[1].param;
			i++;
		}
	}
	printf("%zu eligible subroutines...", eligible_subroutines);
	fflush(stdout);

	// go through the eligible subroutines and set their "call level"
	// subroutines with call levels higher than the nest limit are not eligible
	size_t unknown_call_levels = eligible_subroutines;
	for (int iter = 0; iter <= CALL_NEST_LIMIT && unknown_call_levels; iter++) {
		for (size_t si = 0; si < seqs; si++) {
			song_command_pair_dict_sequence_t * s = &seq_tbl[si];
			if (s->minimum_eligible_index != SEQUENCE_NONE && s->call_level == SEQUENCE_NONE) {
				size_t di = s->left;
				size_t i = 0;
				while (i < s->minimum_eligible_index) {
					di = dict_tbl[di].pair[1].param;
					i++;
				}

				int calls = 0;
				size_t max_call_level = 0;
				while (1) {
					song_command_pair_dict_t * d = &dict_tbl[di];
					for (int i = 0; i < (di==s->right ? 2 : 1); i++) {
						if (d->pair[i].cmd == CALL_CMD) {
							song_command_pair_dict_t * dref = &dict_tbl[d->pair[i].param];
							song_command_pair_dict_sequence_t * sref = &seq_tbl[dref->sequence_id];
							if (sref->minimum_eligible_index != SEQUENCE_NONE && dref->sequence_index >= sref->minimum_eligible_index) {
								if (sref->call_level == SEQUENCE_NONE)
									goto sub_call_level_unknown;

								calls++;
								if (sref->call_level > max_call_level) {
									max_call_level = sref->call_level;
								}
							}
						}
					}

					if (di == s->right)
						break;
					di = d->pair[1].param;
				}

				s->call_level = calls ? max_call_level + 1 : 0;
				if (s->call_level >= CALL_NEST_LIMIT) {
					eligible_subroutines--;
				}
				unknown_call_levels--;
			}
sub_call_level_unknown:;
		}
	}
	if (unknown_call_levels) {
		for (size_t si = 0; si < seqs; si++) {
			song_command_pair_dict_sequence_t * s = &seq_tbl[si];
			if (s->call_level == SEQUENCE_NONE) {
				eligible_subroutines--;
			}
		}
	}

	printf("%zu after nest limit filter.\n", eligible_subroutines);
	fflush(stdout);


	///////////////////////////////////////////// PHASE 3: build the final compressed data
	printf("Creating final compressed song data...");

	compressed_song_data.data = malloc(all_song_data_length * sizeof(*compressed_song_data.data));
	compressed_song_data.length = 0;
	compressed_song_data.max = all_song_data_length;
	size_t compressed_song_data_size = 0;

	//// write main song data
	int song_id = 0;
	int chn_id = -1;
	int write_song_label = 1;
	for (size_t i = 0; i < all_song_data_length; i++) {
		if (write_song_label) {
			do {
				chn_id++;
				if (chn_id == CHANNELS) {
					chn_id = 0;
					song_id++;
				}
			} while (!song_channel_is_used(&song_tbl[song_id], chn_id));
			size_t song_label = song_tbl[song_id].sequence_label_ids[chn_id];
			write_song_command(&compressed_song_data, LAB_CMD, song_label);
			write_song_label = 0;
		}

		unsigned cmd = all_song_data[i].cmd;
		unsigned param = all_song_data[i].param;
		if (cmd == END_CMD) {
			size_t loop_label = song_tbl[song_id].sequence_loop_label_ids[chn_id];
			if (loop_label == SEQUENCE_NONE) {
				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, 0xff, 0, &compressed_song_data_size, 0);
			} else {
				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, 0xfd, loop_label, &compressed_song_data_size, 0);
			}

			write_song_label = 1;
		} else if (cmd == LOOP_CMD) {
			size_t loop_label = song_tbl[song_id].sequence_loop_label_ids[chn_id];
			write_song_command(&compressed_song_data, LAB_CMD, loop_label);
		} else {
			write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, cmd, param, &compressed_song_data_size, 0);
		}
	}

	//// write subroutines
	for (size_t si = 0; si < seqs; si++) {
		song_command_pair_dict_sequence_t * s = &seq_tbl[si];
		size_t di = s->left;
		if (s->minimum_eligible_index != SEQUENCE_NONE && s->call_level < CALL_NEST_LIMIT) {
			// step until we hit the minimum eligible index
			for (size_t i = 0; i < s->minimum_eligible_index; i++) {
				di = dict_tbl[di].pair[1].param;
			}

			// write subroutine body
			song_command_pair_dict_t * d = &dict_tbl[di];
			while (1) {
				if (d->sequence_call_count > 0) {
					write_song_command(&compressed_song_data, LAB_CMD, di + first_label_id);
				}

				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, d->pair[0].cmd, d->pair[0].param, &compressed_song_data_size, 0);

				if (di == s->right)
					break;
				di = d->pair[1].param;
				d = &dict_tbl[di];
			}
			if (!write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, d->pair[1].cmd, d->pair[1].param, &compressed_song_data_size, 1))
				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, 0xff, 0, &compressed_song_data_size, 0);
		}
	}

	////////// done, clean up
	clock_t end_time = clock();
	printf("Compressed %zu bytes of song data to %zu bytes in %.2f seconds. (%.2f%%)\n", songs_size, compressed_song_data_size, (double)(end_time-start_time)/CLOCKS_PER_SEC, (compressed_song_data_size*100.0) / songs_size);
	songs_size = compressed_song_data_size;

	free(all_song_data);
	free(dict_tbl);
	free(seq_tbl);
}








/****************** main function ***********************/

int main(int argc, char *argv[])
{
	if (argc < 3)
	{
		printf(
			"usage: knl-vgm-convert [option]... vgms...\n"
			"options:\n"
			"\t-o filename\n"
			"\t--base-interval n        (default 735)\n"
			"\t--max-iterations n       (default 200000)\n"
			"\t--pair-freq-threshold n  (default 2)\n"
		);
		return EXIT_FAILURE;
	}
	const char * out_name = NULL;
	int base_interval = -1;
	int max_iterations = 200000;
	int pair_freq_threshold = 2;
	
	int argi = 1;
	while (argi < argc) {
		const char * arg = argv[argi];
		const char * param = (argi+1) < argc ? argv[argi+1] : NULL;
		long param_int = param ? strtol(param, NULL, 0) : -1;
		if (!strcmp(arg, "--")) {	// stop arg parsing
			argi++;
			break;
		} else if (!strcmp(arg, "-o")) {
			out_name = param ? param : out_name;
			argi += 2;
		} else if (!strcmp(arg, "--base-interval")) {
			base_interval = param ? param_int : base_interval;
			argi += 2;
		} else if (!strcmp(arg, "--max-iterations")) {
			max_iterations = param ? param_int : max_iterations;
			argi += 2;
		} else if (!strcmp(arg, "--pair-freq-threshold")) {
			pair_freq_threshold = param ? param_int : pair_freq_threshold;
			argi += 2;
		} else if (arg[0] == '-') {
			printf("WARNING: Unrecognized command line option \"%s\".", arg);
			argi++;
		} else {
			break;
		}
	}

	if (!out_name) {
		puts("No output filename defined.");
		return EXIT_FAILURE;
	}
	if (base_interval <= 0) {
		base_interval = 735;
		puts("WARNING: Base interval is not defined, defaulting to 735.");
	}
	if (max_iterations <= 0) {
		max_iterations = -1;
	}
	if (pair_freq_threshold < 2) {
		pair_freq_threshold = 2;
	}

	if (argi >= argc) {
		puts("No input filenames defined.");
		return EXIT_FAILURE;
	}
	for (int i = argi; i < argc; i++)
	{
		load_vgm(argv[i], base_interval);
		for (int i = 0; i < 75; i++) putchar('-');
		putchar('\n');
	}

	compress_song_data(max_iterations, pair_freq_threshold);
	
	
	/********* report ********************/
	printf("\n%zu songs, %zu samples.\n",songs,samples);
	if (!songs)
	{
		puts("No valid songs, quitting.");
		return EXIT_FAILURE;
	}
	
	size_t total_size = songs_size+samples_size;
	printf("Song data: %zu bytes, samples: %zu bytes.\n"
		,songs_size,samples_size);
	printf("Total size: %zu bytes (%.1f KiB).\n", total_size,total_size/1024.0);
	
	int toomany = 0;
	if (songs > MAX_SONGS)
	{
		printf("Too many songs! (max %u)\n",MAX_SONGS);
		toomany++;
	}
	if (samples > MAX_SAMPLES)
	{
		printf("Too many samples! (max %u)\n",MAX_SAMPLES);
		toomany++;
	}
	if (toomany) return EXIT_FAILURE;
	
	
	
	/******************************** OUTPUT FILE ***********************************/
	FILE *f = fopen(out_name,"w");
	if (!f)
	{
		printf("Couldn't open %s: %s\n",out_name,strerror(errno));
		return EXIT_FAILURE;
	}
	
	time_t ttttt = time(NULL);
	fprintf(f,";\n; Generated on %s;\n\n", ctime(&ttttt));
	
	
	
	/************** song data streams ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; Song data streams.\n align 1\nknl_song_tbl:\n");
	for (size_t i = 0; i < songs; i++)
	{
		const song_t * s = &song_tbl[i];
		fprintf(f,"\tdl ");
		for (int j = 0; j < CHANNELS; j++) {
			size_t label = s->sequence_label_ids[j];
			if (label == SEQUENCE_NONE) {
				fputc('0',f);
			} else {
				fprintf(f,"knl_lab_%zu",label);
			}
			if (j < 9) {
				fputc(',', f);
			}
		}
		fputc('\n', f);
	}
	
	/***** actual data *****/
	for (size_t i = 0; i < compressed_song_data.length; i++) {
		const song_command_t * sc = &compressed_song_data.data[i];
		if (sc->cmd == LAB_CMD) {
			fprintf(f,"\nknl_lab_%u:",sc->param);
		} else if (sc->cmd == 0xfd) {
			fprintf(f,"\n\tdl ($fd<<24)+knl_lab_%u",sc->param);
		} else if (sc->cmd == 0xfe) {
			fprintf(f,"\n\tdl ($fe<<24)+knl_lab_%u",sc->param);
		} else if (sc->cmd < 0x100) {
			unsigned command_size = command_size_tbl[sc->cmd];
			if (!command_size) {
				printf("Illegal command %02X found at %zu\n", sc->cmd, i);
				return EXIT_FAILURE;
			}
			fprintf(f,"\n\tdb $%02X",sc->cmd);
			for (unsigned b = 1; b < command_size; b++) {
				fprintf(f,",$%02X",(sc->param>>(8*(b-1)))&0xff);
			}
		}
	}
	
	
	
	
	/************** samples ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; Samples.\n align 1\nknl_sample_tbl:");
	if (samples)
	{
		fprintf(f," dl ");
		
		for (size_t i = 0; i < samples; i++)
		{
			fprintf(f,"knl_sample_%zu",i);
			if (i < samples-1) fputc(',',f);
		}
		
		/***** actual data *****/
		for (size_t i = 0; i < samples; i++)
		{
			sample_t *s = &sample_tbl[i];
			fprintf(f,"\nknl_sample_%zu: db ",i);
			
			for (size_t j = 0; j < s->size; j++)
			{
				unsigned data = s->data[j];
				if (!data) data = 1; /* we use 0 for end marker */
				fprintf(f,"$%02X,",data);
			}
			
			/* end marker */
			fputc(' ',f);
			for (int j = 0; j < 0x100; j++)
			{
				fputc('0',f);
				if (j < 0xff) fputc(',',f);
			}
		}
	}
	
	
	
	fclose(f);
	
	
	
}



