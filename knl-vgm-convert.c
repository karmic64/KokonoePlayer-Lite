
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

data stream format:

CONTROL COMMANDS:
	$67 $yyyyyy - jump
	$68 $yyyyyy - call
	$69 - set loop point
	$6a - if in a call return, else loop if there is a loop, otherwise stop
	$6b $yyyy - wait for $yyyy 44100hz intervals
	$c0-$df - wait for $xx 60hz intervals
	$e0-$ff - wait for $xx 50hz intervals

PSG COMMANDS:
	$6c-$6f $xx - psg low frequency to $xx (or noise mode/pitch on PSG)
	$70-$7f - psg high frequency (bits 2-3 are channel, bits 0-1 are the actual bits (or noise pitch on PSG)
	$80-$bf - psg channel volume (channel in upper nybble, volume in lower nybble)

FM COMMANDS:
	$00-$5f - fm channel control (upper nybble is channel id)
		$x0 - keyoff all operators
		$x1 - keyon all operators
		$x2 $yy - set keyon state to $yy
		$x3 $yy - set FM patch to $yy
		$x4 $yyyy - set FM patch to $yyyy
		$x5 $yy - set low frequency to $yy
		$x6 $yy - set high frequency to $yy
		$x7 $yyyy - set frequency to $yyyy
	$00-$2f - chn3 operator control (upper nybble is operator id - 1)
		$x8 $yy - set low frequency to $yy
		$x9 $yy - set high frequency to $yy
		$xa $yyyy - set frequency to $yyyy
	$60 $yy - set chn.3 mode (0=normal, 1=extd)
	$61 $yy - toggle DAC
	$62 $yy - set LFO to $yy
	$63 $yyyy - set sample output frequency to $yyyy
	$64 $yy - start sample $yy
	$65 $yy - start looped sample $yy
	$66 - stop sample

*/

const unsigned char command_size_tbl[0x100] = {
	// 00-5f fm control
	1,1,2,2,3,2,2,3,2,2,3,0,0,0,0,0,
	1,1,2,2,3,2,2,3,2,2,3,0,0,0,0,0,
	1,1,2,2,3,2,2,3,2,2,3,0,0,0,0,0,
	1,1,2,2,3,2,2,3,0,0,0,0,0,0,0,0,
	1,1,2,2,3,2,2,3,0,0,0,0,0,0,0,0,
	1,1,2,2,3,2,2,3,0,0,0,0,0,0,0,0,
	// 60-6f control and psg low-freq
	2,2,2,3,2,2,1,4,4,1,1,3,2,2,2,2,
	// 70-bf psg main
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	// c0-ff short waits
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
};



#define min(a,b) (((a) < (b)) ? (a) : (b))

/* see vgm spec */
#define NTSC_INTERVAL 735
#define PAL_INTERVAL 882

/* for NTSC, but PAL isn't different enough to worry about it */
#define Z80_CLOCK 3579545.0
#define Z80_CLOCKS_PER_SAMPLE 104.0



/* indexed_read_68k_ptr accepts a 16-bit id, and shifts out the upper 2 bits */
#define MAX_SONGS 0x4000
#define MAX_FM_PATCHES 0x4000

/* k_sample_init_id is 1 byte long (this could be trivially fixed if a need ever arises */
#define MAX_SAMPLES 0x100




unsigned get16(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8); }
unsigned get24(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16); }
unsigned get32(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16) | (*(p+3)<<24); }



/*************** structs ****************/

/////// fm patch

typedef struct {
	uint8_t reg30[4];
	uint8_t reg40[4];
	uint8_t reg50[4];
	uint8_t reg60[4];
	uint8_t reg70[4];
	uint8_t reg80[4];
	uint8_t reg90[4];
	uint8_t regb0;
	uint8_t regb4;
}	fm_patch_t;
void init_fm_patch(fm_patch_t *p)
{
	memset(p,0,sizeof(*p));
	/* set up panning */
	p->regb4 = 0xc0;
}
int fm_patch_cmp(fm_patch_t *a, fm_patch_t *b)
{
	if (memcmp(a->reg30,b->reg30,4)) return 1;
	if (memcmp(a->reg40,b->reg40,4)) return 1;
	if (memcmp(a->reg50,b->reg50,4)) return 1;
	if (memcmp(a->reg60,b->reg60,4)) return 1;
	if (memcmp(a->reg70,b->reg70,4)) return 1;
	if (memcmp(a->reg80,b->reg80,4)) return 1;
	if (memcmp(a->reg90,b->reg90,4)) return 1;
	if (a->regb0 != b->regb0) return 1;
	if (a->regb4 != b->regb4) return 1;
	return 0;
}

unsigned add_fm_patch(fm_patch_t *);	// forward-declare add function


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



/////////// song

typedef struct {	// this struct should always be memset before use to allow safe memcmp'ing
	unsigned cmd;
	unsigned param;
} song_command_t;
typedef struct {
	song_command_t * data;
	size_t length;
	size_t max;	// current allocated length of data array
} song_command_sequence_t;
typedef song_command_sequence_t song_t;
int song_cmp(song_t *a, song_t *b)
{
	if (a->length != b->length) return 1;
	return memcmp(a->data, b->data, a->length * sizeof(song_command_t));
}
void write_song_command(song_t * song, unsigned cmd, unsigned param) {
	if (song->length == song->max) {
		size_t old_song_max = song->max;
		if (!song->max) song->max = 0x20000;
		else song->max *= 2;
		song->data = realloc(song->data,song->max*sizeof(*song->data));
		memset(&song->data[old_song_max], 0, (song->max-old_song_max)*sizeof(*song->data));
	}
	song_command_t * sc = &song->data[song->length++];
	sc->cmd = cmd;
	sc->param = param;
}

unsigned add_song(song_t *);	// forward-declare add function



/////// playback state

typedef struct {
	fm_patch_t patch[6];
	uint16_t freq[6];
	uint16_t extd_freq[3];
	uint8_t keyon[6];
	
	uint8_t chn3_mode;
	uint8_t dac_enable;
	uint8_t lfo;
} fm_state_t;
typedef struct {
	uint16_t freq[4];
	uint8_t volume[4];
} psg_state_t;
typedef struct {
	fm_state_t fm;
	psg_state_t psg;
	
	uint16_t sample_freq;
} playback_state_t;
void init_playback_state(playback_state_t *p)
{
	memset(p, 0, sizeof(*p));
	for (int i = 0; i < 6; i++)
		init_fm_patch(&p->fm.patch[i]);
}


/* flushes fm patches if any of them changed */
void flush_fm_patches(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	for (int chn = 0; chn < 6; chn++)
	{
		fm_patch_t *old = &prv->fm.patch[chn];
		fm_patch_t *new = &state->fm.patch[chn];
		if (!fm_patch_cmp(old,new)) continue;

		unsigned new_id = add_fm_patch(new);
		memcpy(old, new, sizeof(*new));

		write_song_command(song, 3 + (chn << 4) + (new_id < 0x100 ? 0 : 1), new_id);
	}
}

/* flushes fm freqs if any of them changed */
void flush_fm_freqs(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	/******** normal fm channels *********/
	for (int chn = 0; chn < 6; chn++)
	{
		uint16_t *old = &prv->fm.freq[chn];
		uint16_t *new = &state->fm.freq[chn];

		int lodiff = ((*old)&0xff) != ((*new)&0xff);
		int hidiff = ((*old)>>8) != ((*new)>>8);

		if (lodiff && hidiff)
		{
			write_song_command(song, (chn<<4) + 7, *new);
		}
		else if (lodiff)
		{
			write_song_command(song, (chn<<4) + 5, (*new)&0xff);
		}
		else if (hidiff)
		{
			write_song_command(song, (chn<<4) + 6, (*new)>>8);
		}

		*old = *new;
	}

	/********* ext.chn3 frequencies *************/
	for (int op = 0; op < 3; op++)
	{
		uint16_t *old = &prv->fm.extd_freq[op];
		uint16_t *new = &state->fm.extd_freq[op];

		int lodiff = ((*old)&0xff) != ((*new)&0xff);
		int hidiff = ((*old)>>8) != ((*new)>>8);

		if (lodiff && hidiff)
		{
			write_song_command(song, (op<<4) + 10, *new);
		}
		else if (lodiff)
		{
			write_song_command(song, (op<<4) + 8, (*new)&0xff);
		}
		else if (hidiff)
		{
			write_song_command(song, (op<<4) + 9, (*new)>>8);
		}

		*old = *new;
	}
}

/* flushes FM patches, freqs, and keyon state */
void flush_fm_keyon_state(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	flush_fm_patches(song, prv, state);
	flush_fm_freqs(song, prv, state);

	for (int chn = 0; chn < 6; chn++)
	{
		uint8_t *old = &prv->fm.keyon[chn];
		uint8_t *new = &state->fm.keyon[chn];

		if (*old != *new)
		{
			*old = *new;
			if (*new == 0x00)
				write_song_command(song, (chn<<4) + 0, 0);
			else if (*new == 0xf0)
				write_song_command(song, (chn<<4) + 1, 0);
			else
			{
				write_song_command(song, (chn<<4) + 2, *new);
			}
		}
	}
}

/* fully flush fm state */
void flush_fm_state(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	flush_fm_keyon_state(song, prv, state);

	if (prv->fm.chn3_mode != state->fm.chn3_mode)
	{
		prv->fm.chn3_mode = state->fm.chn3_mode;

		write_song_command(song, 0x60, (state->fm.chn3_mode ? 1 : 0));
	}

	if (prv->fm.dac_enable != state->fm.dac_enable)
	{
		prv->fm.dac_enable = state->fm.dac_enable;

		write_song_command(song, 0x61, (state->fm.dac_enable ? 1 : 0));
	}

	if (prv->fm.lfo != state->fm.lfo)
	{
		prv->fm.lfo = state->fm.lfo;

		write_song_command(song, 0x62, state->fm.lfo);
	}
}

/* fully flush psg state */
void flush_psg_state(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	for (int chn = 0; chn < 4; chn++)
	{
		/* volume */
		uint8_t *v = &state->psg.volume[chn];
		uint8_t *pv = &prv->psg.volume[chn];
		if (*v != *pv)
		{
			*pv = *v;
			write_song_command(song, 0x80 + (chn<<4) + *v, 0);
		}

		/* freq */
		uint16_t *f = &state->psg.freq[chn];
		uint8_t fl = (*f) & 0xff;
		uint8_t fh = (*f) >> 8;
		uint16_t *pf = &prv->psg.freq[chn];
		uint8_t pfl = (*pf) & 0xff;
		uint8_t pfh = (*pf) >> 8;
		if (chn != 3)
		{
			if (*pf == (uint16_t)-1 || fl != pfl)
			{
				write_song_command(song, 0x6c + chn, fl);
			}
			if (*pf == (uint16_t)-1 || fh != pfh)
			{
				write_song_command(song, 0x70 + (chn<<2) + fh, 0);
			}
		}
		else
		{
			if (((*pf)&4) != ((*f)&4))
			{ /* noise mode change */
				write_song_command(song, 0x6f, fl);
			}
			else if (((*pf)&3) != ((*f)&3))
			{ /* only noise pitch change */
				write_song_command(song, 0x70 + (chn<<2) + (fl&3), 0);
			}
		}
		*pf = *f;
	}
}

/* fully flush all state (used on wait commands/loop/end) */
void flush_playback_state(song_t * song, playback_state_t * prv, playback_state_t * state)
{
	flush_fm_state(song, prv, state);
	flush_psg_state(song, prv, state);

	if (prv->sample_freq != state->sample_freq)
	{
		prv->sample_freq = state->sample_freq;

		write_song_command(song, 0x63, state->sample_freq);
	}
}




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

fm_patch_t *fm_patch_tbl = NULL;
size_t fm_patches = 0;
size_t fm_patches_max = 0;

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

MAKE_ADD_FUNC(add_fm_patch, fm_patch_t, fm_patch_tbl, fm_patches, fm_patches_max, 0x100, fm_patch_cmp)
MAKE_ADD_FUNC(add_sample, sample_t, sample_tbl, samples, samples_max, 0x20, sample_cmp)
MAKE_ADD_FUNC(add_song, song_t, song_tbl, songs, songs_max, 0x20, song_cmp)

#undef MAKE_ADD_FUNC




/******************** misc funcs ********************/

/* any register involved in the fm_patch_t */
int is_fm_patch_register(unsigned part, unsigned r)
{
	/* alg/pan registers */
	if (r >= 0xb0 && r <= 0xb2) return 1;
	if (r >= 0xb4 && r <= 0xb6) return 1;
	
	/* outside of per-op regs? */
	if (r < 0x30) return 0;
	if (r >= 0xa0) return 0;
	
	/* channel 3 in a part does not exist */
	return (r & 3) != 3;
}

/* if a register is not valid, writes will be ignored */
int is_valid_fm_register(unsigned part, unsigned r)
{
	if (is_fm_patch_register(part,r)) return 1;
	
	/* frequencies */
	if (r >= 0xa0 && r < 0xb0)
		return (r & 3) != 3;
	
	/* following registers only exist in part 1 */
	if (part) return 0;
	if (r == 0x22) return 1; /* LFO */
	if (r == 0x27) return 1; /* extd. chn.3 */
	if (r == 0x28) return 1; /* keyon */
	if (r == 0x2b) return 1; /* DAC enable */
	
	return 0;
}








/************************* main load_vgm function *************************/

int load_vgm(char * vgm_filename)
{
	int return_val = 1;
	
	
	/********** save old data counts for fail cleanup *************/
	size_t old_fm_patches = fm_patches;
	size_t old_samples = samples;
	size_t old_songs = songs;
	size_t old_samples_size = samples_size;
	
	
	/******** init variables **********/
	struct stat vgm_stat;
	uint8_t *vgm = NULL;
	
	song_t song;
	song.data = NULL;
	
	
	/************************** try opening the file **********************/
	printf("Reading %s...",vgm_filename);
	
	if (stat(vgm_filename, &vgm_stat))
	{
		printf("can't stat: %s\n", strerror(errno));
		goto load_vgm_fail;
	}
	size_t vgm_size = vgm_stat.st_size;
	vgm = malloc(vgm_size);
	if (!vgm)
	{
		puts("can't allocate memory");
		goto load_vgm_fail;
	}
	FILE *vgm_f = fopen(vgm_filename,"rb");
	if (!vgm_f)
	{
		printf("can't open: %s\n", strerror(errno));
		goto load_vgm_fail;
	}
	size_t vgm_read = fread(vgm,1,vgm_size,vgm_f);
	int vgm_read_err = errno;
	fclose(vgm_f);
	if (vgm_read != vgm_size)
	{
		printf("read error: %s\n", strerror(vgm_read_err));
		goto load_vgm_fail;
	}
	
	
	
	/********* check file validity ******************/
	if (memcmp(vgm,"Vgm ",4))
	{
		puts("not a vgm");
		goto load_vgm_fail;
	}
	unsigned vgm_reported_size = get32(vgm+4)+4;
	if (vgm_reported_size != vgm_size)
	{
		printf("bad vgm size (%zu bytes, should be %u)", vgm_size, vgm_reported_size);
		goto load_vgm_fail;
	}
	unsigned vgm_version = get32(vgm+8);
	unsigned vgm_loop_offset = get32(vgm+0x1c);
	if (vgm_loop_offset) vgm_loop_offset += 0x1c;
	unsigned vgm_data_offset = vgm_version < 0x150 ? 0x40 : get32(vgm+0x34)+0x34;
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
	uint8_t *vp = vgm+vgm_data_offset;
	uint8_t *vlp = vgm_loop_offset ? vgm+vgm_loop_offset : NULL;
	uint8_t *ve = vgm+vgm_size;
	song.data = NULL;
	song.length = 0;
	song.max = 0;
	
	/*************** playback state **************/
	playback_state_t state;
	playback_state_t prv;
	init_playback_state(&state);
	init_playback_state(&prv);
	
	unsigned psg_latch = 0;
	
	unsigned active_stream_id = -1; /* the stream that last started a sample */
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
	
	unsigned current_wait = 0;
	int loop_hit = 0;
	while (1)
	{
		unsigned c = *vp++;
		/****************** END COMMAND *********************/
		if (c == 0x66)
		{ /* end */
			flush_playback_state(&song, &prv, &state);
			write_song_command(&song, 0x6a, 0);
			break;
		}
		/********************* DATA BLOCKS *******************/
		else if (c == 0x67)
		{ /* data block */
			vp++; /* skip 0x66 */
			unsigned type = *vp++;
			unsigned size = get32(vp);
			vp += 4;
			
			if (!type)
			{
				unsigned i = stream_data_blocks++;
				stream_data_block_tbl[i] = vp;
				stream_data_block_sizes[i] = size;
			}
			
			vp += size;
		}
		/**************** SAMPLE STREAMS *****************/
		else if (c == 0x90)
		{ /* stream setup */
			unsigned sid = *vp++;
			unsigned chip = *vp++;
			unsigned port = *vp++;
			unsigned reg = *vp++;
			if (chip == 2 && !port && reg == 0x2a)
			{
				stream_active_flags[sid/8] |= 1 << (sid&7);
			}
		}
		else if (c == 0x91)
		{ /* stream data */
			unsigned sid = *vp++;
			unsigned bank = *vp++;
			unsigned step = *vp++;
			unsigned base = *vp++;
			
			stream_bank_tbl[sid] = bank;
			stream_step_tbl[sid] = step;
			stream_base_tbl[sid] = base;
		}
		else if (c == 0x92)
		{ /* stream frequency */
			unsigned sid = *vp++;
			unsigned freq = get32(vp);
			vp += 4;
			
			/* right now we have freq in hz, change it to native value */
			freq = round(freq * (Z80_CLOCKS_PER_SAMPLE / Z80_CLOCK) * 256.0);
			
			stream_freq_tbl[sid] = freq;
			
			/* set */
			if ((stream_active_flags[sid/8] & (1 << (sid&7))))
				state.sample_freq = freq;
		}
		else if (c == 0x94)
		{ /* stop stream */
			unsigned sid = *vp++;
			if (sid == active_stream_id)
			{
				write_song_command(&song, 0x66, 0);
			}
		}
		else if (c == 0x95)
		{ /* fast start stream */
			unsigned sid = *vp++;
			unsigned block = get16(vp);
			vp += 2;
			unsigned flags = *vp++;
			
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
					write_song_command(&song, 0x66, 0);
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
					
					/* command */
					write_song_command(&song, 0x64 + ((flags & 1) ? 1 : 0), new_id);
				}
			}
		}
		/********************** FM COMMANDS ********************/
		else if (c == 0x52 || c == 0x53)
		{
			unsigned part = c & 1;
			unsigned r = *vp++;
			unsigned v = *vp++;
			
			/* I'm not gonna do anything if your FM register is invalid. */
			if (is_valid_fm_register(part,r))
			{
				/* only applies for per-operator/channel registers */
				unsigned chn = (r & 3) + (part ? 3 : 0);
				unsigned op = (r & 0xc) >> 2;
				
				unsigned rr = r & 0xf0;
				unsigned rrr = r & 0xfc;
				
				/********************* fm patch control ***********************/
				if (is_fm_patch_register(part,r))
				{
					fm_patch_t *p = &state.fm.patch[chn];
					
					if (rr == 0x30)
						p->reg30[op] = v & 0x7f;
					else if (rr == 0x40)
						p->reg40[op] = v & 0x7f;
					else if (rr == 0x50)
						p->reg50[op] = v & 0xdf;
					else if (rr == 0x60)
						p->reg60[op] = v & 0x9f;
					else if (rr == 0x70)
						p->reg70[op] = v & 0x1f;
					else if (rr == 0x80)
						p->reg80[op] = v;
					else if (rr == 0x90)
						p->reg90[op] = v & 0x0f;
					else if (rrr == 0xb0)
						p->regb0 = v & 0x3f;
					else if (rrr == 0xb4)
						p->regb4 = v & 0xf7;
				}
				/*********************** everything else ********************/
				else
				{
					/****** frequency ******/
					if (r >= 0xa0 && r < 0xa7)
					{
						uint16_t *freq = &state.fm.freq[chn];
						if (!(r & 4))
						{ /* low */
							*freq = (*freq & 0xff00) | v;
						}
						else
						{ /* high */
							*freq = (*freq & 0xff) | (v<<8);
						}
					}
					
					/***** special frequency ******/
					if (r >= 0xa8 && r < 0xaf)
					{
						uint16_t *freq = &state.fm.extd_freq[r&3];
						if (!(r & 4))
						{ /* low */
							*freq = (*freq & 0xff00) | v;
						}
						else
						{ /* high */
							*freq = (*freq & 0xff) | (v<<8);
						}
					}
					
					/******** following regs only exist in part 1, but is_valid_fm_register() has already filtered them *********/
					/******** key on/off ********/
					if (r == 0x28)
					{
						unsigned chn = v & 0x7;
						if (chn >= 4) chn--;
						v &= 0xf0;
						
						uint8_t *ko = &state.fm.keyon[chn];
						if (v != *ko)
						{
							*ko = v;
							flush_fm_keyon_state(&song, &prv, &state);
						}
					}
					
					/********* chn.3 mode *********/
					if (r == 0x27)
					{
						v &= 0x40;
						state.fm.chn3_mode = v;
					}
					
					/********* dac toggle ********/
					if (r == 0x2b)
					{
						v &= 0x80;
						v >>= 7;
						state.fm.dac_enable = v;
					}
					
					/******** lfo ***********/
					if (r == 0x22)
					{
						v &= 0x0f;
						state.fm.lfo = v;
					}
				}
			}
		}
		/************************* PSG COMMANDS *******************/
		else if (c == 0x50)
		{
			unsigned v = *vp++;
			if (v & 0x80)
			{ /* latch */
				psg_latch = v & 0x70;
				unsigned data = v & 0x0f;
				unsigned channel = (psg_latch >> 5) & 3;
				if (psg_latch & 0x10)
					state.psg.volume[channel] = ((~data)&0x0f);
				else if (channel == 3)
					state.psg.freq[3] = data & 7;
				else
					state.psg.freq[channel] = (state.psg.freq[channel] & 0x3f0) | data;
			}
			else
			{ /* data */
				unsigned data = v & 0x3f;
				unsigned channel = (psg_latch >> 5) & 3;
				if (psg_latch & 0x10)
					state.psg.volume[channel] = ((~data)&0x0f);
				else if (channel == 3)
					state.psg.freq[3] = data & 7;
				else
					state.psg.freq[channel] = (state.psg.freq[channel] & 0xf) | (data<<4);
			}
		}
		/********************** WAIT COMMANDS *********************/
		else if ((c & 0xf0) == 0x70)
		{ /* don't bother with minuscule waits */
			current_wait += (c & 0x0f)+1;
		}
		else if (c >= 0x61 && c <= 0x63)
		{ /* regular waits */
			while (1)
			{
				switch (c)
				{
					case 0x61:
					{
						unsigned wait = *vp++;
						wait |= (*vp++)<<8;
						
						current_wait += wait;
						break;
					}
					case 0x62:
						current_wait += NTSC_INTERVAL;
						break;
					case 0x63:
						current_wait += PAL_INTERVAL;
						break;
				}
				
				c = *vp;
				if (!(c >= 0x61 && c <= 0x63) || vp == vlp) break;
				vp++;
			}
			
			flush_playback_state(&song, &prv, &state);
			
			unsigned ntsc_frames = current_wait / NTSC_INTERVAL;
			unsigned ntsc_extra = current_wait % NTSC_INTERVAL;
			unsigned pal_frames = current_wait / PAL_INTERVAL;
			unsigned pal_extra = current_wait % PAL_INTERVAL;
			
			if (!ntsc_extra)
			{
				while (ntsc_frames > 0)
				{
					unsigned i = min(ntsc_frames,0x20);
					write_song_command(&song, 0xc0 + (i - 1), 0);
					ntsc_frames -= i;
				}
			}
			else if (!pal_extra)
			{
				while (pal_frames > 0)
				{
					unsigned i = min(pal_frames,0x20);
					write_song_command(&song, 0xe0 + (i - 1), 0);
					pal_frames -= i;
				}
			}
			else
			{
				while (current_wait > 0)
				{
					unsigned i = min(current_wait,0xffff);
					write_song_command(&song, 0x6b, i);
					current_wait -= i;
				}
			}
			
			current_wait = 0;
		}
		/********************* BAD COMMANDS *********************/
		else if (c >= 0x30 && c < 0x51)
			vp += 1;
		else if (c >= 0x51 && c < 0x60)
			vp += 2;
		else if (c >= 0xa0 && c < 0xc0)
			vp += 2;
		else if (c >= 0xc0 && c < 0xe0)
			vp += 3;
		else if (c >= 0xe1)
			vp += 4;
		else
		{
			printf("Impossible command $%02X at $%X. Cannot continue.\n",c,(unsigned)(vp-vgm-1));
			goto load_vgm_fail;
		}
		
		/******************* check for loop ****************/
		if (!loop_hit && vlp)
		{
			if (vp > vlp)
			{
				printf("WARNING: Passed loop point at $%X. Song will not loop.\n", vgm_loop_offset);
				loop_hit = 1;
			}
			else if (vp == vlp)
			{
				flush_playback_state(&song, &prv, &state);
				write_song_command(&song, 0x69, 0);
				loop_hit = 1;
			}
		}
		
		/******************* peculiar behavior failsafes ****************/
		if (song.length > 0x100000)
		{
			puts("Passed 0x100000 output commands. This either indicates a bug in this program or an extremely large VGM file.");
			goto load_vgm_fail;
		}
		
		if (vp > ve)
		{
			puts("Passed VGM end-of-file. This either indicates a bug in this program or a malformed VGM file.");
			goto load_vgm_fail;
		}
	}
	
	
	
	
	
	
	/********************************** final ending *****************************/
	
	puts("Success.");
	return_val = 0;
	
	add_song(&song);
	for (size_t i = 0; i < song.length; i++) {
		songs_size += command_size_tbl[song.data[i].cmd];
	}
	
	goto load_vgm_cleanup;
	
load_vgm_fail:
	puts("Failed.");
	
	/* discard data generated by a bad vgm */
	free(song.data);
	for (size_t i = old_fm_patches; i < fm_patches; i++)
		/* fm patch cleanup here */ ;
	for (size_t i = old_samples; i < samples; i++)
		free(sample_tbl[i].data);
	for (size_t i = old_songs; i < songs; i++)
		free(song_tbl[i].data);
	fm_patches = old_fm_patches;
	samples = old_samples;
	songs = old_songs;
	samples_size = old_samples_size;
	
	
load_vgm_cleanup:
	
	free(vgm);
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

#define SONG_CMD	((unsigned)-4)	// used to define song starts in the compressed datastream
#define LAB_CMD	((unsigned)-3)	// used to define label numbers in the compressed datastream
#define NOP_CMD ((unsigned)-2)	// temporary cmd value used to hold "holes" in the datastream
#define CALL_CMD	((unsigned)-1)	// temporary cmd value used when building the pairs to denote a token id

#define INSTANCE_NONE (~(size_t)0)
#define SEQUENCE_NONE (~(size_t)0)

#define CALL_SIZE 4
#define INLINE_THRESHOLD 4	// subroutines <= this size are never created

#define CALL_NEST_LIMIT 32

/////////////////////// pair (all pairs in data, used when building the dictionary)

typedef struct {
	size_t left;	// left and right are stored separately as there could be a "NOP_CMD" between the pair elements
	size_t right;	// INSTANCE_NONE indicates this entry is no longer valid
} song_command_pair_instance_t;

typedef struct {
	size_t pair_index;
	size_t ii;
} song_command_pair_instance_ptr_t;

typedef struct {
	song_command_t pair[2];
	unsigned freq;	// actual amount of instances
	song_command_pair_instance_t * instance_tbl;	// list of instances of this pair
	size_t instances_max;	// allocated space for instances table
	size_t instances_length;	// length of instances table, including null-entries
} song_command_pair_t;

typedef struct {
	song_command_pair_t * pairs;	// the pair table itself
	size_t length;
	size_t max;
	size_t first_free;

	size_t highest_pair;
	unsigned highest_pair_freq;

	song_command_pair_instance_ptr_t * left_instance_tbl;	// table that maps source data -> pairs
	song_command_pair_instance_ptr_t * right_instance_tbl;	// table that maps source data -> pairs
} song_command_pair_list_t;

void remove_pair(song_command_pair_list_t * list, size_t pair_index) {
	song_command_pair_t * p = &list->pairs[pair_index];

	p->instances_length = 0;
	if (pair_index < list->first_free) {
		list->first_free = pair_index;
	}
}

void remove_pair_instance(song_command_pair_list_t * list, song_command_pair_instance_ptr_t * ip) {
	size_t pair_index = ip->pair_index;
	if (pair_index < list->length) {
		song_command_pair_t * p = &list->pairs[pair_index];
		if (ip->ii < p->instances_length) {
			song_command_pair_instance_t * i = &p->instance_tbl[ip->ii];
			if (i->left != INSTANCE_NONE && i->right != INSTANCE_NONE) {
				list->left_instance_tbl[i->left].pair_index = INSTANCE_NONE;
				list->right_instance_tbl[i->right].pair_index = INSTANCE_NONE;
				i->left = INSTANCE_NONE;
				i->right = INSTANCE_NONE;
				p->freq--;
				if (!p->freq) {
					remove_pair(list, pair_index);
				}
			}
		}
	}
}

size_t add_pair_instance(song_command_pair_list_t * list, song_command_t * left, song_command_t * right, size_t left_i, size_t right_i) {
	// try finding an instance of this pair in the list already
	size_t found_i = INSTANCE_NONE;
	song_command_pair_t * p = NULL;
	for (size_t i = 0; i < list->length; i++) {
		p = &list->pairs[i];
		if (!memcmp(left, &p->pair[0], sizeof(song_command_t)) && !memcmp(right, &p->pair[1], sizeof(song_command_t))) {
			found_i = i;
			break;
		}
	}

	// if nothing was found then create a new pair, otherwise add this instance to the old pair
	if (found_i == INSTANCE_NONE) {
		found_i = list->first_free;
		if (found_i == list->max) {
			list->max *= 2;
			list->pairs = realloc(list->pairs, list->max * sizeof(*list->pairs));
		}
		if (list->first_free++ == list->length) {
			list->length++;
		}

		p = &list->pairs[found_i];
		memcpy(&p->pair[0], left, sizeof(song_command_t));
		memcpy(&p->pair[1], right, sizeof(song_command_t));
		p->freq = 0;
		p->instance_tbl = malloc(0x10 * sizeof(*p->instance_tbl));
		p->instances_length = 0;
		p->instances_max = 0x10;

		while (list->first_free < list->length && list->pairs[list->first_free].freq != 0) {
			list->first_free++;
		}
	} else {
		if (p->instances_length == p->instances_max) {
			p->instances_max *= 2;
			p->instance_tbl = realloc(p->instance_tbl, p->instances_max * sizeof(*p->instance_tbl));
		}
	}
	p->instance_tbl[p->instances_length].left = left_i;
	p->instance_tbl[p->instances_length].right = right_i;
	list->left_instance_tbl[left_i].pair_index = found_i;
	list->right_instance_tbl[right_i].pair_index = found_i;
	list->left_instance_tbl[left_i].ii = p->instances_length;
	list->right_instance_tbl[right_i].ii = p->instances_length;
	p->instances_length++;
	p->freq++;

	return found_i;
}

void update_highest_pair(song_command_pair_list_t * list) {
	list->highest_pair = -1;
	list->highest_pair_freq = 0;
	for (size_t pi = 0; pi < list->length; pi++) {
		song_command_pair_t * p = &list->pairs[pi];
		if (p->freq > list->highest_pair_freq) {
			list->highest_pair_freq = p->freq;
			list->highest_pair = pi;
		}
	}
}

void init_pair_list(song_command_pair_list_t * list, song_command_t * data, size_t length) {
	// initialize data structure
	list->pairs = malloc(length * sizeof(*list->pairs));
	list->length = 0;
	list->max = length;
	list->first_free = 0;

	list->highest_pair = -1;
	list->highest_pair_freq = 0;

	list->left_instance_tbl = malloc(length * sizeof(*list->left_instance_tbl));
	list->right_instance_tbl = malloc(length * sizeof(*list->right_instance_tbl));
	memset(list->left_instance_tbl, -1, length * sizeof(*list->left_instance_tbl));
	memset(list->right_instance_tbl, -1, length * sizeof(*list->right_instance_tbl));

	// do the first pass through the input data
	for (size_t index = 0; index < length-1; index++) {
		song_command_t * this = &data[index];

		// if either this or the next pair would have an invalid control command then skip
		if (this[0].cmd == 0x69 || this[1].cmd == 0x69 || this[0].cmd == 0x6a || this[1].cmd == 0x6a) {
			continue;
		}

		// otherwise add this pair to the list
		size_t pair_index = add_pair_instance(list, &this[0], &this[1], index+0, index+1);
		unsigned pair_freq = list->pairs[pair_index].freq;
		if (pair_freq > list->highest_pair_freq) {
			list->highest_pair_freq = pair_freq;
			list->highest_pair = pair_index;
		}
	}
}

void free_pair_list(song_command_pair_list_t * list) {
	for (size_t i = 0; i < list->length; i++) {
		free(list->pairs[i].instance_tbl);
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
		write_compressed_song_command(song, dict_tbl, seq_tbl, is_end ? 0x67 : 0x68, di, size_ptr, is_end);
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
		all_song_data_length += song_tbl[i].length;
	}
	song_command_t * all_song_data = malloc(all_song_data_length * sizeof(song_command_t));
	size_t all_song_data_index = 0;
	for (size_t i = 0; i < songs; i++) {
		memcpy(&all_song_data[all_song_data_index], song_tbl[i].data, song_tbl[i].length*sizeof(song_command_t));
		all_song_data_index += song_tbl[i].length;
	}

	// run through the song data and find initial set of pairs
	printf("initializing...");
	fflush(stdout);

	song_command_pair_list_t pair_list;
	init_pair_list(&pair_list, all_song_data, all_song_data_length);

	// begin main iteration loop of finding most common pair and replacing sequences
	int iter = 0;
	song_command_pair_dict_t * dict_tbl = malloc(0x20000 * sizeof(*dict_tbl));
	size_t dict_max = 0x20000;
	size_t dict_length = 0;
	int fib[3] = {0,1,1};
	while (iter < max_iterations && pair_list.highest_pair_freq >= pair_freq_threshold) {
		iter++;
		if (iter >= fib[2]) {	// iterations get faster so we don't want to print on every one
			printf("%d...", iter);
			fflush(stdout);

			fib[0] = fib[1];
			fib[1] = fib[2];
			fib[2] = fib[0] + fib[1];
		}

		song_command_pair_t * p = &pair_list.pairs[pair_list.highest_pair];

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
		for (size_t ii = 0; ii < p->instances_length; ii++) {
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
				if (new_right >= all_song_data_length || all_song_data[new_right].cmd == 0x69 || all_song_data[new_right].cmd == 0x6a)
					new_right = INSTANCE_NONE;

				// update the pair relying on the left side we just replaced, if any
				song_command_pair_instance_ptr_t * left_inst_ptr = &pair_list.right_instance_tbl[left];
				if (left_inst_ptr->pair_index != INSTANCE_NONE) {
					song_command_pair_t * left_pair = &pair_list.pairs[left_inst_ptr->pair_index];
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
			}
		}

		dict_length++;

		// find the next most common pair
		update_highest_pair(&pair_list);
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
	int cur_song_id = -1;
	for (size_t i = 0; i < all_song_data_length; i++) {
		if (song_id != cur_song_id) {
			write_song_command(&compressed_song_data, SONG_CMD, song_id);
			cur_song_id = song_id;
		}

		unsigned cmd = all_song_data[i].cmd;
		unsigned param = all_song_data[i].param;
		write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, cmd, param, &compressed_song_data_size, 0);

		if (cmd == 0x6a)
			song_id++;
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
					write_song_command(&compressed_song_data, LAB_CMD, di);
				}

				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, d->pair[0].cmd, d->pair[0].param, &compressed_song_data_size, 0);

				if (di == s->right)
					break;
				di = d->pair[1].param;
				d = &dict_tbl[di];
			}
			if (!write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, d->pair[1].cmd, d->pair[1].param, &compressed_song_data_size, 1))
				write_compressed_song_command(&compressed_song_data, dict_tbl, seq_tbl, 0x6a, 0, &compressed_song_data_size, 0);
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
		puts("usage: knl-vgm-convert outname vgms...");
		return EXIT_FAILURE;
	}
	
	for (int i = 2; i < argc; i++)
	{
		load_vgm(argv[i]);
		putchar('\n');
		for (int i = 0; i < 75; i++) putchar('-');
		putchar('\n');
	}

	compress_song_data(50000, 2);
	
	
	/********* report ********************/
	printf("\n%zu songs, %zu FM patches, %zu samples.\n",songs,fm_patches,samples);
	if (!songs)
	{
		puts("No valid songs, quitting.");
		return EXIT_FAILURE;
	}
	
	size_t fm_patches_size = fm_patches*(7*4 + 2);
	size_t total_size = songs_size+fm_patches_size+samples_size;
	printf("Song data: %zu bytes, FM patches: %zu bytes, samples: %zu bytes.\n"
		,songs_size,fm_patches_size,samples_size);
	printf("Total size: %zu bytes (%.1f KiB).\n", total_size,total_size/1024.0);
	
	int toomany = 0;
	if (songs > MAX_SONGS)
	{
		printf("Too many songs! (max %u)\n",MAX_SONGS);
		toomany++;
	}
	if (fm_patches > MAX_FM_PATCHES)
	{
		printf("Too many FM patches! (max %u)\n",MAX_FM_PATCHES);
		toomany++;
	}
	if (samples > MAX_SAMPLES)
	{
		printf("Too many samples! (max %u)\n",MAX_SAMPLES);
		toomany++;
	}
	if (toomany) return EXIT_FAILURE;
	
	
	
	/******************************** OUTPUT FILE ***********************************/
	FILE *f = fopen(argv[1],"w");
	if (!f)
	{
		printf("Couldn't open %s: %s\n",argv[1],strerror(errno));
		return EXIT_FAILURE;
	}
	
	time_t ttttt = time(NULL);
	fprintf(f,";\n; Generated on %s;\n\n", ctime(&ttttt));
	
	
	
	/************** song data streams ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; Song data streams.\n align 1\nknl_song_tbl: dl ");
	for (size_t i = 0; i < songs; i++)
	{
		fprintf(f,"knl_song_%zu",i);
		if (i < songs-1) fputc(',',f);
	}
	
	/***** actual data *****/
	/*for (size_t i = 0; i < songs; i++)
	{
		song_t *s = &song_tbl[i];
		fprintf(f,"\nknl_song_%zu: db ",i);
		
		for (size_t j = 0; j < s->length; j++)
		{
			const song_command_t * sc = &s->data[j];
			unsigned command_size = command_size_tbl[sc->cmd];
			if (!command_size) {
				printf("Illegal command %02X found in song %zu\n", sc->cmd, i);
				return EXIT_FAILURE;
			}
			fprintf(f,"%u",sc->cmd);
			for (unsigned b = 1; b < command_size; b++) {
				fprintf(f,",%u",(sc->param>>(8*(b-1)))&0xff);
			}
			if (j < s->length-1) {
				fputc(',', f);
				fputc(' ', f);
			}
		}
	}*/
	for (size_t i = 0; i < compressed_song_data.length; i++) {
		const song_command_t * sc = &compressed_song_data.data[i];
		if (sc->cmd == SONG_CMD) {
			fprintf(f,"\nknl_song_%u:",sc->param);
		} else if (sc->cmd == LAB_CMD) {
			fprintf(f,"\nknl_lab_%u:",sc->param);
		} else if (sc->cmd == 0x67) {
			fprintf(f,"\n\tdl knl_lab_%u+$67000000",sc->param);
		} else if (sc->cmd == 0x68) {
			fprintf(f,"\n\tdl knl_lab_%u+$68000000",sc->param);
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
	
	
	
	/************** fm patches ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; FM patches.\n align 1\nknl_fm_patch_tbl:");
	if (fm_patches)
	{
		fprintf(f," dl ");
		
		for (size_t i = 0; i < fm_patches; i++)
		{
			fprintf(f,"knl_fm_patch_%zu",i);
			if (i < fm_patches-1) fputc(',',f);
		}
		
		/***** actual data *****/
		for (size_t i = 0; i < fm_patches; i++)
		{
			fm_patch_t *p = &fm_patch_tbl[i];
			fprintf(f,"\nknl_fm_patch_%zu: db ",i);
			
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg30[0],p->reg30[1],p->reg30[2],p->reg30[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg40[0],p->reg40[1],p->reg40[2],p->reg40[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg50[0],p->reg50[1],p->reg50[2],p->reg50[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg60[0],p->reg60[1],p->reg60[2],p->reg60[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg70[0],p->reg70[1],p->reg70[2],p->reg70[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg80[0],p->reg80[1],p->reg80[2],p->reg80[3]);
			fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg90[0],p->reg90[1],p->reg90[2],p->reg90[3]);
			
			fprintf(f,"$%02X, $%02X", p->regb0,p->regb4);
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
		
		/***** actual datas *****/
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



