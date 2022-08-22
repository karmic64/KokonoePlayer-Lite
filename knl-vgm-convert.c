
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
	$e0-$ff - wait for $xx 50hz intervals
	$c0-$df - wait for $xx 60hz intervals
	$69 $xxxx - wait for $xxxx 44100hz intervals
	$6a - set loop point
	$6b - loop if there is a loop, otherwise stop

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
	$60 - normal chn.3 mode
	$61 - extd. chn.3 mode
	$62 - DAC disable
	$63 - DAC enable
	$64 $yy - set LFO to $yy
	$65 $yyyy - set sample output frequency to $yyyy
	$66 $yy - start sample $yy
	$67 $yy - start looped sample $yy
	$68 - stop sample

*/



#define min(a,b) (((a) < (b)) ? (a) : (b))

#define NTSC_INTERVAL 735
#define PAL_INTERVAL 882

#define Z80_CLOCK 3579545.0
#define Z80_CLOCKS_PER_SAMPLE 104.0




unsigned get16(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8); }
unsigned get24(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16); }
unsigned get32(uint8_t *p) { return (*(p+0)) | (*(p+1)<<8) | (*(p+2)<<16) | (*(p+3)<<24); }



/*************** structs ****************/

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

typedef struct {
	uint8_t * data;
	size_t size;
} song_t;
int song_cmp(song_t *a, song_t *b)
{
	if (a->size != b->size) return 1;
	return memcmp(a->data, b->data, a->size);
}



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
	size_t old_songs_size = songs_size;
	
	
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
		printf("bad vgm size (%lu bytes, should be %u)", vgm_size, vgm_reported_size);
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
	song.data = malloc(vgm_size);
	uint8_t *sp = song.data;
	
	/*************** playback state functions **************/
	playback_state_t state;
	playback_state_t prv;
	init_playback_state(&state);
	init_playback_state(&prv);
	
	/* flushes fm patches if any of them changed */
	void flush_fm_patches()
	{
		for (int chn = 0; chn < 6; chn++)
		{
			fm_patch_t *old = &prv.fm.patch[chn];
			fm_patch_t *new = &state.fm.patch[chn];
			if (!fm_patch_cmp(old,new)) continue;
			
			unsigned new_id = add_fm_patch(new);
			memcpy(old, new, sizeof(*new));
			
			*sp++ = 3 + (chn << 4) + (new_id < 0x100 ? 0 : 1);
			*sp++ = new_id & 0xff;
			if (new_id >= 0x100) *sp++ = new_id >> 8;
		}
	}
	
	/* flushes fm freqs if any of them changed */
	void flush_fm_freqs()
	{
		/******** normal fm channels *********/
		for (int chn = 0; chn < 6; chn++)
		{
			uint16_t *old = &prv.fm.freq[chn];
			uint16_t *new = &state.fm.freq[chn];
			
			int lodiff = ((*old)&0xff) != ((*new)&0xff);
			int hidiff = ((*old)>>8) != ((*new)>>8);
			
			if (lodiff && hidiff)
			{
				*sp++ = (chn<<4) + 7;
				*sp++ = (*new)&0xff;
				*sp++ = (*new)>>8;
			}
			else if (lodiff)
			{
				*sp++ = (chn<<4) + 5;
				*sp++ = (*new)&0xff;
			}
			else if (hidiff)
			{
				*sp++ = (chn<<4) + 6;
				*sp++ = (*new)>>8;
			}
			
			*old = *new;
		}
		
		/********* ext.chn3 frequencies *************/
		for (int op = 0; op < 3; op++)
		{
			uint16_t *old = &prv.fm.extd_freq[op];
			uint16_t *new = &state.fm.extd_freq[op];
			
			int lodiff = ((*old)&0xff) != ((*new)&0xff);
			int hidiff = ((*old)>>8) != ((*new)>>8);
			
			if (lodiff && hidiff)
			{
				*sp++ = (op<<4) + 0xa;
				*sp++ = (*new)&0xff;
				*sp++ = (*new)>>8;
			}
			else if (lodiff)
			{
				*sp++ = (op<<4) + 0x8;
				*sp++ = (*new)&0xff;
			}
			else if (hidiff)
			{
				*sp++ = (op<<4) + 0x9;
				*sp++ = (*new)>>8;
			}
			
			*old = *new;
		}
	}
	
	/* flushes FM patches, freqs, and keyon state */
	void flush_fm_keyon_state()
	{
		flush_fm_patches();
		flush_fm_freqs();
		
		for (int chn = 0; chn < 6; chn++)
		{
			uint8_t *old = &prv.fm.keyon[chn];
			uint8_t *new = &state.fm.keyon[chn];
			
			if (*old != *new)
			{
				*old = *new;
				if (*new == 0x00)
					*sp++ = (chn<<4) + 0;
				else if (*new == 0xf0)
					*sp++ = (chn<<4) + 1;
				else
				{
					*sp++ = (chn<<4) + 2;
					*sp++ = *new;
				}
			}
		}
	}
	
	/* fully flush fm state */
	void flush_fm_state()
	{
		flush_fm_keyon_state();
		
		if (prv.fm.chn3_mode != state.fm.chn3_mode)
		{
			prv.fm.chn3_mode = state.fm.chn3_mode;
			
			*sp++ = 0x60 + (state.fm.chn3_mode ? 1 : 0);
		}
		
		if (prv.fm.dac_enable != state.fm.dac_enable)
		{
			prv.fm.dac_enable = state.fm.dac_enable;
			
			*sp++ = 0x62 + (state.fm.dac_enable ? 1 : 0);
		}
		
		if (prv.fm.lfo != state.fm.lfo)
		{
			prv.fm.lfo = state.fm.lfo;
			
			*sp++ = 0x64;
			*sp++ = state.fm.lfo;
		}
	}
	
	/* fully flush psg state */
	void flush_psg_state()
	{
		for (int chn = 0; chn < 4; chn++)
		{
			/* volume */
			uint8_t *v = &state.psg.volume[chn];
			uint8_t *pv = &prv.psg.volume[chn];
			if (*v != *pv)
			{
				*pv = *v;
				*sp++ = 0x80 + (chn<<4) + *v;
			}
			
			/* freq */
			uint16_t *f = &state.psg.freq[chn];
			uint8_t fl = (*f) & 0xff;
			uint8_t fh = (*f) >> 8;
			uint16_t *pf = &prv.psg.freq[chn];
			uint8_t pfl = (*pf) & 0xff;
			uint8_t pfh = (*pf) >> 8;
			if (chn != 3)
			{
				if (*pf == (uint16_t)-1 || fl != pfl)
				{ 
					*sp++ = 0x6c + chn;
					*sp++ = fl;
				}
				if (*pf == (uint16_t)-1 || fh != pfh)
				{
					*sp++ = 0x70 + (chn<<2) + fh;
				}
			}
			else
			{
				if (((*pf)&4) != ((*f)&4))
				{ /* noise mode change */
					*sp++ = 0x6f;
					*sp++ = fl;
				}
				else if (((*pf)&3) != ((*f)&3))
				{ /* only noise pitch change */
					*sp++ = 0x70 + (chn<<2) + (fl&3);
				}
			}
			*pf = *f;
		}
	}
	
	/* fully flush all state (used on wait commands/loop/end) */
	void flush_playback_state()
	{
		flush_fm_state();
		flush_psg_state();
		
		if (prv.sample_freq != state.sample_freq)
		{
			prv.sample_freq = state.sample_freq;
			
			*sp++ = 0x65;
			*sp++ = (state.sample_freq&0xff);
			*sp++ = (state.sample_freq>>8);
		}
	}
	
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
			flush_playback_state();
			*sp++ = 0x6b;
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
				*sp++ = 0x68;
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
					*sp++ = 0x68;
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
					*sp++ = 0x66 + ((flags & 1) ? 1 : 0);
					*sp++ = new_id;
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
					/****** frequency (todo: combined low/high command) ******/
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
							flush_fm_keyon_state();
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
			
			flush_playback_state();
			
			unsigned ntsc_frames = current_wait / NTSC_INTERVAL;
			unsigned ntsc_extra = current_wait % NTSC_INTERVAL;
			unsigned pal_frames = current_wait / PAL_INTERVAL;
			unsigned pal_extra = current_wait % PAL_INTERVAL;
			
			if (!ntsc_extra)
			{
				while (ntsc_frames > 0)
				{
					unsigned i = min(ntsc_frames,0x20);
					*sp++ = 0xc0 + (i - 1);
					ntsc_frames -= i;
				}
			}
			else if (!pal_extra)
			{
				while (pal_frames > 0)
				{
					unsigned i = min(ntsc_frames,0x20);
					*sp++ = 0xe0 + (i - 1);
					pal_frames -= i;
				}
			}
			else
			{
				while (current_wait > 0)
				{
					unsigned i = min(current_wait,0xffff);
					*sp++ = 0x69;
					*sp++ = i & 0xff;
					*sp++ = i >> 8;
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
				flush_playback_state();
				*sp++ = 0x6a;
				loop_hit = 1;
			}
		}
	}
	
	
	
	
	
	
	/********************************** final ending *****************************/
	
	puts("Success.");
	return_val = 0;
	
	song.size = sp - song.data;
	songs_size += song.size;
	add_song(&song);
	
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
	songs_size = old_songs_size;
	
	
load_vgm_cleanup:
	
	free(vgm);
	return return_val;
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
		for (int i = 0; i < 79; i++) putchar('-');
		putchar('\n');
	}
	
	
	/********* report ********************/
	printf("\n%lu songs, %lu FM patches, %lu samples.\n",songs,fm_patches,samples);
	if (!songs)
	{
		puts("No valid songs, quitting.");
		return EXIT_FAILURE;
	}
	
	size_t fm_patches_size = fm_patches*(7*4 + 2);
	size_t total_size = songs_size+fm_patches_size+samples_size;
	printf("Song data: %lu bytes, FM patches: %lu bytes, samples: %lu bytes.\n"
		,songs_size,fm_patches_size,samples_size);
	printf("Total size: %lu bytes (%.1f KiB).\n", total_size,total_size/1024.0);
	
	
	
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
		fprintf(f,"knl_song_%lu",i);
		if (i < songs-1) fputc(',',f);
	}
	
	/***** actual datas *****/
	for (size_t i = 0; i < songs; i++)
	{
		song_t *s = &song_tbl[i];
		fprintf(f,"\nknl_song_%lu: db ",i);
		
		for (size_t j = 0; j < s->size; j++)
		{
			fprintf(f,"$%02X",s->data[j]);
			if (j < s->size-1) fputc(',',f);
		}
	}
	
	
	
	/************** fm patches ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; FM patches.\n align 1\nknl_fm_patch_tbl: dl ");
	for (size_t i = 0; i < fm_patches; i++)
	{
		fprintf(f,"knl_fm_patch_%lu",i);
		if (i < fm_patches-1) fputc(',',f);
	}
	
	/***** actual data *****/
	for (size_t i = 0; i < fm_patches; i++)
	{
		fm_patch_t *p = &fm_patch_tbl[i];
		fprintf(f,"\nknl_fm_patch_%lu: db ",i);
		
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg30[0],p->reg30[1],p->reg30[2],p->reg30[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg40[0],p->reg40[1],p->reg40[2],p->reg40[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg50[0],p->reg50[1],p->reg50[2],p->reg50[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg60[0],p->reg60[1],p->reg60[2],p->reg60[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg70[0],p->reg70[1],p->reg70[2],p->reg70[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg80[0],p->reg80[1],p->reg80[2],p->reg80[3]);
		fprintf(f,"$%02X,$%02X,$%02X,$%02X, ", p->reg90[0],p->reg90[1],p->reg90[2],p->reg90[3]);
		
		fprintf(f,"$%02X, $%02X", p->regb0,p->regb4);
	}
	
	
	
	
	/************** samples ****************/
	/***** pointers *****/
	fprintf(f,"\n\n; Samples.\n align 1\nknl_sample_tbl: dl ");
	for (size_t i = 0; i < samples; i++)
	{
		fprintf(f,"knl_sample_%lu",i);
		if (i < samples-1) fputc(',',f);
	}
	
	/***** actual datas *****/
	for (size_t i = 0; i < samples; i++)
	{
		sample_t *s = &sample_tbl[i];
		fprintf(f,"\nknl_sample_%lu: db ",i);
		
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
	
	
	
	fclose(f);
	
	
	
}



