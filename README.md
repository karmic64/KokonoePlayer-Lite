# KokonoePlayer-Lite

KokonoePlayer-Lite is yet another VGM-player sound driver for the Sega Genesis/MegaDrive. It supports many VGM "song slots" for sound effects, and sample playback. It runs entirely on the Z80, so the 68000 is free for whatever else it can do.

## Prerequisites

To use KokonoePlayer-Lite, you need the following:

* `make`, and a Makefile that builds your project. Other build systems are unsupported.
* A **native** C compiler (preferably `gcc`), to compile the VGM converter.
* [vasm](http://sun.hasenbraten.de/vasm/), to assemble the 68000 part. When running `make`, use `SYNTAX=mot CPU=m68k`. The output executable should be named `vasmm68k_mot`.
* [WLA-DX](https://www.villehelin.com/wla.html), to assemble the Z80 part.
* A 68000 ELF-compatible linker, to link the KokonoePlayer-Lite output file into your ROM.

## Integrating KokonoePlayer-Lite into your build process

This section assumes you are using `make` to build your project.

First off, add the following to your `Makefile`:
```make
KNL_DIR := ...
KNL_SONG_SLOTS := ...
KNL_VGMS := ...

include $(KNL_DIR)Makefile
```
Make sure you have defined a default target before including the KokonoePlayer-Lite Makefile, otherwise `make` will try looking in there for it and not in your own Makefile. The include should be placed near the top of your Makefile, after the default rule, but high enough so all of your other rules can access the KokonoePlayer-Lite variables. The KokonoePlayer-Lite Makefile has its own default rule that will warn you if you have not defined a default rule yet.

Replace the `KNL_DIR` value with the path to the root of the KokonoePlayer-Lite sources, **with** a trailing slash. Replace the `KNL_SONG_SLOTS` value with the amount of song slots you wish to use. Replace the `KNL_VGMS` value with the path of every VGM file you wish to use.

If you are using C, find any rule that compiles individual source files into output files. Add the option `-I $(KNL_DIR)`, allowing the compiler to locate the `knl.h` header file.

If you have a phony clean target, add `knl-clean` as a dependency.

Now find the rule in your `Makefile` that links the final ROM. Add `$(KNL_OUT)` both as a dependency and an input file. Now, the next time you build your ROM, the KokonoePlayer-Lite .elf file will be built and linked with it.

If you look at the KokonoePlayer-Lite `Makefile`, there are a wide variety of other variables which you can override as well. For example, if your build tools are not in PATH, you could do something like this in your `Makefile` before including the KokonoePlayer-Lite `Makefile`:
```make
KNL_CC := ./tools/gcc/gcc.exe
KNL_VASM := ./tools/vasm/vasmm68k_mot.exe
KNL_WLAZ80 := ./tools/wla/wla-z80.exe
KNL_WLALINK := ./tools/wla/wlalink.exe
```

Especially notable is the `KNL_BASE_INTERVAL` variable. This value allows you to change how often the VGM data is processed, in 44100Hz-sample units. If your music uses odd refresh rates, or is designed for PAL, it's worth finding the value that makes your music sound the best. Just keep in mind, the lower the value, the more time will be spent processing, so samples will sound worse. The default is 368, or about 120Hz.

## Using the KokonoePlayer-Lite functions

### In assembly language

KokonoePlayer-Lite has been designed to interface with C, thus the routines use the standard `gcc` C calling convention. For example, take the following function prototype:
```c
unsigned knl_func(void * the_pointer, unsigned the_int, unsigned short the_short)
```

You push the parameters on the stack in **reverse order**, `jsr` to the relevant label, then pull the parameters off. For the above function:
```m68k
	; push them on...
	pea the_short
	pea the_int
	pea the_pointer
	; call the function...
	jsr knl_func
	; then pull them off.
	lea 12(sp),sp
```
Parameters are ALWAYS pushed as longs, even if the actual sizes are smaller!

The routines themselves are guaranteed to preserve the values of `d2-d7/a2-a7`, but `d0-d1/a0-a1` are scratch registers and you should not rely on their values being preserved. Return values are passed in `d0`.

### In C

Simply `#include <knl.h>`, then call the functions as you would any other. Make sure you set `$(KNL_DIR)` as a header file directory in your `Makefile`!

### Thread-safety precautions

KokonoePlayer-Lite functions are **not** thread-safe. You must ensure that only one is ever called at a time.

To request the Z80 bus (e.g. for DMA), it is OK to simply write directly to the bus request register. However, do not call any KokonoePlayer-Lite functions while the Z80 bus must be held, because they may release the bus.

## KokonoePlayer function reference

### knl_reset
```c
void knl_reset();
```
Resets the player variables to a known state, and uploads the Z80 code. Call this in your initial reset code.

### knl_init
```c
void knl_init(unsigned song_slot, unsigned song_id);
```
Initializes song `song_id` playback in the song slot `song_slot`. Song IDs are assigned incrementally starting from 0, in the order you gave when assigning the value of `KNL_VGMS`.

Higher-numbered song slots are higher priority. For example, you could have 4 song slots, 0 for music, 1 and 2 for sound effects, and 3 for music cues that overtake all other sound (for example, the extra life jingle in Sonic).

### knl_stop
```c
void knl_stop(unsigned song_slot);
```
Stops playback of song slot `song_slot`.

### knl_pause
```c
void knl_pause(unsigned song_slot);
```
Pauses playback of song slot `song_slot`.

### knl_resume
```c
void knl_resume(unsigned song_slot);
```
Resumes playback of song slot `song_slot`, if it was paused.

## Disclaimer

All the VGMs in this repository are used for testing purposes only, and are taken unmodified from DefleMask and Furnace's demo song libraries. They are not mine.

The `vgmspec170.txt` document contains the official VGM specifications. It is put here for reference purposes only.