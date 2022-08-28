
## user variables

ifndef KNL_DIR
$(warning KNL_DIR not defined. Defaulting to the current working directory.)
KNL_DIR := ./
endif
ifndef KNL_SONG_SLOTS
$(warning KNL_SONG_SLOTS not defined. Defaulting to 1.)
KNL_SONG_SLOTS := 1
endif
ifndef KNL_VGMS
$(error KNL_VGMS not defined)
endif

KNL_BASE_INTERVAL ?= 368




## build tools

ifdef COMSPEC
KNL_DOTEXE?=.exe
else
KNL_DOTEXE?=
endif


KNL_CC ?= gcc
KNL_VASM ?= vasmm68k_mot
KNL_WLAZ80 ?= wla-z80
KNL_WLALINK ?= wlalink

KNL_CFLAGS ?= -s -Ofast -Wall -Wextra
KNL_CLIBS ?= -lm
KNL_VASMFLAGS ?= -I$(KNL_DIR) -spaces -opt-speed -m68000 -DKNL_SONG_SLOTS=$(KNL_SONG_SLOTS) -DKNL_BASE_INTERVAL=$(KNL_BASE_INTERVAL)
KNL_WLAZ80FLAGS ?= -I $(KNL_DIR) -D KNL_SONG_SLOTS=$(KNL_SONG_SLOTS) -D KNL_BASE_INTERVAL=$(KNL_BASE_INTERVAL)
KNL_WLALINKFLAGS ?= -b



## inputs

KNL_CONVERT := $(KNL_DIR)knl-vgm-convert.c

KNL_68K := $(KNL_DIR)knl-68k.asm
KNL_Z80 := $(KNL_DIR)knl-z80.asm
KNL_Z80_LN := $(KNL_DIR)knl-z80.ln

## outputs

KNL_OUT_CONVERT := $(KNL_DIR)knl-vgm-convert$(DOTEXE)
KNL_OUT_MODULE := $(KNL_DIR)KNL-MODULE.asm

KNL_OUT_68K := $(KNL_DIR)knl-68k.elf
KNL_OUT_Z80 := $(KNL_DIR)knl-z80.o
KNL_OUT_Z80_LN := $(KNL_DIR)knl-z80.bin

## for external use

KNL_OUT := $(KNL_OUT_68K)




## phony targets

.PHONY: knl-default knl-clean
knl-default:
	@echo "Please don't run make directly on this Makefile."
	@echo "Refer to the documentation for how to properly integrate it into your project."
	@exit 1

knl-clean:
	$(RM) $(KNL_OUT_CONVERT) $(KNL_OUT_MODULE) $(KNL_OUT_68K) $(KNL_OUT_Z80) $(KNL_Z80_LN) $(KNL_OUT_Z80_LN)



## rules

$(KNL_OUT_MODULE): $(KNL_OUT_CONVERT) $(KNL_VGMS)
	$(KNL_OUT_CONVERT) $(KNL_OUT_MODULE) $(KNL_VGMS)


$(KNL_OUT_CONVERT): $(KNL_CONVERT)
	$(KNL_CC) $(KNL_CFLAGS) $< -o $@ $(KNL_CLIBS)



$(KNL_OUT_Z80): $(KNL_Z80)
	$(KNL_WLAZ80) $(KNL_WLAZ80FLAGS) -o $@ $<

# wlalink doesn't support include paths, so dynamically generate link file
$(KNL_OUT_Z80_LN): $(KNL_OUT_Z80)
	echo [objects] > $(KNL_Z80_LN) && \
		echo $(KNL_OUT_Z80) >> $(KNL_Z80_LN) && \
		$(KNL_WLALINK) $(KNL_WLALINKFLAGS) $(KNL_Z80_LN) $@

$(KNL_OUT_68K): $(KNL_68K) $(KNL_OUT_Z80_LN) $(KNL_OUT_MODULE)
	$(KNL_VASM) -Felf $(KNL_VASMFLAGS) -o $@ $<

