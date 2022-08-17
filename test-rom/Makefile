
.PHONY: default clean
default: test-rom.gen
clean: knl-clean
	$(RM) test-rom.elf test-rom.gen


KNL_DIR := ../
KNL_SONG_SLOTS := 2
KNL_VGMS := ../test-vgms/*.vgm

include $(KNL_DIR)Makefile


LD := m68k-linux-gnu-ld

test-rom.elf: test-rom.asm font.rom exceptions.asm
	$(KNL_VASM) -Felf $(KNL_VASMFLAGS) -o $@ $<

test-rom.gen: link.ld test-rom.elf $(KNL_OUT)
	$(LD) -M -T link.ld --oformat binary -o $@ test-rom.elf $(KNL_OUT)
