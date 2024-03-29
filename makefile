#  makefile
#
#  Copyright (c) 2017-2022, Joshua Riek
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Build tools
CC           := i686-elf-gcc
LD           := i686-elf-ld
AR           := i686-elf-ar
OBJCOPY      := i686-elf-objcopy
NASM         := nasm

# Output directory
SRCDIR        = ./src
OBJDIR        = ./obj
BINDIR        = ./bin

# Build flags
CFLAGS       +=
LDFLAGS      +=
ARFLAGS      +=
LDFLAGS      += -m elf_i386
NASMFLAGS    += -f elf -g3 -F dwarf
OBJCOPYFLAGS += -O binary

# This is just for make to re-compile when there is a change
INCLUDES = $(SRCDIR)/cli.asm \
		   $(SRCDIR)/cmos.asm \
		   $(SRCDIR)/disk.asm \
		   $(SRCDIR)/dos.asm \
		   $(SRCDIR)/fat.asm \
		   $(SRCDIR)/keyboard.asm \
		   $(SRCDIR)/math.asm \
		   $(SRCDIR)/memory.asm \
		   $(SRCDIR)/serial.asm \
		   $(SRCDIR)/string.asm \
		   $(SRCDIR)/test.asm \
		   $(SRCDIR)/video.asm


# Set phony targets
.PHONY: all clean clobber kernel bootloader image debug run gdb


# Rule to make targets
all: kernel bootloader image


# Makefile target for the kernel
ifeq ($(and $(shell which $(LD)),$(shell which $(OBJCOPY))),)
kernel: $(BINDIR)/kernel.bin

$(BINDIR)/kernel.bin: $(SRCDIR)/kernel.asm $(INCLUDES) | $(BINDIR)
	$(NASM) $< -O0 -f bin -o $@
else
kernel: $(BINDIR)/kernel.bin

$(BINDIR)/kernel.bin: $(BINDIR)/kernel.elf
	$(OBJCOPY) $^ $(OBJCOPYFLAGS) $@

$(BINDIR)/kernel.elf: $(OBJDIR)/kernel.o | $(BINDIR)
	$(LD) $^ $(LDFLAGS) -Ttext=0x1000 -o $@

$(OBJDIR)/kernel.o: $(SRCDIR)/kernel.asm $(INCLUDES) | $(OBJDIR)
	$(NASM) $< -O0 $(NASMFLAGS) -o $@
endif

# Makefile target for both bootloaders
ifeq ($(and $(shell which $(LD)),$(shell which $(OBJCOPY))),)
bootloader: $(BINDIR)/boot12.bin $(BINDIR)/boot16.bin

$(BINDIR)/boot12.bin: $(SRCDIR)/boot12.asm | $(BINDIR)
	$(NASM) $^ -f bin -o $@

$(BINDIR)/boot16.bin: $(SRCDIR)/boot16.asm | $(BINDIR)
	$(NASM) $^ -f bin -o $@
else
bootloader: $(BINDIR)/boot12.bin $(BINDIR)/boot16.bin

$(BINDIR)/boot12.bin: $(BINDIR)/boot12.elf
	$(OBJCOPY) $^ $(OBJCOPYFLAGS) $@

$(BINDIR)/boot12.elf: $(OBJDIR)/boot12.o | $(BINDIR)
	$(LD) $^ $(LDFLAGS) -Ttext=0x0000 -o $@

$(OBJDIR)/boot12.o: $(SRCDIR)/boot12.asm | $(OBJDIR)
	$(NASM) $^ $(NASMFLAGS) -o $@

$(BINDIR)/boot16.bin: $(BINDIR)/boot16.elf
	$(OBJCOPY) $^ $(OBJCOPYFLAGS) $@

$(BINDIR)/boot16.elf: $(OBJDIR)/boot16.o | $(BINDIR)
	$(LD) $^ $(LDFLAGS) -Ttext=0x0000 -o $@

$(OBJDIR)/boot16.o: $(SRCDIR)/boot16.asm | $(OBJDIR)
	$(NASM) $^ $(NASMFLAGS) -o $@
endif

# Makefile target to create both disk images
image: $(BINDIR)/boot12.img $(BINDIR)/boot16.img

$(BINDIR)/boot12.img: $(BINDIR)/boot12.bin $(BINDIR)/kernel.bin
	dd if=/dev/zero of=$@ bs=1024 count=1440 status=none
	mkfs.vfat -F12 $@ 1> /dev/null
	mcopy -n -i $@ $(BINDIR)/kernel.bin ::
	dd if=$< of=$@ bs=1 skip=62 seek=62 conv=notrunc status=none

$(BINDIR)/boot16.img: $(BINDIR)/boot16.bin $(BINDIR)/kernel.bin
	dd if=/dev/zero of=$@ bs=1024 count=16384 status=none
	mkfs.vfat -F16 $@ 1> /dev/null
	mcopy -i $@ $(BINDIR)/kernel.bin ::
	dd if=$< of=$@ bs=1 skip=62 seek=62 conv=notrunc status=none


# Create the obj dir
$(OBJDIR):
	@mkdir -p $@

# Create the bin dir
$(BINDIR):
	@mkdir -p $@


# Clean produced files
clean:
	rm -f $(OBJDIR)/* $(BINDIR)/*

# Clean files from emacs
clobber: clean
	rm -f $(SRCDIR)/*~ $(SRCDIR)\#*\# ./*~


# Makefile target to run or debug both disk images
ifeq ($(FAT), 16)
run: image
	qemu-system-i386 -serial stdio -rtc base=localtime -drive file=bin/boot16.img,format=raw

debug: image
	qemu-system-i386 -serial stdio -rtc base=localtime -S -s -drive file=bin/boot16.img,format=raw
else
run: image
	qemu-system-i386 -serial stdio -rtc base=localtime -drive file=bin/boot12.img,format=raw,if=floppy

debug: image
	qemu-system-i386 -serial stdio -rtc base=localtime -S -s -drive file=bin/boot12.img,format=raw,if=floppy
endif

gdb: image
	-gdb -q -ex "break *_start+5"
