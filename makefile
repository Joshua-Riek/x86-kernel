#  makefile
#
#  Copyright (c) 2017-2020, Joshua Riek
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
NASM         ?= nasm
OBJCOPY      ?= objcopy
DD           ?= dd

# Other tools
QEMU         ?= qemu-system-i386

# Output directory
SRCDIR        = ./src
OBJDIR        = ./obj
BINDIR        = ./bin

# Build flags
CFLAGS       +=
LDFLAGS      +=
ARFLAGS      +=
LDFLAGS      += -e entryPoint -m elf_i386 -Ttext=0x1000
NASMFLAGS    += -O0 -f elf -g3 -F dwarf
OBJCOPYFLAGS += -O binary

# Disk image file
DISKIMG       = floppy.img

# Set phony targets
.PHONY: all clean clobber kernel install-linux install-win debug run


# Rule to make targets
all: kernel


# Makefile target for the kernel
kernel: $(BINDIR)/kernel.bin

$(BINDIR)/kernel.bin: $(BINDIR)/kernel.elf
	$(OBJCOPY) $^ $(OBJCOPYFLAGS) $@

$(BINDIR)/kernel.elf: $(OBJDIR)/kernel.o | $(BINDIR)
	$(LD) $^ $(LDFLAGS) -o $@

$(OBJDIR)/kernel.o: $(SRCDIR)/kernel.asm | $(OBJDIR)
	$(NASM) $^ $(NASMFLAGS) -o $@

$(OBJDIR):
	@mkdir -p $@

$(BINDIR):
	@mkdir -p $@


# Clean produced files
clean:
	rm -f $(OBJDIR)/* $(BINDIR)/*.bin $(BINDIR)/*.elf

# Clean files from emacs
clobber: clean
	rm -f $(SRCDIR)/*~ $(SRCDIR)\#*\# ./*~


# Fix for error "cp: cannot create regular file"
# mount B: \b

# Write the kernel to a disk image
install-win: 
	cp 1440k.img $(DISKIMG)
	imdisk -a -f $(DISKIMG) -m B:
#	cp $(BINDIR)/kernel.bin B:/kernel.bin
	cp $(BINDIR)/*.bin B:/
	imdisk -D -m B:

# Write the kernel to a disk image
install-linux:
	cp 1440k.img $(DISKIMG)
	rm -rf tmp-loop

	mkdir tmp-loop
	mount -o loop -t vfat $(DISKIMG) tmp-loop
	cp $(BINDIR)/kernel.bin tmp-loop/

	sleep 0.2
	unmount tmp-loop || exit
	rm -rf tmp-loop

# Windows fat16 install

#install:
#	cp $(BINDIR)/kernel.bin B:/kernel.bin
#	rawcopy -l -m \\\.\B: $(DISKIMG)


# Run the disk image
run:
	$(QEMU) -serial stdio -rtc base=localtime -fda $(DISKIMG)


# Start a debug session with qemu
debug:
	$(QEMU) -serial stdio -rtc base=localtime -S -s -fda $(DISKIMG)

