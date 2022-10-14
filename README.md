## x86 Kernel

This is a 16-bit real mode kernel for x86-compatible PCs, written entirely in assembly language, which can boot from a floppy disk, hard disk, USB, or CD.

## Requirements

Please install the packages below, or type:

```
$ sudo apt-get install nasm qemu dosfstools mtools build-essential
```

This project also uses an i686-elf cross-compiler, you can click 
[here](https://wiki.osdev.org/GCC_Cross-Compiler) for more information 
on compiling it yourself, or just use some precompiled binaries 
[here](https://github.com/lordmilko/i686-elf-tools/releases).

## Building

To checkout the source and build:

```
$ git clone https://github.com/Joshua-Riek/x86-kernel
$ cd x86-kernel
$ make
```

A build should look like this:

```
$ make
nasm src/kernel.asm -O0 -f elf -g3 -F dwarf -o obj/kernel.o
i686-elf-ld obj/kernel.o  -m elf_i386 -Ttext=0x1000 -o bin/kernel.elf
objcopy bin/kernel.elf -O binary bin/kernel.bin
nasm src/boot12.asm -f elf -g3 -F dwarf -o obj/boot12.o
i686-elf-ld obj/boot12.o  -m elf_i386 -Ttext=0x0000 -o bin/boot12.elf
objcopy bin/boot12.elf -O binary bin/boot12.bin
nasm src/boot16.asm -f elf -g3 -F dwarf -o obj/boot16.o
i686-elf-ld obj/boot16.o  -m elf_i386 -Ttext=0x0000 -o bin/boot16.elf
objcopy bin/boot16.elf -O binary bin/boot16.bin
dd if=/dev/zero of=bin/boot12.img bs=1024 count=1440 status=none
mkfs.vfat -F12 bin/boot12.img 1> /dev/null
mcopy -n -i bin/boot12.img ./bin/kernel.bin ::
dd if=bin/boot12.bin of=bin/boot12.img bs=1 skip=62 seek=62 conv=notrunc status=none
dd if=/dev/zero of=bin/boot16.img bs=1024 count=16384 status=none
mkfs.vfat -F16 bin/boot16.img 1> /dev/null
mcopy -i bin/boot16.img ./bin/kernel.bin ::
dd if=bin/boot16.bin of=bin/boot16.img bs=1 skip=62 seek=62 conv=notrunc status=none
```

To build without the i686-elf cross-compiller:

```
$ nasm src/kernel.asm -f bin -o bin/kernel.bin
$ nasm src/boot12.asm -f bin -o bin/boot12.bin
$ nasm src/boot16.asm -f bin -o bin/boot16.bin
```
