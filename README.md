## x86 Kernel

This is a 16-bit real mode kernel for x86-compatible PCs, written entirely in assembly language, which can boot from a floppy disk, hard disk, USB, or CD.

## Requirements

Please install the packages below, or type:

```
$ sudo apt-get install gdb nasm qemu dosfstools mtools
```
> This project uses an optional i686-elf cross-compiler, you can click 
[here](https://wiki.osdev.org/GCC_Cross-Compiler) for more 
information on compiling it yourself, or use some precompiled binaries 
[here](https://github.com/lordmilko/i686-elf-tools/releases).

## Building

To checkout the source and build:

```
$ git clone https://github.com/Joshua-Riek/x86-kernel
$ cd x86-kernel
$ make
```

## Virtual Machine

To run the bootloader in a virtual machine:
```
$ make run
```

## Virtual Machine Debugging

Start a virtual machine with a GDB stub:
```
$ make debug
```

Open another ternimal and connect to the virtual machine's GDB stub:
```
$ make gdb
```
> For debug symbols to be generated, you must compile with an i686-elf cross-compiller.
