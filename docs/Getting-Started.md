
## Contents
1. [Requirements](Getting-Started#Requirements)
2. [Building](Getting-Started#Building)
3. [Installing](Getting-Started#Installing)


## Requirements

To install QEMU click [here](https://www.qemu.org/download/), or type:
```
$ sudo apt-get install qemu
```

To install Nasm click [here](https://www.nasm.us/pub/nasm/releasebuilds/2.14.02/), or type:
```
$ sudo apt-get install nasm
```

As for getting yourself an i686-elf cross-compiler you can click
[here](https://wiki.osdev.org/GCC_Cross-Compiler) for more information on compiling it yourself, 
or just use some precompiled binarys I have attached 
[here](https://github.com/Joshua-Riek/SuccOS/releases/) in the first release. Lastly, if you are using Windows, please download [ImDisk](https://sourceforge.net/projects/imdisk-toolkit/) for installing the kernel to the floppy disk image.


## Building

To checkout the source and build:
```
$ git clone https://github.com/Joshua-Riek/SuccOS.git
$ cd SuccOS/
$ make
```

A build should look like this:
```
$ make
nasm src/kernel.asm -O0 -f elf -g3 -F dwarf -o obj/kernel.o
i686-elf-ld obj/kernel.o -e entryPoint -m elf_i386 -Ttext=0x1000 -o bin/kernel.elf
objcopy bin/kernel.elf -O binary bin/kernel.bin
```

To build without a cross-compiller:
```
$ nasm src/kernel.asm -O0 -f bin -o bin/kernel.bin
```

## Installing

To install the kernel on linux:
```
$ sudo make install-linux
```

A successful install should look like:
```
$ sudo make install-linux
cp 1440k.img floppy.img
rm -rf tmp-loop
mkdir tmp-loop
mount -o loop -t vfat floppy.img tmp-loop
cp ./bin/kernel.bin tmp-loop/
sleep 0.2
unmount tmp-loop || exit
rm -rf tmp-loop
```

To install the kernel on Windows:
```
$ make install-win
```

A successful install should look like:
```
$ make install-win
cp 1440k.img floppy.img
imdisk -a -f floppy.img -m B:
Creating device...
Created device 2: B: -> floppy.img
Notifying applications...
Done.
cp ./bin/kernel.bin B:/kernel.bin
imdisk -D -m B:
Notifying applications...
Flushing file buffers...
Locking volume...
Failed, forcing dismount...
Removing device...
Removing mountpoint...
Done.
```
