### setupDisk
Setup values for the bios peramater block
* IN: `Nothing`
* OUT: `Nothing`

### readSectors
Read the given sectors from the disk
* IN: `ES:DI, AX:DX, CX` = Location to load the sectors, LBA, Sectors to read
* OUT: `CF` = Carry flag set on error

### writeSectors
Write the given sectors to the disk
* IN: `ES:DI, AX:DX, CX` = Location of sectors to write, LBA, Sectors to write
* OUT: `CF` = Carry flag set on error

### loadCwd
Allocates and load the current working directory into memory
* IN: `Nothing`
* OUT: `ES:DI, CF` = Location of dir in memory, Carry flag set on error

### unloadCwd
Unallocate and free the current working directory from memory
* IN: `Nothing`
* OUT: `Nothing`

### readCwd
Load the current working directory into memory
* IN: `ES:DI` = Location to load the dir
* OUT: `CF` = Carry flag set on error

### writeCwd
Write the current working firectory to the disk
* IN: `ES:DI` = Location of the dir to write
* OUT: `CF` = Carry flag set on error

### allocCwd
Allocate the current dir into the memory map
* IN: `Nothing`
* OUT: `ES:DI, CF` = Location allocaded for the dir, Carry flag set on error

### freeCwd
Free the current working dir from use in the memory map
* IN: `ES:DI` = Location of dir to free from memory 
* OUT: `Nothing`

### loadFat
Allocates and load the FAT table into memory
* IN: `Nothing`
* OUT: `ES:DI, CF` = Location of FAT in memory, Carry flag set on error

### unloadFat
Unallocate and free the FAT table from memory
* IN: `Nothing`
* OUT: `Nothing`

### readFat
Read the FAT table into memory
* IN: `ES:DI` = Location to load the FAT
* OUT: `CF` = Carry flag set on error

### writeFat
Write the FAT table to the disk
* IN: `ES:DI` = Location of the FAT to write
* OUT: `CF` = Carry flag set on error

### allocFat
Allocate the FAT table into the memory map
* IN: `Nothing`
* OUT: `ES:DI, CF` = Location allocaded for the FAT, Carry flag set on error

### freeFAT
Free the FAT table from use in the memory map
* IN: `ES:DI` = Location of FAT to free from memory 
* OUT: `Nothing`

### searchDir
Search through a loaded dir for a single file
* IN: `ES:DI, DS:SI` = Disk buffer with loaded dir, Filename in 8.3 format
* OUT: `ES:DI, CF` = Pointer to file entry, Carry flag set on error

### changeDir 
Change the current working directory
* IN: `DS:SI` = Directory to search for
* OUT: `CF` = Cary flag set on error

### createDir
Create an empty directory
* IN: `DS:SI` = Directory to create
* OUT: `CF` = Cary flag set on error

### removeDir
Remove an empty directory
* IN: `DS:SI` = Directory to remove
* OUT: `CF` = Cary flag set on error

### fileSize
Search for a file and return it's size
* IN: `DS:SI` = Filename to search for
* OUT: `AX:DX, CF` = Filesize, Carry flag set on error

### fileExists
Search for a file to see if it exists
* IN: `DS:SI` = Filename to search for
* OUT: `CF` = Cary flag set on error

### createFile
Create an empty file
* IN: `DS:SI` = Filename to create
* OUT: `CF` = Carry flag set on error

### renameFile
Rename a file
* IN: `DS:SI, ES:DI` = File to rename, New filename
* OUT: `CF` = Carry flag set on error

### deleteFile
Delete a file
* IN: `DS:SI` = File to delete
* OUT: `CF` = Carry flag set on error

### readFile
Read a file from disk and into memory
* IN: `DS:SI, ES:DI` = Name of file to read, Location to load the file
* OUT: `DX:AX, CF` = Filesize, Carry flag set on error

### writeFile
Write a file from memory onto the disk
* IN: `DS:SI, ES:DI, DX:AX` = Name of file to write, Location of file's contents, Filesize
* OUT: `CF` = Carry flag set on error

