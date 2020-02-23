
## [Cli Functions](Cli-Functions)
Functions relating to the file [`cli.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/cli.asm)

* [cliLoop](Cli-Functions#cliLoop) - Main loop for the command line interface
* [doDir](Cli-Functions#doDir) - Command to show the contents of the a directory
* [doRename](Cli-Functions#doRename) - Command to rename a file
* [doDel](Cli-Functions#doDel) - Command to delete a file
* [doType](Cli-Functions#doType) - Command to show the contents of a file
* [doCopy](Cli-Functions#doCopy) - Command to copy one file to another
* [doCls](Cli-Functions#doCls) - Command to clear the screen
* [doTime](Cli-Functions#doTime) - Command to show the system time
* [doDate](Cli-Functions#doDate) - Command to show the system date
* [doHelp](Cli-Functions#doHelp) - Command to display the available cli commands
* [doCd](Cli-Functions#doCd) - Command to change the current directory
* [doMd](Cli-Functions#doMd) - Command to make a new directory
* [doRd](Cli-Functions#) - Command to remove a directory

## [Cmos Functions](Cmos-Functions)
Functions relating to the file [`cmos.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/cmos.asm)

* [cmosRead](Cmos-Functions#cmosRead) - Read the contents from a chosen CMOS register
* [cmosWrite](Cmos-Functions#cmosWrite) - Write to the chosen CMOS register
* [cmosReadDate](Cmos-Functions#cmosReadDate) - Get the system date from the CMOS
* [cmosReadTime](Cmos-Functions#cmosReadTime) - Get the system time from the CMOS
* [cmosDelay](Cmos-Functions#cmosDelay) - Use the CMOS to wait a period of time in seconds
* [bcd](Cmos-Functions#bcd) - Convert a value into a binary-coded decimal (BCD)

## [Disk Functions](Disk-Functions)
Functions relating to the file [`disk.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/disk.asm)

* [setupDisk](Disk-Functions#setupDisk) - Setup values for the bios paramater block
* [readSectors](Disk-Functions#readSectors) - Read sectors from the disk
* [writeSectors](Disk-Functions#writeSectors) - Write sectors to the disk
* [loadCwd](Disk-Functions#loadCwd) - Allocate and load the cwd
* [unloadCwd](Disk-Functions#unloadCwd) - Unallocate and free the cwd
* [readCwd](Disk-Functions#readCwd) - Read the cwd into memory
* [writeCwd](Disk-Functions#writeCwd) - Write the cwd to the disk
* [allocCwd](Disk-Functions#allocCwd) - Allocate the cwd into memory
* [freeCwd](Disk-Functions#freeCwd) - Free the cwd from memory
* [loadFat](Disk-Functions#loadFat) - Allocate and load the FAT
* [unloadFat](Disk-Functions#unloadFat) - Unallocate and free the FAT
* [readFat](Disk-Functions#readFat) - Read the FAT into memory
* [writeFat](Disk-Functions#writeFat) - Write the FAT to the disk
* [allocFat](Disk-Functions#allocFat) - Allocate the FAT into memory
* [freeFat](Disk-Functions#freeFat) - Free the FAT from memory
* [searchDir](Disk-Functions#searchDir) - Search a loaded dir for an 8.3 filename
* [changeDir](Disk-Functions#changeDir) - Change and set the cwd
* [createDir](Disk-Functions#createDir) - Create an empty directory
* [removeDir](Disk-Functions#removeDir) - Remove an empty directory
* [fileSize](Disk-Functions#fileSize) - Get the size of a file
* [fileExists](Disk-Functions#fileExists) - Check to see if a file exists
* [createFile](Disk-Functions#createFile) - Create an empty file
* [renameFile](Disk-Functions#renameFile) - Rename an existing file 
* [deleteFile](Disk-Functions#deleteFile) - Delete a single file
* [readFile](Disk-Functions#readFile) - Read the contents of a file
* [writeFile](Disk-Functions#writeFile) - Write data to a file

## [Fat Functions](Fat-Functions)
Functions relating to the file [`fat.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/fat.asm)

* [readClustersFAT12](Fat-Functions#readClustersFAT12) - Read the data from FAT12 clusters into memory
* [readClustersFAT16](Fat-Functions#readClustersFAT16) - Read the data from FAT16 clusters into memory
* [readClusters](Fat-Functions#readClusters) - Use FAT12 or FAT16, then read the clusters
* [removeClustersFAT12](Fat-Functions#removeClustersFAT12) - Remove FAT12 clusters from the FAT table
* [removeClustersFAT16](Fat-Functions#removeClustersFAT16) - Remove FAT16 clusters from the FAT table
* [removeClusters](Fat-Functions#removeClusters) - Determine to use FAT12 or FAT16, then remove the clusters
* [writeClustersFAT12](Fat-Functions#writeClustersFAT12) - Write new FAT12 clusters and data to the disk
* [writeClustersFAT16](Fat-Functions#writeClustersFAT16) - Write new FAT16 clusters and data to the disk
* [writeClusters](Fat-Functions#writeClusters) - Determine to use FAT12 or FAT16, then write the clusters

## [Kernel Functions](Kernel-Functions)
Functions relating to the file [`kernel.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/kernel.asm)

* [entryPoint](Kernel-Functions#entryPoint) - Primary entrypoint into the operating system

## [Keyboard Functions](Keyboard-Functions)
Functions relating to the file [`keyboard.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/keyboard.asm)

* [setupKbdCtrl](Keyboard-Functions#setupKbdCtrl) - Set my own kbd controller interrupt
* [kbdCtrlRead](Keyboard-Functions#kbdCtrlRead) - Read a byte from the kbd controller
* [kbdCtrlWrite](Keyboard-Functions#kbdCtrlWrite) - Send a byte to the kbd controller
* [kbdCtrlSendCmd](Keyboard-Functions#kbdCtrlSendCmd) - Send a command to the kbd controller
* [kbdCtrlSelfTest](Keyboard-Functions#kbdCtrlSelfTest) - Preform a kbd controller self test
* [kbdCtrlSetLeds](Keyboard-Functions#kbdCtrlSetLeds) - Set the kbd leds
* [kbdCtrlWaitUntillKey](Keyboard-Functions#kbdCtrlWaitUntillKey) - Wait untill a key is pressed
* [kbdCtrlCaptureInput](Keyboard-Functions#kbdCtrlCaptureInput) - Get input from kbd and fill into buffer
* [kbdCtrlHandler](Keyboard-Functions#kbdCtrlHandler) - Custom handler for the kbd controller 
* [kbdEncSendCmd](Keyboard-Functions#kbdEncSendCmd) - Send a command to the kbd encoder
* [scanToAscii](Keyboard-Functions#ScanToAscii) - Translate scan code to ascii and set flags
* [kbdBiosFlushBuffer](Keyboard-Functions#kbdBiosFlushBuffer) - Flush key from BIOS kbd buffer
* [kbdBiosCheckBuffer](Keyboard-Functions#kbdBiosCheckBuffer) - Check the BIOS kbd buffer for a key
* [kbdBiosGetChar](Keyboard-Functions#kbdBiosGetChar) - Grab a char from the BIOS kbd buffer
* [kbdBiosStoreKey](Keyboard-Functions#kbdBiosStoreKey) - Shove char into the BIOS kbd buffer
* [kbdBiosWaitUntillKey](Keyboard-Functions#kbdBiosWaitUntillKey) - Wait untill a key is in the BIOS kbd buffer
* [kbdBiosCaptureInput](Keyboard-Functions#kbdBiosCaptureInput) - Get input from BIOS kbd and fill into buffer

## [Memory Functions](Memory-Functions)
Functions relating to the file [`memory.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/memory.asm)

* [setupMemory](Memory-Functions#setupMemory) - Get the inital low memory and allocate areas into the memory map
* [memBytesToBlocks32](Memory-Functions#memBytesToBlocks32) - Calculate how many blocks from the size in bytes
* [memBlockToAddress](Memory-Functions#memBlockToAddress) - Convert the block into a segment and offset address
* [memAddressToBlock](Memory-Functions#memAddressToBlock) - Convert the segment and offset address into a block
* [memAllocNextBlock](Memory-Functions#memAllocNextBlock) - Allocate the next available block in memory
* [memAllocBlocks](Memory-Functions#memAllocBlocks) - Allocate the next available blocks in memory
* [memAllocBytes](Memory-Functions#memAllocBytes) - Allocate the next available bytes in memory
* [memFreeBlock](Memory-Functions#memFreeBlock) - Free the block in memory related to the address
* [memFreeBlocks](Memory-Functions#memFreeBlocks) - Free the blocks in memory related to the address 
* [memFreeBytes](Memory-Functions#memFreeBytes) - Free the bytes in memory related to the address
* [allocMemAddress](Memory-Functions#allocMemAddress) - Manually allocate an address into memory

## [Serial Functions](Serial-Functions)
Functions relating to the file [`serial.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/serial.asm)

* [initSerial](Serial-Functions#initSerial) - Setup the serial (com1) port
* [writeSerial](Serial-Functions#writeSerial) - Write some data to the serial port
* [readSerial](Serial-Functions#readSerial) - Read some data from the serial port
* [writeSerialStr](Serial-Functions#writeSerialStr) - Write a string to the serial port
* [writeSerialNumPadding32](Serial-Functions#writeSerialNumPadding32) - Write a formatted number to the serial port


## [String Functions](String-Functions)
Functions relating to the file [`string.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/string.asm)

* [itoa](String-Functions#itoa) - Converts a 32-bit number and base to a string
* [atoi](String-Functions#atoi) - Converts a string and base to a 16-bit number
* [convertFilename83](String-Functions#convertFilename83) - Convert the filename into a fat formatted filename
* [padStr](String-Functions#padStr) - Pad a string with Ascii characters
* [parseStr](String-Functions#parseStr) - Split the string into tokens
* [strCmp](String-Functions#strCmp) - Checks to see if the two passed strings are equal
* [strLen](String-Functions#strLen) - Get the length of a string
* [charToLower](String-Functions#charToLower) - Convert an Acsii letter to a lowercase character
* [charToUpper](String-Functions#charToUpper) - Convert an Acsii letter to a uppercase character

## [Video Functions](Video-Functions)
Functions relating to the file [`video.asm`](https://github.com/Joshua-Riek/SuccOS/blob/master/src/video.asm)

* [setupVideo](Video-Functions#setupVideo) - Get the cursor position from the BIOS, then save for use
* [videoWriteChar](Video-Functions#videoWriteChar) - Write a character into video memory
* [videoWriteStr](Video-Functions#videoWriteStr) - Write a string into video memory
* [videoWriteNumPadding32](Video-Functions#videoWriteNumPadding32) - Write a 32-bit number into video memory with padding
* [videoWriteNumPadding](Video-Functions#videoWriteNumPadding) - Write a 16-bit number into video memory with padding
* [videoWriteNum32](Video-Functions#videoWriteNum32) - Write a 32-bit number into video memory
* [videoWriteNum](Video-Functions#videoWriteNum) - Write a 16-bit number into video memory
* [videoScroll](Video-Functions#videoScroll) - Scroll the screen up one line, if cursor is on the last available line.
* [videoClearScreen](Video-Functions#videoClearScreen) - Clear the screen of text
* [videoSaveScreen](Video-Functions#videoSaveScreen) - Save the contents of the screen into memory
* [videoRestoreScreen](Video-Functions#videoRestoreScreen) - Restore the contents of the screen from memory
* [videoUpdateBiosCur](Video-Functions#videoUpdateBiosCur) - Update the BIOS cursor
* [videoUpdateCur](Video-Functions#videoUpdateCur) - Update the hardware cursor
