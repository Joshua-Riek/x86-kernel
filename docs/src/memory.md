# Memory Functions

### setupMemory
Get the inital low memory and allocate areas into the memory map
* IN: `Nothing`
* OUT: `Nothing`

### memBytesToBlocks32
Calculate how many blocks are required to allocate from the size in bytes
* IN: `AX:DX` = Size in bytes
* OUT: `BX` = Blocks (1 block = 512 bytes)

### memBlockToAddress
Convert the block into a segment and offset address
* IN: `CX` = Block to get the address of
* OUT: `ES:DI` = Pointer to the corresponding block

### memAddressToBlock
Convert the segment and offset address into a block
* IN: `ES:DI` = Pointer to memory address
* OUT: `CX` = Block corresponding to the address

### memAllocNextBlock
Allocate the next available block in memory
* IN: `Nothing`
* OUT: `ES:DI, CF` = Pointer to memory address, Carry flag set on error

### memAllocBlocks
Allocate the next available blocks in memory
* IN: `BX` = Blocks to allocate (1 block is 512 bytes)
* OUT: `ES:DI, CF` = Pointer to memory adress, Carry flag set on error

### memAllocBytes
Allocate the next available bytes in memory
* IN: `AX:DX` = Size in bytes
* OUT: `ES:DI, CF` = Pointer to memory address, Carry flag set on error

### memFreeBlock
Free the block in memory related to the address
* IN: `ES:DI` = Pointer to memory address
* OUT: `Nothing`

### memFreeBlocks
Free the blocks in memory related to the address
* IN: `ES:DI, BX` = Pointer to memory address, Blocks to free (1 block is 512 bytes)
* OUT: `Nothing`

### memFreeBytes
Free the bytes in memory related to the address
* IN: `ES:DI, AX:DX` = Pointer to memory address, size in bytes
* OUT: `Nothing`

### allocMemAddress
Manually allocate an address into memory
* IN: `ES:DI, AX:DX` = Pointer to memory address, size in bytes
* OUT: `CF` = Carry flag set on error
