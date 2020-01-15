;  memory.asm
;
;  This file handles memory allocation.  
;  Copyright (c) 2017-2020, Joshua Riek
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

;---------------------------------------------------
; Memory manager contants
;---------------------------------------------------
    
    %define BLOCKS_PER_KB 2
    %define BLOCK_SIZE    512

;---------------------------------------------------
; Memory manager varables
;---------------------------------------------------
    
    loMemMaxBlocks  dw 0
    loMemUsedBlocks dw 0
    loMemMapSeg     dw 0
    loMemMapOff     dw 0
    kernelSize      dw 0x4000
    
;---------------------------------------------------
setupMemory:
;
; Get the inital low memory and allocate areas
; into the memory map.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es
    
    mov ax, cs                                  ; Set the data segment to the code segment
    mov ds, ax                                  ; This is to ensure correct data refrences     
    
    clc                                         ; Clear carry flag
    int 0x12                                    ; Get Conventional memory size
    jc .error

    xor dx, dx
    mov bx, BLOCKS_PER_KB                       ; Take the lower memory size
    mul bx                                      ; and multiply, for ammount of blocks
    mov word [loMemMaxBlocks], ax

    add bx, KERNEL_OFF
    mov bx, word [kernelSize]                   ; Get the kernel size    
    call memBytesToBlocks                       ; Get the num of blocks to allocate (rounded value)

    mov bx, BLOCK_SIZE                          ; Get the block size 
    mul bx                                      ; Multiply back to the value

    mov dx, ax                                  ; Offset for bitmap location
    mov ax, es                                  ; Segment for bitmap location

    mov word [loMemMapOff], dx                  ; Save the bitmap location
    mov word [loMemMapSeg], ax

    mov bx, word [loMemMaxBlocks]               ; Allocate the bitmap into memory
    call allocMemAddress
    jc .error

    mov bx, 0x1000                              ; Allocate the bios data area into memory
    mov ax, 0x0000                              ; Segment
    mov dx, 0x0000                              ; Offset
    call allocMemAddress
    jc .error

    mov bx, word [kernelSize]                   ; Allocate the kernel into memory
    mov ax, es                                  ; Segment
    mov dx, KERNEL_OFF                          ; Offset
    call allocMemAddress
    jc .error
    
    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
 
    clc                                         ; Clear carry, for no error
    ret
    
  .error:
    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    stc                                         ; Set carry, error occured
    ret
    
;---------------------------------------------------
memBytesToBlocks:
;
; Calculate how many blocks are required to allocate
; from the size in bytes, note that this
; value is rounded up if remander.
;
; Expects:    BX = Size in bytes
;
; Returns:    AX = Rounded up size in blocks
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push dx
    
    xor dx, dx                                  ; Clear remander
    mov ax, bx                                  ; Grab the size param
    mov bx, BLOCK_SIZE                          ; Divide by block size in bytes
    div bx
    
    or dx, dx                                   ; Test for remander
    jz .roundUp                                 ; Jump if zero flag set
    inc ax                                      ; If remander, add one
    
  .roundUp:
    pop dx                                      ; Restore registers
    pop cx
    pop bx
    ret
        
;---------------------------------------------------
memBytesToBlocks32:
;
; Calculate how many blocks are required to allocate
; from the size in bytes, note that this
; value is rounded up if remander.
;
; Expects:    AX:DX = Size in bytes
;
; Returns:    BX    = Rounded up size in blocks
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push cx
    push dx
    
    mov bx, BLOCK_SIZE                          ; Divide by block size in bytes
    div bx
    
    or dx, dx                                   ; Test for remander
    jz .roundUp                                 ; Jump if zero flag set
    inc ax                                      ; If remander, add one
    
  .roundUp:
    xchg ax, bx
    
    pop dx                                      ; Restore registers
    pop cx
    pop ax
    ret
    
;---------------------------------------------------
memBlockToAddress:
;
; Convert the block into a segment
; and offset address.
;
; Expects:    CX = Block
;
; Returns: AX:DX = Segment:Offset
;
;---------------------------------------------------
    push cx                                     ; Save register
    
    clc
    xor ax, ax                                  ; Clear segment output register
    xor dx, dx                                  ; Clear offset output register

  .loopBlocks:
    add dx, BLOCK_SIZE                          ; Add the block size to the offset
    jnc .nextBlock                              ; Continue to next block if no carry
    
    add ah, 0x10                                ; Correct segment for 64k boundry

  .nextBlock:
    loop .loopBlocks 

    pop cx                                      ; Restore register
    ret
    
;---------------------------------------------------
memAddressToBlock:
;
; Convert the segment and offset into the correct
; index into the memory bitmap.
;
; Expects: AX:DX = Segment:Offset
;
; Returns:    CX = Block number/ index
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx
    
    push dx                                     ; Save the offset 
    
    xor dx, dx                                  ; Clear remander
    mov bx, BLOCK_SIZE                          ; Get the the block size
    div bx                                      ; Divide ax:bx
    shl ax, 1                                   ; This is a segment address so shift left
    shl ax, 1
    shl ax, 1
    shl ax, 1
    mov cx, ax                                  ; Movte into cx

    xor dx, dx                                  ; Clear remander
    pop ax                                      ; Grab the offset param
    mov bx, BLOCK_SIZE                          ; Get the block size
    div bx                                      ; Divide ax:bx
    
    add cx, ax                                  ; Add the segment and offset

    pop dx                                      ; Restore registers
    pop bx
    pop ax
    ret
   
;---------------------------------------------------   
memAllocNextBlock:
;
; Allocate the next available block.
;    
; Expects:  None
;
; Returns: AX:DX = Segment:Offset
;          CF    = Set on error
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx
    
    mov cx, word [loMemMaxBlocks]
    sub cx, word [loMemUsedBlocks]
    cmp cx, 0                                   ; See if their are no more available
    jle .error                                  ; memory blocks

    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location

    xor cx, cx
    
  .checkBitmap:
    mov al, byte [es:di]                        ; Grab a byte from es:di
    cmp al, 0x00                                ; Check for a free block (0x00)
    je .done                                    ; If free, bail out

    inc di                                      ; Increase segment offset
    inc cx                                      ; Increase counter register

    cmp cx, word [loMemMaxBlocks]               ; Continue through the bitmap while cx
    jle .checkBitmap                            ; is less than the max memory blocks

  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    stc                                         ; Return carry on error
    ret

  .done:
    mov byte [es:di], 0x0f                      ; Set the bit to used (0x0f)
    inc word [loMemUsedBlocks]                  ; Increase the used low memory

    call memBlockToAddress                      ; Get the return address for ax:dx
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    clc                                         ; No error, so clear carry
    ret

;---------------------------------------------------   
memAllocBlocks:
;
; Allocate the next available blocks.
;    
; Expects: BX    = Blocks to allocate
;
; Returns: AX:DX = Segment:Offset
;          CF    = Set on error
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx
    
    mov cx, word [loMemMaxBlocks]
    sub cx, word [loMemUsedBlocks]
    cmp cx, 0                                   ; See if their are no more available
    jle .error                                  ; memory blocks

    cmp bx, word [loMemMaxBlocks]               ; Ensure that the blocks to allocate is 
    jg .error                                   ; within the memory limit
    
    cmp bx, cx                                  ; Ensure that the blocks to allocate is
    jg .error                                   ; not more than the available blocks

    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location

    xor cx, cx
    
  .checkBitmap:
    push di                                     ; Save index
    xor dx, dx                                  ; Clear for a counter

  .checkBlocks:
    mov al, byte [es:di]                        ; Grab a byte from es:di
    cmp al, 0x00                                ; Check for a free block (0x00)
    jne .nextBlock                              ; If empty, continue on
    
    inc di                                      ; Increase offset
    inc dx                                      ; Increase counter

    cmp dx, bx                                  ; See if the counter is greater than the size
    jl .checkBlocks
    
  .nextBlock:
    pop di                                      ; Restore index
    cmp dx, bx                                  ; See if the their is a continueous section
    jnl .done                                   ; of blocks equal to the size wanted
    
    inc di                                      ; Increase segment offset
    inc cx                                      ; Increase counter register

    cmp cx, word [loMemMaxBlocks]               ; Continue through the bitmap while cx
    jle .checkBitmap                            ; is less than the max memory blocks

  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    stc                                         ; Return carry on error
    ret

  .done:
    push cx                                     ; Save index of block
    mov cx, bx                                  ; Get the block size
    
  .setBlocks:
    mov byte [es:di], 0x0f                      ; Set the bit to used (0xf)
    inc word [loMemUsedBlocks]                  ; Increase the used low memory
    inc di                                      ; Increase segment offset
    loop .setBlocks
    
    pop cx                                      ; Get the index of the block
    call memBlockToAddress                      ; Get the return address for ax:dx

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    clc                                         ; No error, so clear carry
    ret

;---------------------------------------------------   
memFreeBlocks:
;
; Free x ammount of blocks.
;    
; Expects: AX:DX = Segment:Offset
;             BX = Blocks to free
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx
    
    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location
    call memAddressToBlock                      ; Convert address to block index
    
    add di, cx                                  ; Add block index to offset
    mov cx, bx                                  ; Blocks to free
    
  .free:
    mov byte [es:di], 0x00                      ; Set the bit to free (0x0)
    dec word [loMemUsedBlocks]                  ; Decrease the used low memory
    inc di                                      ; Increase segment offset
    loop .free

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;---------------------------------------------------   
memFreeBlock:
;
; Free the block related to the address.
;    
; Expects: AX:DX = Segment:Offset
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx
    
    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location

    call memAddressToBlock                      ; Convert address to block index
    add di, cx                                  ; Add block index to offset

    mov byte [es:di], 0x00                      ; Set the bit to free (0x0)
    dec word [loMemUsedBlocks]                  ; Decrease the used low memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    
;---------------------------------------------------
allocMemAddress:
;
; Allocate an memory address in size of bytes.
;
; Expects: AX:DX = Segment:Offset
;             BX = Size in bytes
;
; Returns:    CF = Set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds
    
    mov cx, cs
    mov ds, cx
    
    mov di, word [loMemMapSeg]                  ; Set segment to memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set offset into memory map
    call memAddressToBlock
    call memBytesToBlocks
    
    mov dx, ax                                  ; Size to allocate
    mov ax, cx                                  ; Restore the bitmap index
    
    mov cx, word [loMemMaxBlocks]
    cmp ax, cx                                  ; See if the size wanted to allocate
    jg .error                                   ; is larger than the maximum blocks

    sub cx, word [loMemUsedBlocks]
    cmp cx, 0                                   ; See if their are no more available
    jle .error                                  ; memory blocks

    add di, ax                                  ; Add to the bitmap offset from value in ax 
    
    push dx                                     ; Store the number of blocks to allocate
    push di                                     ; Store the index into the bitmap
    
  .checkBitmap:
    or dx, dx                                   ; When dx is empty, memory is able to be allocated
    jz .checkPassed
    mov al, byte [es:di]                        ; Grab a byte from es:di
    cmp al, 0x0f                                ; Check for used block (0x0f)
    je .errorLoop 
    inc di                                      ; Increase segment offset
    dec dx                                      ; Decrease the ammount of blocks to allocate
    jmp .checkBitmap

  .checkPassed:
    pop di                                      ; Restore the index into the bitmap
    pop dx                                      ; Restore the number of blocks to allocate

    mov cx, dx                                  ; Move the number of blocks to the counter reg
  .setBits:
    mov byte [es:di], 0x0f                      ; Set the bit to used (0x0f)
    inc word [loMemUsedBlocks]                  ; Increase the used low memory
    inc di                                      ; Increase segment offset
    loop .setBits

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry on no error
    ret
    
  .errorLoop:
    pop di                                      ; Restore pushed registers before loop
    pop dx
    
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    stc                                         ; Set carry on error
    ret

