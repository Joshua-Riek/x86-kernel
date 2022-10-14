;  memory.asm
;
;  Copyright (c) 2017-2022, Joshua Riek
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

    loMemMaxBlocks  dw 0                        ; Maximum available blocks of memory
    loMemUsedBlocks dw 0                        ; Current used blocks of memory
    loMemMapSeg     dw 0                        ; Memory map segment
    loMemMapOff     dw 0                        ; Memory map offset
    kernelSize      dw 0x8000                   ; Static kernel size

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

    mov cx, ax
    xor dx, dx
    mov bx, 2                                   ; Take the lower memory size
    mul bx                                      ; and multiply, for ammount of blocks
    mov word [loMemMaxBlocks], ax

    mov ax, cx
    mov cl, 6                                   ; Shift bits left (ax*(2^6))
    shl ax, cl                                  ; Convert the memory to 16-byte paragraphs
    mov bx, word [loMemMaxBlocks]               ; Shift bits right to calculate segment offset
    mov cl, 4
    shr bx, cl
    sub ax, bx                                  ; Reserve bytes for the memory map
  
    mov es, ax                                  ; Set the extra segment register to the memory map location
    xor di, di                                  ; Now es:di points to the memory map

    mov word [loMemMapOff], di                  ; Save the bitmap location
    mov word [loMemMapSeg], es

    xor ax, ax
    mov dx, word [loMemMaxBlocks]
    xor cl, cl
    call memSet                                 ; Zero data before allocating the bitmap into memory
    call allocMemAddress                        ; Allocate the bitmap into memory
    jc .error
                                                ; Allocate the bios data area into memory
    mov ax, 0x0000                              ; Size hi
    mov dx, 0x1000                              ; Size lo
    mov di, 0x0000                              ; Offset
    mov es, di                                  ; Segment
    call allocMemAddress
    jc .error
                                                ; Allocate the stack location into memory
    mov ax, 0x0000                              ; Size hi
    mov dx, 0x1000                              ; Size lo
    mov di, 0x0f00
    mov es, di                                  ; Segment
    mov di, 0x1000                              ; Offset
    call allocMemAddress
    jc .error
                                                ; Allocate the kernel into memory
    mov ax, 0x0000                              ; Size hi
    mov dx, word [kernelSize]                   ; Size lo
    mov di, 0x0000
    mov es, di                                  ; Segment
    mov di, 0x1000                              ; Offset
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

    xor cx, cx
    xchg cx, dx

    mov bx, 512                                 ; Take the hi word in ax
    div bx                                      ; And divide by bx
    xchg ax, cx                                 ; Handle the lo word
    div bx                                      ; Divide again by bx

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
; Convert the block into a segment and offset address.
;
; Expects:    CX = Block
;
; Returns: ES:DI = Segment:Offset
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push cx
    push dx

    xor ax, ax
    mov es, ax                                  ; Clear segment output register
    mov di, ax                                  ; Clear offset output register

  .loopBlocks: 
    clc
    add di, 512                                 ; Add the block size to the offset
    jnc .nextBlock                              ; Continue to next block if no carry

    push dx
    mov dx, es
    add dh, 0x10                                ; Correct segment for 64k boundry
    mov es, dx
    pop dx

  .nextBlock:
    loop .loopBlocks 

    pop dx                                      ; Restore registers
    pop cx
    pop ax
    ret
    
;---------------------------------------------------
memAddressToBlock:
;
; Convert the segment and offset into the correct
; index into the memory bitmap.
;
; Expects: ES:DI = Segment:Offset
;
; Returns:    CX = Block number/ index
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx

    push di                                     ; Save the offset 
    mov ax, es                                  ; Deal with the segment first

    xor dx, dx                                  ; Clear remander
    mov bx, 512                                 ; Get the the block size
    div bx                                      ; Divide ax:bx
    shl ax, 1                                   ; This is a segment address so shift left
    shl ax, 1
    shl ax, 1
    shl ax, 1
    mov cx, ax                                  ; Movte into cx

    xor dx, dx                                  ; Clear remander
    pop ax                                      ; Grab the offset param
    mov bx, 512                                 ; Get the block size
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
; Returns: ES:DI = Segment:Offset
;          CF    = Set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
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
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Return carry on error
    ret

  .done:
    mov byte [es:di], 0x0f                      ; Set the bit to used (0x0f)
    inc word [loMemUsedBlocks]                  ; Increase the used low memory

    call memBlockToAddress                      ; Get the return address for ax:dx

    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; No error, so clear carry
    ret
     
;---------------------------------------------------   
memAllocBlocks:
;
; Allocate the next available blocks.
;    
; Expects: BX    = Blocks to allocate
;
; Returns: ES:DI = Segment:Offset
;          CF    = Set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
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
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

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
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; No error, so clear carry
    ret    
    
;---------------------------------------------------   
memAllocBytes:
;
; Allocate the next available blocks.
;    
; Expects: AX:DX = Size in bytes
;
; Returns: ES:DI = Segment:Offset
;          CF    = Set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx

    add dx, 0x400                               ; Add 1024 bytes to the block

    call memBytesToBlocks32
    cmp bx, 0
    jz .done
    
    call memAllocBlocks
    jc .error
    
    pop dx                                      ; Restore registers
    pop bx
    pop ax

    push cx
    mov cl, 0x00
    call memSet
    pop cx
    
  .done:
    ;call logAllocMem
  
    clc                                         ; No error, so clear carry
    ret

  .error:
    pop dx                                      ; Restore registers
    pop ax
    pop dx
    pop bx
    pop ax
    
    stc                                         ; Return carry on error
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

    call memAddressToBlock                      ; Convert address to block index

    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location
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
memFreeBlocks:
;
; Free x ammount of blocks.
;    
; Expects: ES:DI = Segment:Offset
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

    call memAddressToBlock                      ; Convert address to block index

    mov di, word [loMemMapSeg]                  ; Set correct segment for memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set correct offset for memory map location
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
memFreeBytes:
;
; Free the x ammount of bytes.
;    
; Expects: AX:DX = Size in bytes
;          ES:DI = Segment:Offset
;          
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push dx

    ;call logFreeMem

    add dx, 0x400                               ; Add 1024 bytes to the block

    call memBytesToBlocks32
    cmp bx, 0
    jz .done

    mov ax, es
    mov dx, di
    call memFreeBlocks

    pop dx                                      ; Restore registers
    pop ax

  .done:
    clc                                         ; No error, so clear carry
    ret

;---------------------------------------------------
allocMemAddress:
;
; Allocate an memory address in size of bytes.
;
; Expects: ES:DI = Segment:Offset
;          AX:DX = Size in bytes (Hi:Lo)
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

    ;call logAllocMem

    call memAddressToBlock                      ; Convert address to block/ index
    call memBytesToBlocks32                     ; Convert size in bytes to size in blocks

    mov ax, cs
    mov ds, ax

    mov di, word [loMemMapSeg]                  ; Set segment to memory map location
    mov es, di
    mov di, word [loMemMapOff]                  ; Set offset into memory map

    mov dx, bx                                  ; Size to allocate
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

;---------------------------------------------------
memSet:
;
; Expects: AX:DX = Size in bytes
;          ES:DI = Segment:Offset
;             CL = The value to fill
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

  .readBytes:
    mov byte [es:di], cl

    clc
    add di, 1                                   ; Increaese the pointer, buffer fix
    jnc .nextByte 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when bx overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .nextByte:
    dec dx

    cmp dx, 0                                   ; Decrease counter and see if we are at the end
    jne .readBytes
    cmp ax, 0
    je .done

    sub ax, 1
    mov dx, 0xffff
    jmp .nextByte

  .done:
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
logAllocMem:
;
; Log memory allocation to the serial port.
;
; Expects: ES:DI = Segment:Offset
;          AX:DX = Size in bytes (Hi:Lo)
;
; Returns: None
;
;---------------------------------------------------
    push si

    mov si, .allocrStr
    call serialWriteStr
    
    call logPrint
    
    pop si
    ret

  .allocrStr db "[+] Allocated address:   0x", 0
    
;---------------------------------------------------
logFreeMem:
;
; Log memory unallocation to the serial port.
;
; Expects: ES:DI = Segment:Offset
;          AX:DX = Size in bytes (Hi:Lo)
;
; Returns: None
;
;---------------------------------------------------
    push si
    
    mov si, .unAllocStr
    call serialWriteStr

    call logPrint
    pop si
    ret
    
  .unAllocStr  db "[-] Unallocated address: 0x", 0

;---------------------------------------------------
logPrint:
;
; Display address and size to the serial port.
;
; Expects: ES:DI = Segment:Offset
;          AX:DX = Size in bytes (Hi:Lo)
;
; Returns: None
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

    push ax
    push dx

    mov ax, es                                  ; Handle the segment addr first
    xor dx, dx                                  ; Clear remander
    mov bx, 0x10                                ; Shift left
    mul bx                                      ; Multiplying by 16 is the same as (x << 4)

    clc
    add ax, di                                  ; Add the offset addr to the segment addr
    adc dx, 0x0000                              ; Check for carry
    
    mov bx, 0x10
    mov cx, 0x0530
    call serialWriteNumPadding32                ; Display address

    mov si, .lenStr
    call serialWriteStr

    pop ax
    pop dx
    mov bx, 0x000a
    mov cx, 0x0020
    call serialWriteNumPadding32                ; Display size

    mov al, 0x0a
    call serialWrite
    mov al, 0x0d
    call serialWrite
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

  .lenStr db " => Size in Bytes: ", 0
 