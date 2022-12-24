;  disk.asm
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

bpbBuffer:
    bootJump          times 3 db 0              ; Jump instruction over the OEM / BIOS param block
    OEMName           times 8 db 0              ; Disk label
    bytesPerSector    dw 0                      ; Bytes per sector
    sectorsPerCluster db 0                      ; Sectors per cluster
    reservedSectors   dw 0                      ; Reserved sectors
    fats              db 0                      ; Number of fats
    rootDirEntries    dw 0                      ; Number of entries in root dir
    sectors           dw 0                      ; Logical sectors
    mediaType         db 0                      ; Media descriptor byte
    fatSectors        dw 0                      ; Sectors per FAT
    sectorsPerTrack   dw 0                      ; Sectors per track
    heads             dw 0                      ; Number of sides/heads
    hiddenSectors     dd 0                      ; Hidden sectors
    hugeSectors       dd 0                      ; LBA sectors
    driveNum          db 0                      ; Drive number
    reserved          db 0                      ; This is not used
    bootSignature     db 0                      ; Drive signature
    volumeId          dd 0                      ; Volume ID
    volumeLabel       times 11 db 0             ; Volume Label
    fatTypeLabel      times 8 db 0              ; File system type

    rootDirSize       dw 0
    rootDirSector     dw 0
    startOfData       dw 0
    totalClusters     dw 0
    sectorsPerFat     dw 0
    bytesPerCluster   dd 0
    drive             db 0
    cwdCluster        dw 0
    cwdSEG            dw 0
    cwdOFF            dw 0
    fatSEG            dw 0
    fatOFF            dw 0

;---------------------------------------------------
setupDisk:
;
; Setup values for the bios paramater block.
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
    push es
    push ds

    clc                                         ; Clear carry flag
    int 0x12                                    ; Get Conventional memory size in kb
    jc .error

    mov cl, 6                                   ; Shift bits left (ax*(2^6))
    shl ax, cl                                  ; Convert the memory to 16-byte paragraphs

    sub ax, 512 >> 4                            ; Reserved 512 bytes for the boot sector
    mov ds, ax                                  ; Set the data segment register to the boot sector location
    xor si, si                                  ; Now ds:si points to the bs

    mov dl, byte [cs:drive]
    cmp dl, 4                                   ; When booting from a hard drive, you must
    jb .continue                                ; call int 13h to fix some bpb entries

    push es
    mov ah, 0x08                                ; Get Drive Parameters func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jc .error
    pop es

    and cx, 0x003f                              ; Maximum sector number is the high bits 6-7 of cl
    mov word [ds:si+0x18], cx                   ; And whose low 8 bits are in ch
    xor bx, bx                                  ; Convert the maximum head number to a word
    mov bl, dh
    inc bx                                      ; Head numbers start at zero, so add one
    mov word [ds:si+0x1a], bx                   ; Save the head number

  .continue:
    cld                                         ; Clear the direction flag

    mov bx, cs
    mov di, bpbBuffer                           ; Set es:di to the bpb buffer
    mov es, bx

    mov cx, 61
    rep movsb                                   ; Copy 61 bytes from ds:si to es:di to fill the table

    xor dx, dx
    mov ax, 32                                  ; Size of root dir = (rootDirEntries * 32) / bytesPerSector
    mul word [cs:rootDirEntries]                ; Multiply by the total size of the root directory
    div word [cs:bytesPerSector]                ; Divided by the number of bytes used per sector
    xor cx, cx
    xchg cx, ax

    mov al, byte [cs:fats]                      ; Location of root dir = (fats * fatSectors) + reservedSectors
    mul word [cs:fatSectors]                    ; Multiply by the sectors used
    add ax, word [cs:reservedSectors]           ; Increase ax by the reserved sectors
  
    mov word [cs:rootDirSize], cx               ; Root dir size
    mov word [cs:rootDirSector], ax             ; Starting sector of the root dir

    mov word [cs:startOfData], ax               ; Start of the user data area 
    add word [cs:startOfData], cx

    xor dx, dx
    xor ah, ah                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [cs:fats]                      ; Move number of fats into al
    mul word [cs:fatSectors]                    ; Move fat sectors into bx
    mov word [cs:sectorsPerFat], ax

    xor dx, dx
    mov ax, word [cs:reservedSectors]           ; Total fat clusters = (sectors - startOfUserData) / sectorsPerCluster
    mov bx, word [cs:sectors]                   ; Take the total sectors subtracted
    sub bx, word [cs:startOfData]               ; by the start of the data sectors
    div word [cs:sectorsPerCluster]             ; Now divide by the sectors per cluster
    mov word [cs:totalClusters], ax

    xor dx, dx
    mov ax, word [cs:bytesPerSector]
    mov bl, byte [cs:sectorsPerCluster]
    xor bh, bh
    mul bx
    mov word [cs:bytesPerCluster], ax
    mov word [cs:bytesPerCluster+2], dx

    call logBpb

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .error:
    pop es

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;---------------------------------------------------
readSectors:
;
; Read sectors starting at a given sector by 
; the given times and load into a buffer. 
;
; Expects: ES:DI = Location of data
;          AX:DX = Starting sector/ lba
;          CX    = Number of sectors to read      
;
; Returns: CF    = Carry flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov bx, cs                                  ; Ensure corret data segment
    mov ds, bx

    mov bx, di                                  ; Set disk buffer offset to bx

  .sectorLoop:
    push ax
    push cx
    push dx

    push bx                                     ; Save disk buffer offset

    ;xor dx, dx                                 ; This allows us to access even more sectors!

    div word [sectorsPerTrack]                  ; Divide the lba (value in ax:dx) by sectorsPerTrack
    mov cx, dx                                  ; Save the absolute sector value 
    inc cx

    xor dx, dx                                  ; Divide by the number of heads
    div word [heads]                            ; to get absolute head and track values
    mov dh, dl                                  ; Save the absolute head to bh

    mov ch, al                                  ; Low 8 bits of absolute track
    shl ah, 1                                   ; High 2 bits of absolute track
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    or cl, ah                                   ; Now cx is set with respective track and sector numbers
 
    mov dl, byte [drive]                        ; Set correct drive for int 13h
    pop bx                                      ; Restore disk buffer offset
    
    mov di, 5                                   ; Try five times to read the sector

  .attemptRead:
    mov ax, 0x0201                              ; Read Sectors func of int 13h, read one sector
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jnc .readOk                                 ; If no carry set, the sector has been read

    xor ah, ah                                  ; Reset Drive func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)

    dec di                                      ; Decrease read attempt counter
    jnz .attemptRead                            ; Try to read the sector again

    jmp .readError

  .readOk:
    pop dx
    pop cx
    pop ax

    clc
    inc ax                                      ; Increase the next sector to read
    add bx, word [bytesPerSector]               ; Add to the buffer address for the next sector
    jnc .nextSector 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when bx overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .nextSector:
    loop .sectorLoop                            ; Read the next sector for cx times

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .readError:                                   ; Error while reading a sector
    pop dx
    pop cx
    pop ax

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry flag on error
    ret

;---------------------------------------------------
writeSectors:
;
; Write sectors starting at a given sector by 
; the given number, writes data from the buffer to disk.
;
; Expects: ES:DI = Location of data
;          AX:DX = Starting sector/ lba
;          CX    = Number of sectors to write     
;
; Returns: CF    = Carry flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov bx, cs                                  ; Ensure corret data segment
    mov ds, bx

    mov bx, di                                  ; Set disk buffer offset to bx

  .sectorLoop:
    push ax
    push cx
    push dx

    push bx                                     ; Save disk buffer offset
    
    ;xor dx, dx                                 ; This will allow us to access more sectors!
    
    div word [sectorsPerTrack]                  ; Divide the lba (value in ax:dx) by sectorsPerTrack
    mov cx, dx                                  ; Save the absolute sector value 
    inc cx

    xor dx, dx                                  ; Divide by the number of heads
    div word [heads]                            ; to get absolute head and track values
    mov bh, dl                                  ; Save the absolute head to bh

    mov ch, al                                  ; Low 8 bits of absolute track
    shl ah, 1                                   ; High 2 bits of absolute track
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    or cl, ah                                   ; Now cx is set with respective track and sector numbers
 
    mov dl, byte [drive]                        ; Set correct drive for int 13h
    mov dh, bh                                  ; Move the absolute head into dh
    pop bx                                      ; Restore disk buffer offset

    mov di, 5                                   ; Try five times to write the sector

  .attemptWrite:
    mov ax, 0x0301                              ; Write sectors func of int 13h, write one sector
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jnc .writeOk                                 ; If no carry set, the sector has been written
  
    xor ah, ah                                  ; Reset Drive func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)

    dec di                                      ; Decrease write attempt counter
    jnz .attemptWrite                           ; Try to write the sector again

    jmp .writeError

  .writeOk:
    pop dx
    pop cx
    pop ax

    clc
    inc ax                                      ; Increase the next sector to write
    add bx, word [bytesPerSector]               ; Add to the buffer address for the next sector
    jnc .nextSector 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when bx overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .nextSector:
    loop .sectorLoop                            ; Write the next sector for cx times

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret
    
  .writeError:                                   ; Error while writing a sector
    pop dx
    pop cx
    pop ax

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry flag on error
    ret

;--------------------------------------------------
loadCwd:
;
; This allocates and loads the current working
; directory into memory.
;
; Expects: Nothing
;
; Returns: ES:DI = Address of current working dir
;          CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax

    call allocCwd                               ; Allocate the current working dir into memory
    jc .memError

    call readCwd                                ; Read the current working directory
    jc .readError

    mov ax, es
    mov dx, di
    mov word [cwdSEG], ax                       ; Save current working dir segment
    mov word [cwdOFF], dx                       ; Save current working dir offset

    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .readError:
    call unloadCwd                              ; Free the dir from memory

  .memError:
    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
unloadCwd:
;
; This unallocates the dir from memory.
;
; Expects: Nothing
;
; Returns: Nothing
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax
    
    mov ax, word [cwdSEG]                       ; Get the dir segment
    mov es, ax
    mov di, word [cwdOFF]                       ; Get the dir offset
    call freeCwd                                ; Free the dir from memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;--------------------------------------------------
readCwd:
;
; Load the current working directory into memory.
;
; Expects: ES:DI = Disk buffer
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax

    mov bx, word [cwdCluster]
    cmp bx, 0                                   ; When cwd cluster is zero, its the root dir
    jne .dir                                    ; This is because you cannot have a cluster below 2

  .root:
    xor dx, dx
    mov cx, word [rootDirSize]                  ; Read the size in sectors of the directory
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call readSectors                            ; Read the sectors 
    jc .readError
    jmp .done

  .dir:
    mov ax, bx                                  ; Cwd cluster
    call readClusters                           ; Read the cluster
    jc .readClustError

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .readClustError:
  .readError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
writeCwd:
;
; Write the current working directroy to the disk.
;
; Expects: ES:DI = Disk buffer
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax

    mov bx, word [cwdCluster]
    cmp bx, 0                                   ; When cwd cluster is zero, its the root dir
    jne .cwd
    
  .root:
    xor dx, dx
    mov cx, word [rootDirSize]                  ; Size of the directory in sectors
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call writeSectors                           ; Write sectors the sectors
    jc .writeError
    jmp .done

  .cwd:
    mov ax, bx                                  ; Cwd cluster
    mov bx, es
    mov cx, di                                  ; Save the location of the disk buffer

    call removeClusters                         ; Remove the directorys cluster
    jc .removeClustError

    mov es, bx
    mov di, cx

    xor dx, dx
    xor ax, ax
    mov ax, word [es:di+FAT_DIR_CLUSTER_LO]

  .search:
    cmp byte [es:di], 0x00                      ; Empty entry Marker
    je .write

  .nextEntry:
    clc
    add di, 32                                  ; Add 32, this points to the next entry
    add dx, 32
    jnc .search
    
  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

    jmp .search

  .write:
    mov es, bx                                  ; Set the buffer back to the start of the dir
    mov di, cx

    xor ax, ax
    call writeClusters                          ; Now write the new clusters to the disk

    mov word [cwdCluster], cx

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret
  
  .removeClustError:
  .writeError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
allocCwd:
;
; Allocate the current dir into the memory map.
; NOTE: THIS DOES NOT READ THE DIR INTO MEMORY.
;
; Expects: Nothing
;
; Returns: ES:DI = Address of root dir
;          CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push ds

    mov ax, cs
    mov ds, ax

    xor dx, dx
    mov ax, 32                                  ; Size of root dir in bytes = (rootDirEntries * 32)
    mul word [rootDirEntries]                   ; Multiply by the total size of the root directory
    xchg ax, dx

    call memAllocBytes                          ; Allocate memory
    jc .memError                                ; Out of memory

    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .memError:
    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
freeCwd:
;
; Free the current working dir from use in the memory map.
;
; Expects: ES:DI = Address of dir
;
; Returns: Nothing
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push ds

    mov ax, cs
    mov ds, ax

    xor dx, dx
    mov ax, 32                                  ; Size of root dir in bytes = (rootDirEntries * 32)
    mul word [rootDirEntries]                   ; Multiply by the total size of the root directory
    xchg ax, dx

    call memFreeBytes                           ; Free memory

    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;--------------------------------------------------
loadFat:
;
; This allocates and loads the FAT into memory.
;
; Expects: Nothing
;
; Returns: ES:DI = Address of FAT
;          CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax

    call allocFat                               ; Allocate the FAT into memory
    jc .memError

    call readFat                                ; Now read the FAT
    jc .readError

    mov ax, es
    mov dx, di
    mov word [fatSEG], ax                       ; Save root segment
    mov word [fatOFF], dx                       ; Save root offset

    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .readError:
    call freeFat                                ; Free the FAT from memory

  .memError:
    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
unloadFat:
;
; This unallocates the FAT from memory.
;
; Expects: Nothing
;
; Returns: Nothing
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax

    mov ax, word [fatSEG]                       ; Get the FAT segment
    mov es, ax
    mov di, word [fatOFF]                       ; Get the FAT offset
    call freeFat                                ; Free the FAT from memory

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
readFat:
;
; Read the fat into memory.
;
; Expects: ES:DI = Disk buffer
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs                                  ; Ensure correct data segment
    mov ds, dx

    xor dx, dx
    xor ah, ah                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [fats]                         ; Move number of fats into al
    mul word [fatSectors]                       ; Move fat sectors into bx
    xor cx, cx
    mov cx, ax                                  ; Store in cx

    mov ax, word [reservedSectors]              ; Convert the first fat on the disk
    call readSectors                            ; Read the sectors
    jc .readError

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
   
    clc                                         ; Clear carry, for no error
    ret

  .readError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;---------------------------------------------------
writeFat:
;
; Write the fat im memory to disk.
;
; Expects: ES:DI = Location of FAT
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push ds

    mov dx, cs                                  ; Ensure correct data segment
    mov ds, dx

    xor dx, dx
    xor ah, ah                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [fats]                         ; Move number of fats into al
    mul word [fatSectors]                       ; Move fat sectors into bx
    xor cx, cx
    mov cx, ax                                  ; Store in cx

    mov ax, word [reservedSectors]              ; Convert the first fat on the disk
    call writeSectors                           ; Write the sectors
    jc .writeError

    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeError:
    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
allocFat:    
;
; Allocate the FAT into the memory map.
; NOTE: THIS DOES NOT READ FAT INTO MEMORY.
;
; Expects: Nothing
;
; Returns: ES:DI = Address of FAT
;          CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push ds

    mov ax, cs
    mov ds, ax

    xor dx, dx
    xor ah, ah                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [fats]                         ; Move number of fats into al
    mul word [fatSectors]                       ; Move fat sectors into bx
    xor dx, dx
    mul word [bytesPerSector]                   ; Divided by the number of bytes used per sector   
    xchg ax, dx

    call memAllocBytes                          ; Allocate memory
    jc .memError                                ; Out of memory

    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .memError:
    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------
freeFat:
;
; Allocate the FAT into the memory map.
;
; Expects: ES:DI = Address of FAT
;
; Returns: Nothing
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, cs
    mov ds, ax

    xor dx, dx
    xor ah, ah                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [fats]                         ; Move number of fats into al
    mul word [fatSectors]                       ; Move fat sectors into bx
    xor dx, dx
    mul word [bytesPerSector]                   ; Divided by the number of bytes used per sector   
    xchg ax, dx

    call memFreeBytes                           ; Free memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;--------------------------------------------------
searchDir:
;
; Search through the allready loaded dir for
; a file in 8.3 format.
;
; Expects: DS:SI = Filename in 8.3 format
;          ES:DI = Disk buffer loaded with root dir
;
; Returns: ES:DI = Pointer to the file entry
;          CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax
    push bx
    push cx
    push dx
    push si

  .search:
    mov al, byte [es:di]                        ; Grab the first byte of the entry

    cmp al, 0x00                                ; Empty entry Marker
    je .fileNotFound
    cmp al, 0xe5                                ; Free entry marker
    je .nextEntry

    mov bx, si
    mov dx, di

    cld                                         ; Clear direction flag
    mov cx, 11                                  ; Compare first 11 bytes
    rep cmpsb                                   ; Compare ds:si and es:di cx times
    je .fileFound                               ; We found the file :)

    mov di, dx
    mov si, bx

  .nextEntry:
    clc
    add di, 32                                  ; Add 32, this points to the next entry 
    jnc .search

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

    jmp .search

  .fileFound:
    mov di, dx

    pop si                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, file found
    ret

  .fileNotFound:     
    pop si                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, file not found
    ret

;--------------------------------------------------  
changeDir:
; 
; Attempt to change the current working directory
;
; Expects: DS:SI = Directory to search for
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Output string
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the dir
    jc .loadDirError

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov ax, word [es:di+FAT_DIR_CLUSTER_LO]     ; Get the file cluster
    mov bl, byte [es:di+FAT_DIR_ATTRIBUTES]     ; Get the file atribute byte

    call unloadCwd                              ; Free the dir from memory

    cmp bl, 0x10                                ; Check to see if its a directory
    jne .notDir

    mov word [cwdCluster], ax                   ; If it is a directory, set the cwd custer

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
    call unloadCwd                              ; Free the dir from memory

  .notDir:    
  .loadDirError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .tmpName times 12 db 0x00

;--------------------------------------------------  
createDir:
;
; Create an empty directory.
;
; Expects: DS:SI = Dir to create
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    call fileExists                             ; Check to see if the file allready exists
    jnc .existsError

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)
    
    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call fatFmtTimeAndDate
    mov word [.time], ax
    mov word [.date], dx

    mov ax, 0x0000
    mov dx, 0x0200
    call roundFilesize
    call memAllocBytes
    jc .memoryError

    push ax
    push dx

    mov ax, word [.time]
    mov dx, word [.date]

    mov byte [es:di+FAT_DIR_FILENAME], '.'
    mov word [es:di+FAT_DIR_FILENAME+1], 0x2020 ; Pad filename and ext with spaces
    mov word [es:di+FAT_DIR_FILENAME+3], 0x2020
    mov word [es:di+FAT_DIR_FILENAME+5], 0x2020
    mov word [es:di+FAT_DIR_FILENAME+7], 0x2020
    mov word [es:di+FAT_DIR_FILENAME+9], 0x2020
    mov byte [es:di+FAT_DIR_ATTRIBUTES], 0x10   ; File attribtutes
    mov byte [es:di+FAT_DIR_RESERVED], 0x00     ; Reserved for Windows NT usage
    mov byte [es:di+FAT_DIR_CREATION_MS], 0x00  ; 10-millisecond units past creation time below
    mov word [es:di+FAT_DIR_CREATION_TIME], ax  ; Time of the file created
    mov word [es:di+FAT_DIR_CREATION_DATE], dx  ; Date of the file created
    mov word [es:di+FAT_DIR_ACCESSED_DATE], dx  ; Date of last access to the file
    mov word [es:di+FAT_DIR_CLUSTER_HI], 0x0000 ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [es:di+FAT_DIR_MODIFIED_TIME], ax  ; Time of the file last modified
    mov word [es:di+FAT_DIR_MODIFIED_DATE], dx  ; Date of the file last modified
    mov word [es:di+FAT_DIR_CLUSTER_LO], 0x0000 ; Low starting cluster
    mov word [es:di+FAT_DIR_FILESIZE], 0x0000   ; Size of the file
    mov word [es:di+FAT_DIR_FILESIZE+2], 0x0000 ;

    add di, 32
    mov cx, word [cwdCluster]

    mov word [es:di+FAT_DIR_FILENAME], '..'
    mov word [es:di+FAT_DIR_FILENAME+2], 0x2020 ; Pad filename and ext with spaces
    mov word [es:di+FAT_DIR_FILENAME+4], 0x2020
    mov word [es:di+FAT_DIR_FILENAME+6], 0x2020
    mov word [es:di+FAT_DIR_FILENAME+8], 0x2020
    mov byte [es:di+FAT_DIR_FILENAME+10], 0x20
    mov byte [es:di+FAT_DIR_ATTRIBUTES], 0x10   ; File attribtutes
    mov byte [es:di+FAT_DIR_RESERVED], 0x00     ; Reserved for Windows NT usage
    mov byte [es:di+FAT_DIR_CREATION_MS], 0x4c  ; 10-millisecond units past creation time below
    mov word [es:di+FAT_DIR_CREATION_TIME], ax  ; Time of the file created
    mov word [es:di+FAT_DIR_CREATION_DATE], dx  ; Date of the file created
    mov word [es:di+FAT_DIR_ACCESSED_DATE], dx  ; Date of last access to the file
    mov word [es:di+FAT_DIR_CLUSTER_HI], 0x0000 ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [es:di+FAT_DIR_MODIFIED_TIME], ax  ; Time of the file last modified
    mov word [es:di+FAT_DIR_MODIFIED_DATE], dx  ; Date of the file last modified
    mov word [es:di+FAT_DIR_CLUSTER_LO], cx     ; Low starting cluster
    mov word [es:di+FAT_DIR_FILESIZE], 0x0000   ; Size of the file
    mov word [es:di+FAT_DIR_FILESIZE+2], 0x0000 ; 

    mov ax, 0x0000
    mov dx, 0x0200
    sub di, 32
    call writeClusters                          ; Now write the two dir entrys to the disk

    pop dx
    pop ax
    call memFreeBytes

    call loadCwd                                ; Allocate and read the dir
    jc .loadDirError

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

  .search:
    mov al, byte [es:di]                        ; Grab the first byte of the entry

    cmp al, 0x00                                ; Empty entry Marker
    je .freeEntry
    cmp al, 0xe5                                ; Free entry marker
    je .freeEntry

    clc
    add di, 32                                  ; Point to the next entry
    jnc .nextEntry

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .nextEntry:
    jmp .search

  .freeEntry:
    push cx
    cld                                         ; Clear direction flag
    mov si, .tmpName                            ; Get the new filename
    mov cx, 11                                  ; Length of filename
    rep movsb                                   ; Copy bytes from ds:si to es:di 
    pop cx

    sub di, 11

    push ax
    push dx

    mov ax, word [.time]
    mov dx, word [.date]

    mov byte [es:di+FAT_DIR_ATTRIBUTES], 0x10   ; File attribtutes
    mov byte [es:di+FAT_DIR_RESERVED], 0x00     ; Reserved for Windows NT usage
    mov byte [es:di+FAT_DIR_CREATION_MS], 0x4c  ; 10-millisecond units past creation time below
    mov word [es:di+FAT_DIR_CREATION_TIME], ax  ; Time of the file created
    mov word [es:di+FAT_DIR_CREATION_DATE], dx  ; Date of the file created
    mov word [es:di+FAT_DIR_ACCESSED_DATE], dx  ; Date of last access to the file
    mov word [es:di+FAT_DIR_CLUSTER_HI], 0x0000 ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [es:di+FAT_DIR_MODIFIED_TIME], ax  ; Time of the file last modified
    mov word [es:di+FAT_DIR_MODIFIED_DATE], dx  ; Date of the file last modified
    mov word [es:di+FAT_DIR_CLUSTER_LO], cx     ; Low starting cluster
    mov word [es:di+FAT_DIR_FILESIZE], 0x0000   ; Size of the file
    mov word [es:di+FAT_DIR_FILESIZE+2], 0x0000 ; 

    pop dx
    pop ax

    mov es, bx
    mov di, dx

    call writeCwd                               ; Finally, write it to the disk
    jc .writeDirError

    call unloadCwd                              ; Free the dir from memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeDirError:
    call unloadCwd                              ; Free the dir from memory

  .memoryError:
  .loadDirError:
  .existsError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .date dw 0
  .time dw 0
  .tmpName times 12 db 0x00

;--------------------------------------------------  
removeDir:
; 
; Attempt to remove an empty directory
;
; Expects: DS:SI = Directory to remove
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Output string
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the root dir
    jc .loadDirError

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov cx, word [es:di+FAT_DIR_CLUSTER_LO]     ; Get the file cluster
    mov al, byte [es:di+FAT_DIR_ATTRIBUTES]     ; Get the file atribute byte

    cmp al, 0x10                                ; Check to see if its a directory
    jne .notDir

    mov byte [es:di], 0xe5                      ; Mark file entry as deleted

    mov es, bx
    mov di, dx
    call writeCwd

    call unloadCwd                              ; Free the dir from memory

    mov ax, cx
    call removeClusters

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
  .notDir:
    call unloadCwd                              ; Free the dir from memory
    
  .loadDirError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .tmpName times 12 db 0x00

;--------------------------------------------------  
fileSize:
; 
; Search for a file and return its size.
;
; Expects: DS:SI = Filename to search for
;
; Returns: AX:DX = Filesize
;          CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push bx                                     ; Save registers
    push cx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Output string
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the root dir
    jc .error

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    cmp word [es:di+FAT_DIR_ATTRIBUTES], 0x10   ; Check to see if its a directory
    je .fileNotFound

    mov dx, word [es:di+FAT_DIR_FILESIZE]       ; Get the size of the file
    mov ax, word [es:di+FAT_DIR_FILESIZE+2]

    call unloadCwd                              ; Free the dir from memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
    call unloadCwd                              ; Free the dir from memory

  .error:
    xor ax, ax
    xor dx, dx

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    stc                                         ; Set carry, error occured
    ret

 .tmpName times 12 db 0x00

;--------------------------------------------------  
fileExists:
; 
; Load the cwd and search for a file to see
; if it exists.
;
; Expects: DS:SI = Filename to search for
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Output string
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the root dir
    jc .error

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound
    
    call unloadCwd                              ; Free the dir from memory
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
    call unloadCwd                              ; Free the dir from memory

  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .tmpName times 12 db 0x00

;--------------------------------------------------  
createFile:
;
; Create an empty file in the root directory.
;
; Expects: DS:SI = Filename to create
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    call fileExists                             ; Check to see if the file allready exists
    jnc .existsError                            ; Note: stores/converts filename and loads root dir

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the root dir
    jc .error

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov cx, word [rootDirEntries]
    
  .search:
    mov al, byte [es:di]                        ; Grab the first byte of the entry
    
    cmp al, 0x00                                ; Empty entry Marker
    je .freeEntry
    cmp al, 0xe5                                ; Free entry marker
    je .freeEntry

    clc
    add di, 32                                  ; Point to the next entry
    jnc .nextEntry

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .nextEntry:
    loop .search
    jmp .noFileEntrys

  .freeEntry:
    cld                                         ; Clear direction flag
    mov si, .tmpName                            ; Get the new filename
    mov cx, 11                                  ; Length of filename
    rep movsb                                   ; Copy bytes from ds:si to es:di 

    sub di, 11

    push ax
    push dx

    call fatFmtTimeAndDate

    mov byte [es:di+FAT_DIR_ATTRIBUTES], 0x00   ; File attribtutes
    mov byte [es:di+FAT_DIR_RESERVED], 0x00     ; Reserved for Windows NT usage
    mov byte [es:di+FAT_DIR_CREATION_MS], 0x00  ; 10-millisecond units past creation time below
    mov word [es:di+FAT_DIR_CREATION_TIME], ax  ; Time of the file created
    mov word [es:di+FAT_DIR_CREATION_DATE], dx  ; Date of the file created
    mov word [es:di+FAT_DIR_ACCESSED_DATE], dx  ; Date of last access to the file
    mov word [es:di+FAT_DIR_CLUSTER_HI], 0x0000 ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [es:di+FAT_DIR_MODIFIED_TIME], ax  ; Time of the file last modified
    mov word [es:di+FAT_DIR_MODIFIED_DATE], dx  ; Date of the file last modified
    mov word [es:di+FAT_DIR_CLUSTER_LO], 0x0000 ; Low starting cluster
    mov word [es:di+FAT_DIR_FILESIZE], 0x0000   ; Size of the file
    mov word [es:di+FAT_DIR_FILESIZE+2], 0x0000 ; 

    pop dx
    pop ax

    mov es, bx
    mov di, dx

    call writeCwd                               ; Finally, write it to the disk
    jc .writeDirError

    call unloadCwd                              ; Free the dir from memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .noFileEntrys:
  .writeDirError:
    call unloadCwd                              ; Free the dir from memory

  .error:
  .existsError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .tmpName times 12 db 0x00

;--------------------------------------------------  
renameFile:
;
; Rename a file in the cwd.
;
; Expects: DS:SI = Filename to change
;          ES:DI = New filename
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    push di
    mov di, .tmpName1                           ; Old filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    pop si
    mov di, .tmpName2                           ; New filename 
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the dir
    jc .error

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName1                           ; Search for the file to rename
    call searchDir
    jc .fileNotFound

    cld                                         ; Clear direction flag
    mov si, .tmpName2                           ; Get the new filename
    mov cx, 11                                  ; Lenght of filename
    rep movsb                                   ; Copy bytes from ds:si to es:di 

    mov es, bx                                  ; Set the current dir offset back
    mov di, dx
  
    call writeCwd                               ; Finally, write it to the disk
    jc .writeDirError

    call unloadCwd                              ; Free the dir from memory

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
  .writeDirError:
    call unloadCwd                              ; Free the dir from memory

  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .tmpName1 times 12 db 0x00
  .tmpName2 times 12 db 0x00

;--------------------------------------------------  
deleteFile:
;
; Delete a file in the cwd and removes
; all fat clusters relating to the file.
;
; Expects: DS:SI = Filename to remove
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)

    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    call loadCwd                                ; Allocate and read the dir
    jc .loadDirError

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file to delete
    call searchDir
    jc .fileNotFound

    mov byte [es:di], 0xe5                      ; Mark file entry as deleted
    mov ax, word [es:di+FAT_DIR_FILESIZE]       ; Size in bytes of the file
    mov si, word [es:di+FAT_DIR_FILESIZE+2]     ; Size in bytes of the file
    mov cx, word [es:di+FAT_DIR_CLUSTER_LO]     ; File cluster number

    mov es, bx
    mov di, dx

    call writeCwd                               ; Finally, write it to the disk
    jc .writeError1

    call unloadCwd                              ; Free the dir from memory

    mov dx, si
    test ax, ax                                 ; Check to see if the file is empty
    jnz .fileNotZero
    test dx, dx
    jz .done

  .fileNotZero:
    mov ax, cx
    call removeClusters
    jc .writeError2

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeError2:
    call unloadFat                              ; Free the Fat from memory
    jmp .loadFatError

  .fileNotFound:
  .writeError1:
    call unloadCwd                              ; Free the dir from memory

  .loadDirError:
  .loadFatError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    stc                                         ; Set carry, error occured

    ret

  .tmpName times 12 db 0x00

;--------------------------------------------------  
readFile:
;
; Read a file from disk and into memory.
;
; Expects: DS:SI = Filename to read
;          ES:DI = Location to load file
;
; Returns: DX:AX = Filesize
;             CF = Carry Flag set on error
;
;--------------------------------------------------  
    push bx                                     ; Save registers
    push cx
    push si
    push di
    push es
    push ds

    push es
    push di

    mov dx, cs
    mov es, dx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)
 
    mov dx, cs
    mov ds, dx                                  ; Now we refrence .tmpName through ds

    pop word [.loadOFF]
    pop word [.loadSEG]

    call loadCwd                                ; Allocate and read the dir
    jc .loadDirError

    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov ax, word [es:di+FAT_DIR_CLUSTER_LO]     ; File cluster number
    mov cx, word [es:di+FAT_DIR_FILESIZE]       ; Get the size of the file
    mov dx, word [es:di+FAT_DIR_FILESIZE+2]

    call unloadCwd                              ; Free the dir from memory

    cmp ax, 0
    je .done

    mov di, word [.loadSEG]
    mov es, di
    mov di, word [.loadOFF]

    call readClusters
    jc .readError

    mov ax, cx
    xchg ax, dx
    
  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    clc                                         ; Clear carry, for no error
    ret
     
  .fileNotFound:
    call freeCwd                                ; Free the dir from memory

  .loadDirError:
  .readError:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx

    stc                                         ; Set carry, error occured
    ret

  .loadSEG dw 0
  .loadOFF dw 0
  .tmpName times 12 db 0x00

;--------------------------------------------------  
writeFile:
;
; Create an empty file in the root directory.
;
; Expects: DS:SI = Filename to read
;          ES:DI = Location of data
;          DX:AX = Filesize
;
; Returns:    CF = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    push es
    push di

    mov cx, cs
    mov es, cx                                  ; Set correct data segment for local var

    mov di, .tmpName                            ; Filename
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)
 
    mov cx, cs
    mov ds, cx                                  ; Now we refrence .tmpName through ds

    mov word [.loFilesize], ax                  ; Save the lo word of the filesize
    mov word [.hiFilesize], dx                  ; Save the hi word of the filesize

    mov si, .tmpName 
    call createFile                             ; Create the file entry
    jc .createFileError

    test ax, ax                                 ; Check to see if the file is empty
    jnz .fileExists
    test dx, dx
    jnz .fileExists

    pop di
    pop es

    jmp .fileZero

  .fileExists:
    pop di
    pop es
    mov ax, word [.loFilesize]
    mov dx, word [.hiFilesize]
    call writeClusters

    call loadCwd                                ; Allocate and read the dir
    jc .loadDirError
    
    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file to rename
    call searchDir

    push dx
    mov ax, word [.loFilesize]
    mov dx, word [.hiFilesize]

    xchg ax, dx
    mov word [es:di+FAT_DIR_CLUSTER_LO], cx
    mov word [es:di+FAT_DIR_FILESIZE], ax
    mov word [es:di+FAT_DIR_FILESIZE+2], dx
    pop dx

    mov es, bx
    mov di, dx

    call writeCwd                               ; Finally, write it to the disk
    jc .writeDirError

    call unloadCwd                              ; Free the dir from memory

  .fileZero:   
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeDirError:
    call unloadCwd                              ; Free the dir from memory
    jmp .loadDirError

  .createFileError:
    pop di
    pop es

  .loadDirError: 
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .loFilesize dw 0
  .hiFilesize dw 0
  .tmpName times 12 db 0x00

;---------------------------------------------------
roundFilesize:
;
; Round the filesize bassed on bytes.
;
; Expects: DX:AX = Filesize
;
; Returns: DX:AX = Filesize rounded
;
;---------------------------------------------------
    push bx
    push cx
    push ds

    push cs
    pop ds

    push ax
    push dx
    
    xor dx, dx
    xor bh, bh                                  ; Calculate the size in bytes per cluster
    mov ax, word [bytesPerSector]               ; So, take the bytes per sector
    mov bl, byte [sectorsPerCluster]            ; and mul that by the sectors per cluster
    mul bx
    
    xchg ax, bx
    xchg dx, cx

    pop ax
    pop dx
    call u32x32div
    inc ax

    mov bx, ax
    mov ax, word [bytesPerCluster]
    mov dx, word [bytesPerCluster+2]
    call u32x16mul

    xchg ax, dx
    pop ds
    pop cx
    pop bx
    ret

;---------------------------------------------------
logBpb:
;
; Log bpb information to the serial port.
;
; Expects: None
;
; Returns: None
;
;---------------------------------------------------
%ifdef DEBUG
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    push cs
    pop ds

    mov si, .str0
    call serialWriteStr

    mov si, .str1
    call serialWriteStr

    mov cx, 3
    mov si, bootJump

  .loop1:
    lodsb
    mov ah, 0
    mov bx, 16
    call serialWriteNum
    loop .loop1

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str2
    call serialWriteStr

    mov cx, 8
    mov si, OEMName

  .loop2:
    lodsb
    call serialWrite
    loop .loop2

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str3
    call serialWriteStr

    mov ax, [bytesPerSector]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str4
    call serialWriteStr

    xor ah, ah
    mov al, byte [sectorsPerCluster]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str5
    call serialWriteStr

    mov ax, word [reservedSectors]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str6
    call serialWriteStr

    xor ah, ah
    mov al, byte [fats]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str7
    call serialWriteStr

    mov ax, word [rootDirEntries]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str8
    call serialWriteStr

    mov ax, word [sectors]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str9
    call serialWriteStr

    xor ah, ah
    mov al, byte [mediaType]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str10
    call serialWriteStr

    mov ax, word [fatSectors]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str11
    call serialWriteStr

    mov ax, word [sectorsPerTrack]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str12
    call serialWriteStr

    mov ax, word [heads]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str13
    call serialWriteStr

    mov ax, word [hiddenSectors+2]
    mov dx, word [hiddenSectors]
    mov bx, 16
    call serialWriteNum32

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str14
    call serialWriteStr

    mov ax, word [hugeSectors+2]
    mov dx, word [hugeSectors]
    mov bx, 16
    call serialWriteNum32

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str15
    call serialWriteStr

    xor ah, ah
    mov al, byte [driveNum]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str16
    call serialWriteStr

    xor ah, ah
    mov al, byte [reserved]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str17
    call serialWriteStr

    xor ah, ah
    mov al, byte [bootSignature]
    mov bx, 16
    call serialWriteNum

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str18
    call serialWriteStr

    mov ax, word [volumeId+2]
    mov dx, word [volumeId]
    mov bx, 16
    call serialWriteNum32

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str19
    call serialWriteStr

    mov cx, 11
    mov si, volumeLabel

  .loop3:
    lodsb
    call serialWrite
    loop .loop3

    mov al, 13
    call serialWrite
    mov al, 10
    call serialWrite

    mov si, .str20
    call serialWriteStr

    mov cx, 8
    mov si, fatTypeLabel

  .loop4:
    lodsb
    call serialWrite
    loop .loop4

    mov al, 13
    call serialWrite
    mov al, 10
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

  .str0 db "BPB (BIOS Parameter Block)", 10, 13, 0
  .str1 db "bootJump:          0x", 0
  .str2 db "OEMName:           ", 0
  .str3 db "bytesPerSector:    0x", 0
  .str4 db "sectorsPerCluster: 0x", 0
  .str5 db "reservedSectors:   0x", 0
  .str6 db "fats:              0x", 0
  .str7 db "rootDirEntries:    0x", 0
  .str8 db "sectors:           0x", 0
  .str9 db "mediaType:         0x", 0
  .str10 db "fatSectors:        0x", 0
  .str11 db "sectorsPerTrack:   0x", 0
  .str12 db "heads:             0x", 0
  .str13 db "hiddenSectors:     0x", 0
  .str14 db "hugeSectors:       0x", 0
  .str15 db "driveNum:          0x", 0
  .str16 db "reserved:          0x", 0
  .str17 db "bootSignature:     0x", 0
  .str18 db "volumeId:          0x", 0
  .str19 db "volumeLabel:       ", 0
  .str20 db "fatTypeLabel:      ", 0
%else
    ret
%endif

