;  disk.asm
;
;  This file contains BIOS disk functions.  
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
    bytesPerCluster   dw 0
    drive db 0
    cwdCluster dw 0
    rootSEG dw 0
    rootOFF dw 0
    fatSEG dw 0
    fatOFF dw 0

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
    
    xor bx, bx
    mov si, 0x7c00                              ; Set ds:si to the bootstrap code
    mov ds, bx
    
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
loadRootDir:
;
; This allocates and loads the root dir into memory.
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
    push si
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax
    
    call allocRootDir                           ; Allocate the root dir into memory
    jc .memError
    
    call readRootDir                            ; Read the root directory
    jc .readError

    mov ax, es
    mov dx, di
    mov word [rootSEG], ax                      ; Save root segment
    mov word [rootOFF], dx                      ; Save root offset

    pop ds                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret
    
  .readError:
    call unloadRootDir                          ; Free the root dir from memory

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
unloadRootDir:
;
; This unallocates the root dir from memory.
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
    
    mov ax, word [rootSEG]                      ; Get the root segment
    mov es, ax
    mov di, word [rootOFF]                      ; Get the root offset
    call freeRootDir                            ; Free the root dir from memory

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
readRootDir:
;
; Load the root directory to the appointed buffer
; from the the drive param block buffer passed.
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
writeRootDir:
;
; Write the root directory (pointed by es:di) from
; memory onto the drive.
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
    mov ax, word [es:di+dirFat.clusterLo]

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

  .dot db '.', 0
    
;--------------------------------------------------
allocRootDir:
;
; Allocate the root dir into the memory map.
; NOTE: THIS DOES NOT READ THE ROOT DIR INTO MEMORY.
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
freeRootDir:
;
; Free the root dir from use in the memory map.
;
; Expects: ES:DI = Address of root dir
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
; NOTE: THIS DOES NOT READ FAT INTO MEMORY.
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
; Search through the allready loaded root dir for
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

    call loadRootDir                            ; Allocate and read the root dir
    jc .loadDirError

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov ax, word [es:di+dirFat.clusterLo]       ; Get the file cluster
    mov bl, byte [es:di+dirFat.attributes]      ; Get the file atribute byte

    call unloadRootDir                          ; Free the root dir from memory

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
    call unloadRootDir                          ; Free the root dir from memory
    
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

    mov ax, 0x0000
    mov dx, 0x0200
    call memAllocBytes
    jc .memoryError
    
    mov byte [di+dirFat.filename],     '.'
    mov word [di+dirFat.filename+1],   0x2020   ; Pad filename and ext with spaces
    mov word [di+dirFat.filename+3],   0x2020
    mov word [di+dirFat.filename+5],   0x2020
    mov word [di+dirFat.filename+7],   0x2020
    mov word [di+dirFat.filename+9],   0x2020
    mov byte [di+dirFat.attributes],   0x10     ; File attribtutes
    mov byte [di+dirFat.reserved],     0        ; Reserved for Windows NT usage
    mov byte [di+dirFat.idk],          0x4c     ; 10-millisecond units past creation time below
    mov word [di+dirFat.creationTime], 0x6996   ; Time of the file created
    mov word [di+dirFat.creationDate], 0x5036   ; Date of the file created
    mov word [di+dirFat.accessedDate], 0x5036   ; Date of last access to the file
    mov word [di+dirFat.clusterHi],    0        ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [di+dirFat.modifiedTime], 0x6997   ; Time of the file last modified
    mov word [di+dirFat.modifiedDate], 0x5036   ; Date of the file last modified
    mov word [di+dirFat.clusterLo],    0        ; Low starting cluster
    mov word [di+dirFat.filesize],     0        ; Size of the file
    mov word [di+dirFat.filesize+2],   0        ;

    add di, 32
    mov cx, word [cwdCluster]
    
    mov word [di+dirFat.filename],     '..'
    mov word [di+dirFat.filename+2],   0x2020   ; Pad filename and ext with spaces
    mov word [di+dirFat.filename+4],   0x2020
    mov word [di+dirFat.filename+6],   0x2020
    mov word [di+dirFat.filename+8],   0x2020
    mov byte [di+dirFat.filename+10],  0x20
    mov byte [di+dirFat.attributes],   0x10     ; File attribtutes
    mov byte [di+dirFat.reserved],     0        ; Reserved for Windows NT usage
    mov byte [di+dirFat.idk],          0x4c     ; 10-millisecond units past creation time below
    mov word [di+dirFat.creationTime], 0x6996   ; Time of the file created
    mov word [di+dirFat.creationDate], 0x5036   ; Date of the file created
    mov word [di+dirFat.accessedDate], 0x5036   ; Date of last access to the file
    mov word [di+dirFat.clusterHi],    0        ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [di+dirFat.modifiedTime], 0x6997   ; Time of the file last modified
    mov word [di+dirFat.modifiedDate], 0x5036   ; Date of the file last modified
    mov word [di+dirFat.clusterLo],    cx       ; Low starting cluster
    mov word [di+dirFat.filesize],     0        ; Size of the file
    mov word [di+dirFat.filesize+2],   0        ; 

    sub di, 32
    call writeClusters                          ; Now write the two dir entrys to the disk

    call memFreeBytes
    
    call loadRootDir                            ; Allocate and read the root dir
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

    mov byte [di+dirFat.attributes],   0x10     ; File attribtutes
    mov byte [di+dirFat.reserved],     0        ; Reserved for Windows NT usage
    mov byte [di+dirFat.idk],          0x4c     ; 10-millisecond units past creation time below
    mov word [di+dirFat.creationTime], 0x6996   ; Time of the file created
    mov word [di+dirFat.creationDate], 0x5036   ; Date of the file created
    mov word [di+dirFat.accessedDate], 0x5036   ; Date of last access to the file
    mov word [di+dirFat.clusterHi],    0        ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [di+dirFat.modifiedTime], 0x6997   ; Time of the file last modified
    mov word [di+dirFat.modifiedDate], 0x5036   ; Date of the file last modified
    mov word [di+dirFat.clusterLo],    cx       ; Low starting cluster
    mov word [di+dirFat.filesize],     0        ; Size of the file
    mov word [di+dirFat.filesize+2],   0        ; 

    mov es, bx
    mov di, dx
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeDirError
    
    call unloadRootDir                          ; Free the root dir from memory

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
    call unloadRootDir                          ; Free the root dir from memory

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

  .tmpName times 12 db 0x00
        
;--------------------------------------------------  
removeDir:
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

    call loadRootDir                            ; Allocate and read the root dir
    jc .loadDirError

    mov bx, es                                  ; Save the current dir offset
    mov dx, di
    
    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov cx, word [es:di+dirFat.clusterLo]       ; Get the file cluster
    mov al, byte [es:di+dirFat.attributes]      ; Get the file atribute byte

    cmp al, 0x10                                ; Check to see if its a directory
    jne .notDir

    mov byte [es:di], 0xe5                      ; Mark file entry as deleted
    
    mov es, bx
    mov di, dx
    call writeRootDir
    
    call unloadRootDir                          ; Free the root dir from memory

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
    call unloadRootDir                          ; Free the root dir from memory
    
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

    call loadRootDir                            ; Allocate and read the root dir
    jc .error
        
    mov bx, es                                  ; Save the current dir offset
    mov dx, di
    
    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov dx, word [di+dirFat.filesize]           ; Get the size of the file
    mov ax, word [di+dirFat.filesize+2]
    
    call unloadRootDir                          ; Free the root dir from memory
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop cx
    pop bx
    
    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
    call unloadRootDir                          ; Free the root dir from memory

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
; Load the root dir and search for a file to see
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

    call loadRootDir                            ; Allocate and read the root dir
    jc .error

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound
    
    call unloadRootDir                          ; Free the root dir from memory
    
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
    call unloadRootDir                          ; Free the root dir from memory

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
    
    call loadRootDir                            ; Allocate and read the root dir
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

    sub di, 11                                  ; 

    mov byte [di+dirFat.attributes],   0        ; File attribtutes
    mov byte [di+dirFat.reserved],     0        ; Reserved for Windows NT usage
    mov byte [di+dirFat.idk],          0        ; 10-millisecond units past creation time below
    mov word [di+dirFat.creationTime], 0xc67e   ; Time of the file created
    mov word [di+dirFat.creationDate], 0        ; Date of the file created
    mov word [di+dirFat.accessedDate], 0        ; Date of last access to the file
    mov word [di+dirFat.clusterHi],    0        ; High starting cluster for file (alwase zero for fat16 and fat12)
    mov word [di+dirFat.modifiedTime], 0xc67e   ; Time of the file last modified
    mov word [di+dirFat.modifiedDate], 0        ; Date of the file last modified
    mov word [di+dirFat.clusterLo],    0        ; Low starting cluster
    mov word [di+dirFat.filesize],     0        ; Size of the file
    mov word [di+dirFat.filesize+2],   0        ; 

    mov es, bx
    mov di, dx
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError
    
    call unloadRootDir                          ; Free the root dir from memory

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
  .writeError:
    call unloadRootDir                          ; Free the root dir from memory

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
; Rename a file in the root directory.
;
; Expects: DS:SI = Filename to change
;          DS:DI = New filename
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
    
    call loadRootDir                            ; Allocate and read the root dir
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
  
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError
    
    call unloadRootDir                          ; Free the root dir from memory

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
  .writeError:
    call unloadRootDir                          ; Free the root dir from memory

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
; Delete a file in the root directory and removes
; all fat clusters relating to the file.
;
; Expects: DS:AX = Filename to remove
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
    
    call loadRootDir                            ; Allocate and read the root dir
    jc .loadRootError
    
    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file to delete
    call searchDir
    jc .fileNotFound

    mov byte [es:di], 0xe5                      ; Mark file entry as deleted
    mov ax, word [di+dirFat.filesize]           ; Size in bytes of the file
    mov cx, word [di+dirFat.clusterLo]          ; File cluster number

    mov es, bx
    mov di, dx
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError1

    call unloadRootDir                          ; Free the root dir from memory

    cmp ax, 0
    je .done

    ;call loadFat                                ; Allocate and read the FAT
    ;jc .loadFatError

    mov ax, cx
    call removeClusters
    jc .writeError2
    
    ;call unloadFat                              ; Free the FAT from memory
    
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
    call unloadRootDir                          ; Free the root dir from memory
    
  .loadRootError:
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
; Create an empty file in the root directory.
;
; Expects: DS:SI = Filename to read
;          ES:DI = Location to load file
;
; Returns: DS:AX = Filesize
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
    
    call loadRootDir                            ; Allocate and read the root dir
    jc .loadRootError
    
    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file
    call searchDir
    jc .fileNotFound

    mov ax, word [es:di+dirFat.clusterLo]          ; File cluster number
    mov cx, word [es:di+dirFat.filesize]           ; Get the size of the file
    mov dx, word [es:di+dirFat.filesize+2]

    call unloadRootDir                          ; Free the root dir from memory

    cmp cx, 0
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
    call freeRootDir                            ; Free the root dir from memory

  .loadRootError:
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

    ;pop word [.loadOFF]
    ;pop word [.loadSEG]
    mov word [.loFilesize], ax                  ; Save the lo word of the filesize
    mov word [.hiFilesize], dx                  ; Save the hi word of the filesize
    
    ;mov si, .tmpName
    ;call fileExists                             ; Do not overwrite a file if it exists!
    ;jnc .fileExists

    mov si, .tmpName 
    call createFile                             ; Create the file to write
    ;jc .createFileError
.fileExists:
    pop di
    pop es
    mov ax, word [.loFilesize]
    mov dx, word [.hiFilesize]
    call writeClusters
    ;; CALL NEW FUNC HERE
    
    call loadRootDir                            ; Allocate and read the root dir
    jc .loadRootError
    
    mov bx, es                                  ; Save the current dir offset
    mov dx, di

    mov si, .tmpName                            ; Search for the file to rename
    call searchDir

    push dx
    ;mov cx, word [.freeClusters]
    mov ax, word [.loFilesize]
    mov dx, word [.hiFilesize]

    xchg ax, dx
    mov word [di+dirFat.clusterLo], cx
    mov word [di+dirFat.filesize], ax
    mov word [di+dirFat.filesize+2], dx
    pop dx
    
    mov es, bx
    mov di, dx
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeRootError
    
    call unloadRootDir                          ; Free the root dir from memory

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

  .writeFatError:                               ; Error, unable to write the new fat to the disk
    call unloadFat                              ; But was able to create a newfile entry
    jmp .error

  .writeSectorsError:                           ; Was able to write FAT and a newfile entry
    pop cx                                      ; But, error on writing clusters to disk
    pop ax
    jmp .error
    
  .writeRootError:
    call unloadRootDir                          ; Free the root dir from memory
    
.fileExistsError:
.createFileError:
.loadFatError:
.loadRootError:                               ; W FAT, W NEWFILE, W CLUSTERS

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


    .loFilesize dw 0
    .hiFilesize dw 0
    .tmpName times 12 db 0x00

