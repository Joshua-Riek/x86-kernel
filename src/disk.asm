;  bvideo.asm
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
    drive db 0
    tmpFilename1 times 12 db 0
    tmpFilename2 times 12 db 0

    diskBufferSeg     dw 0
    diskBufferOff     dw 0
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
; Expects: CX    = Number of sectors to read
;          AX    = Starting sector or lba    
;          ES:DI = Disk buffer
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

    mov bx, cs                                  ; Ensure corret data segment
    mov ds, bx
    
    mov bx, di                                  ; Set disk buffer offset to bx
    
  .sectorLoop:
    push ax
    push cx

    push bx                                     ; Save disk buffer offset
    
    xor dx, dx
    div word [sectorsPerTrack]                  ; Divide the lba (value in ax) by sectorsPerTrack
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
    pop cx
    pop ax
    
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
    pop cx
    pop ax

  .driveError:
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
; Expects: CX    = Number of sectors to read
;          AX    = Starting sector or lba    
;          ES:DI = Disk buffer
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

    mov bx, cs                                  ; Ensure corret data segment
    mov ds, bx
    
    mov dl, byte [drive]                        ; Set correct drive for int 13h
    mov bx, di                                  ; Set disk buffer offset to bx
    
  .sectorLoop:
    push ax
    push cx

    push bx                                     ; Save disk buffer offset
    push dx                                     ; Save drive number
    
    xor dx, dx
    div word [sectorsPerTrack]                  ; Divide the lba (value in ax) by sectorsPerTrack
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
 
    pop dx                                      ; Set correct drive for int 13h
    mov dh, bh                                  ; Move the absolute head into dh
    pop bx                                      ; Restore disk buffer offset
    
    mov di, 5                                   ; Try five times to write the sector
    
  .attemptRead:
    mov ax, 0x0301                              ; Write sectors func of int 13h, write one sector
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jnc .readOk                                 ; If no carry set, the sector has been written
  
    xor ah, ah                                  ; Reset Drive func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    
    dec di                                      ; Decrease write attempt counter
    jnz .attemptRead                            ; Try to write the sector again

    jmp .readError
    
  .readOk:
    pop cx
    pop ax
    
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
    
  .readError:                                   ; Error while reading a sector
  
    pop cx
    pop ax

  .driveError:
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
readClusters:
;
; Read the starting cluster passed, loads into 
; location es:di, please note that the fat must
; be last loaded into memory, or else this will not work.
;
; Expects: AX    = Starting sector or lba
;          ES:DI = Location of FAT (MUST BE LOADED INTO MEMORY)
;          DS:SI = Where to load the file
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

  .readCluster:
    push ax                                     ; Save the current cluster

    push es                                     ; Save the data and extra seg to stack
    push ds 
    mov dx, cs
    mov ds, dx
    
    xor bh, bh
    xor dx, dx                                  ; Get the cluster start = (cluster - 2) * sectorsPerCluster + userData
    sub ax, 2                                   ; Subtract 2
    mov bl, byte [sectorsPerCluster]            ; Sectors per cluster is a byte value
    mul bx                                      ; Multiply (cluster - 2) * sectorsPerCluster
    add ax, word [startOfData]                  ; Add the userData 

    xor ch, ch
    mov cl, byte [sectorsPerCluster]            ; Sectors to read

    pop es                                      ; Pop them in reverse to switch values
    pop ds
    xchg di, si

    call readSectors                            ; Read the sectors
    jc .readError

    pop ax                                      ; Restore cluster number
    push es
    push ds

    xor dx, dx
;
;   NOTE: Not sure if this is needed for fat16     
;   cmp word [totalClusters], 4085              ; Calculate the next FAT12 or FAT16 sector
    jmp .calculateNextSector12
    
  .calculateNextSector16:                       ; Get the next sector for FAT16 (cluster * 2)
    mov bx, 2                                   ; Multiply the cluster by two (cluster is in ax)
    mul bx

    jmp .loadNextSector
    
  .calculateNextSector12:                       ; Get the next sector for FAT12 (cluster + (cluster * 1.5))
    mov bx, 3                                   ; We want to multiply by 1.5 so divide by 3/2 
    mul bx                                      ; Multiply the cluster by the numerator
    mov bx, 2                                   ; Return value in ax and remainder in dx
    div bx                                      ; Divide the cluster by the denominator
   
  .loadNextSector:
    pop es
    pop ds
    xchg di, si

    push di
    add di, ax                                  ; Point to the next cluster in the FAT entry
    mov ax, word [es:di]                        ; Load ax to the next cluster in FAT
    pop di
       
    or dx, dx                                   ; Is the cluster caluclated even?
    jz .evenSector

  .oddSector:    
    shr ax, 1                                   ; Drop the first 4 bits of the next cluster
    shr ax, 1
    shr ax, 1
    shr ax, 1
    jmp .nextSectorCalculated

  .evenSector:
    and ax, 0x0fff                              ; Drop the last 4 bits of next cluster

  .nextSectorCalculated:                        ; Register ax is set with the next cluster 
    cmp ax, 0x0ff8
    jae .fileLoaded                             ; Are we at the end of the file?

    clc
    add si, 512                                 ; Add to the buffer address for the next sector
    jnc .readCluster 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, ds                                  ; overlaps a 64k page boundry, when bx overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov ds, dx                                  ; extra segment by 0x1000
    pop dx

    jmp .readCluster

  .fileLoaded:
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
    pop ax

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
removeClusters:
;
; Take the loaded fat table from the disk buffer,
; then calculate and remove the used file sectors 
; based on the starting cluster number.
;
; Expects: AX    = Starting cluster
;          ES:DI = Disk buffer
;          ES:DI = Location of FAT (MUST BE LOADED INTO MEMORY)
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

    cmp ax, 0                                   ; Do nothing if starting cluster is zero
    je .done
    
  .nextCluster:
    xor dx, dx
    cmp word [totalClusters], 4085              ; Calculate the next FAT12 or FAT16 sector 
    jl .calculateNextSector12
    
  .calculateNextSector16:                       ; Get the next sector for FAT16 (cluster * 2)
    mov bx, 2                                   ; Multiply the cluster by two (cluster is in ax)
    mul bx

    jmp .loadNextSector
    
  .calculateNextSector12:                       ; Get the next sector for FAT12 (cluster + (cluster * 1.5))
    mov bx, 3                                   ; We want to multiply by 1.5 so divide by 3/2 
    mul bx                                      ; Multiply the cluster by the numerator
    mov bx, 2                                   ; Return value in ax and remainder in dx
    div bx                                      ; Divide the cluster by the denominator
   
  .loadNextSector:
    push es
    push di

    add di, ax                                  ; Point to the next cluster in the FAT entry
    mov ax, word [es:di]                        ; Load ax to the next cluster in FAT

    or dx, dx                                   ; Is the cluster caluclated even?
    jz .evenSector

  .oddSector:
    push ax
    and ax, 0x000f
    mov word [es:di], ax
    pop ax

    shr ax, 1                                   ; Drop the first 4 bits of the next cluster
    shr ax, 1
    shr ax, 1
    shr ax, 1
    jmp .nextSectorCalculated

  .evenSector:
    push ax
    and ax, 0xf000
    mov word [es:di], ax
    pop ax

    and ax, 0x0fff                              ; Drop the last 4 bits of next cluster

  .nextSectorCalculated:
    pop di
    pop es
    
    cmp ax, 0x0ff8
    jae .eof                                    ; Are we at the end of the file?

    jmp .nextCluster 

  .eof:
    call writeFat                               ; Write the updated fat to disk
    jc .writeError
    
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
    push cx
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax
    
    mov cx, word [rootDirSize]                  ; Read the size in sectors of the directory
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call readSectors                            ; Read the sectors 
    jc .readError

    pop ds                                      ; Restore registers
    pop cx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret

  .readError:
    pop ds                                      ; Restore registers
    pop cx
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
    push cx
    push ds

    mov ax, cs                                  ; Ensure correct data segment
    mov ds, ax
    
    mov cx, word [rootDirSize]                  ; Size of the directory in sectors
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call writeSectors                           ; Write sectors the sectors
    jc .writeError

    pop ds                                      ; Restore registers
    pop cx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret

  .writeError:
    pop ds                                      ; Restore registers
    pop cx
    pop ax

    stc                                         ; Set carry, error occured
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
; Expects: ES:DI = Disk buffer
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
    push cx
    push si

    push ds
    
    mov cx, cs
    mov ds, cx
    mov cx, word [rootDirEntries]

    pop ds
    
  .search:
    xchg cx, dx

    push si                                     ; Save filename offset
    push di
    
    cld                                         ; Clear direction flag
    mov cx, 11                                  ; Compare first 11 bytes
    rep cmpsb                                   ; Compare ds:si and es:di cx times
    je .fileFound                               ; We found the file :)

    pop di
    pop si                                      ; Restore filename offset
    
    add di, 32
    xchg dx, cx
    loop .search

  .fileNotFound:     
    pop si                                      ; Restore registers
    pop cx
    pop ax
    
    stc                                         ; Set carry, file not found
    ret

  .fileFound:
    pop di                                      ; Restore registers
    pop si
    
    pop si
    pop cx
    pop ax
    
    clc                                         ; Clear carry, file found
    ret

;--------------------------------------------------  
fileExists:
; 
; Load the root dir and search for a file to see
; if it exists.
;
; Expects: DS:AX = Filename to search for
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

    mov si, ax                                  ; File to find
    call convertFilename1                       ; Convert filename and format, store in tmpFilename1

    mov cx, cs
    mov ds, cx
    
    call readRootDir                            ; Read the root directory
    jc .readError

    mov si, tmpFilename1                        ; Search for the file
    call searchDir
    jc .fileNotFound

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
createFile:
;
; Create an empty file in the root directory.
;
; Expects: DS:AX = Filename to create
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
    
    push es                                     ; Save the loaction of the 
    push di                                     ; loaded data to the stack

    mov cx, cs
    mov ds, cx

    mov cx, word [rootDirEntries]
    
  .search:
    mov al, byte [es:di]                        ; Grab the first byte of the entry
    
    cmp al, 0x00                                ; Empty entry Marker
    je .freeEntry
    cmp al, 0xe5                                ; Free entry marker
    je .freeEntry
    
    add di, 32                                  ; Point to the next entry
    loop .search

    pop di
    pop es
    
    jmp .noFileEntrys

  .freeEntry:
    cld                                         ; Clear direction flag
    mov si, tmpFilename1                        ; Get the new filename
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

    pop di
    pop es
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError

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

  .existsError:
  .noFileEntrys:
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
renameFile:
;
; Rename a file in the root directory.
;
; Expects: DS:AX = Filename to change
;          DS:BX = New filename
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------  
    push ax                                     ; Save registers
    push bx
    push cx
    push ds

    mov si, ax                                  ; File to change
    call convertFilename1                       ; Convert filename and format, store in tmpFilename1

    mov si, bx                                  ; New filename
    call convertFilename2                       ; Convert filename and format, store in tmpFilename2

    mov cx, cs
    mov ds, cx
    
    call readRootDir                            ; Read the root directory
    jc .readError
    
    push es                                     ; Save the loaction of the 
    push di                                     ; loaded data to the stack

    mov si, tmpFilename1                        ; Search for the file to rename
    call searchDir
    jc .fileNotFound
    
    cld                                         ; Clear direction flag
    mov si, tmpFilename2                        ; Get the new filename
    mov cx, 11                                  ; Lenght of filename
    rep movsb                                   ; Copy bytes from ds:si to es:di 

    pop di                                      ; Restore root directory location
    pop es
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError

    pop ds                                      ; Restore registers
    pop cx
    pop bx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret

  .fileNotFound:
    pop di                                      ; Cleanup the stack
    pop es
    
  .readError:
  .writeError:
    pop ds                                      ; Restore registers
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret
    
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
    push ds
    
    mov si, ax                                  ; File to delete
    call convertFilename1                       ; Convert filename and format, store in tmpFilename1

    mov cx, cs
    mov ds, cx
    
    call readRootDir                            ; Read the root directory
    jc .readError
    
    push es                                     ; Save the loaction of the 
    push di                                     ; loaded data to the stack

    mov si, tmpFilename1                        ; Search for the file to delete
    call searchDir
    jc .fileNotFound

    mov bx, word [di+dirFat.clusterLo]          ; File cluster number
    mov ax, word [di+dirFat.filesize]           ; Size in bytes of the file
    mov byte [di], 0xe5                         ; Make file entry as deleted

    pop di                                      ; Restore root directory location
    pop es
    
    call writeRootDir                           ; Finally, write it to the disk
    jc .writeError

    cmp ax, 0
    je .done

    call readFat
    jc .readError

    mov ax, bx
    call removeClusters
    jc .writeError
    
  .done:
    pop ds                                      ; Restore registers
    pop cx
    pop bx
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret
    
  .fileNotFound:
    pop di                                      ; Cleanup the stack
    pop es
    
  .readError:
  .writeError:
    pop ds                                      ; Restore registers
    pop cx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

;--------------------------------------------------  
readFile:
;
; Create an empty file in the root directory.
;
; Expects: DS:AX = Filename to create
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

    mov si, ax                                  ; File to read
    call convertFilename1                       ; Convert filename and format, store in tmpFilename1

    mov cx, cs
    mov ds, cx
    
    call readRootDir                            ; Read the root directory
    jc .readError
    
    push es                                     ; Save the loaction of the 
    push di                                     ; loaded data to the stack

    mov si, tmpFilename1                        ; Search for the file
    call searchDir
    jc .fileNotFound

    mov bx, word [di+dirFat.clusterLo]          ; File cluster number
    mov ax, word [di+dirFat.filesize]           ; Get the size of the file
    mov dx, word [di+dirFat.filesize+2]

    pop di                                      ; Restore root directory location
    pop es

    cmp ax, 0
    je .done


    call readFat
    jc .readError


    mov si, 0x1f00
    mov ds, si
    mov si, 0x1000-256
    
    mov ax, bx
    call readClusters
    jc .readError

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
 
    
  .fileNotFound:
    pop di                                      ; Restore root directory location
    pop es
    
  .noFileEntrys:
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
convertFilename1:
;
; Convert the filename into a fat formatted filename (8.3 format)
; then copy it locally for kernel usage.
;
; Expects: DS:SI = Filename
;
; Returns: Nothing (sets tmpFilename1, for kernel usage)
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push cx
    push si
    push di
    push es
    
    mov cx, cs                                  ; Ensure correct extra segment
    mov es, cx

    mov di, tmpFilename1                        ; Converted name will go into es:di
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)
    
    pop es                                      ; Restore registers
    pop di
    pop si
    pop cx
    pop ax
    ret
    
;--------------------------------------------------
convertFilename2:
;
; Convert the filename into a fat formatted filename (8.3 format)
; then copy it locally for kernel usage.
;
; Expects: DS:SI = Filename
;
; Returns: Nothing (sets tmpFilename2, for kernel usage)
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push cx
    push si
    push di
    push es
    
    mov cx, cs                                  ; Ensure correct extra segment
    mov es, cx

    mov di, tmpFilename2                        ; Converted name will go into es:di
    call convertFilename83                      ; Convert the filename into a fat formatted filename (8.3 format)
    
    pop es                                      ; Restore registers
    pop di
    pop si
    pop cx
    pop ax
    ret




