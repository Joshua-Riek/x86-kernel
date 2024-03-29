;  fat.asm
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

%define FAT_DIR_FILENAME               0x00     ; Fat dir offsets
%define FAT_DIR_EXTENSION              0x08
%define FAT_DIR_ATTRIBUTES             0x0b
%define FAT_DIR_RESERVED               0x0c
%define FAT_DIR_CREATION_MS            0x0d
%define FAT_DIR_CREATION_TIME          0x0e
%define FAT_DIR_CREATION_DATE          0x10
%define FAT_DIR_ACCESSED_DATE          0x12
%define FAT_DIR_CLUSTER_HI             0x14
%define FAT_DIR_MODIFIED_TIME          0x16
%define FAT_DIR_MODIFIED_DATE          0x18
%define FAT_DIR_CLUSTER_LO             0x1a
%define FAT_DIR_FILESIZE               0x1c

;--------------------------------------------------
readClustersFAT12:
;
; Load the FAT12 table into memory, then follow the
; cluster chain and load into location es:di.
;
; Please note that this will dynamically allocate
; and load the entire FAT into memory.
;
; Expects: ES:DI = Where to load the file
;          AX    = Cluster start
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

    push es
    push di
    
    mov dx, cs
    mov ds, dx

    call loadFat                                ; Allocate and read the FAT
    jc .loadFatError

    mov word [.fatOFF], di
    mov word [.fatSEG], es

    pop di
    pop es
 
  .clusterLoop:
    push ax

    dec ax
    dec ax
    xor dx, dx
    xor bh, bh                                  ; Calculate the first sector of the given cluster in ax
    mov bl, byte [sectorsPerCluster]            ; First subtract 2 from the cluster
    mul bx                                      ; Multiply the cluster by the sectors per cluster
    add ax, word [startOfData]                  ; Finally add the first data sector

    mov cx, bx                                  ; Sectors to read
    call readSectors                            ; Read the sectors
    jc .readError

    pop ax
   
  .loadNextCluster:
    push es
    push di

    mov di, word [.fatSEG]
    mov es, di                                  ; Tempararly set es:di to the FAT buffer
    mov di, word [.fatOFF]

    xor dx, dx                                  ; Get the next cluster for fat
    mov bl, 3                                   ; We want to multiply by 1.5 so divide by 3/2 
    mul bx                                      ; Multiply the cluster by the numerator
    dec bx                                      ; Return value in ax and remainder in dx
    div bx                                      ; Divide the cluster by the denominator

    xor bx, bx
    add di, ax                                  ; Add the offset into the fat table
    adc bx, 0                                   ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the fat table
    mov ax, es                                  ; Shift left by 4 bits
    add ah, bl                                  ; Then add the higher half to the segment
    mov es, ax

    mov ax, word [es:di]                        ; Load ax to the next cluster in fat

    pop di
    pop es

    or dx, dx                                   ; Is the cluster caluclated even?
    jz .evenCluster

  .oddCluster:
    mov cl, 4                                   ; Drop the first 4 bits of the next cluster
    shr ax, cl
    jmp .nextClusterCalculated

  .evenCluster:
    and ax, 0x0fff                              ; Drop the last 4 bits of next cluster

  .nextClusterCalculated:
    cmp ax, 0x0ff8                              ; Are we at the end of the file?
    jae .fileLoaded

    xchg bx, ax
    xor dx, dx
    xor ah, ah                                  ; Calculate the size in bytes per cluster
    mov al, byte [sectorsPerCluster]            ; So, take the sectors per cluster
    mul word [bytesPerSector]                   ; And mul that by the bytes per sector
    xchg bx, ax                                 ; Bytes per cluster in bx:dx

    mov cl, 4
    shl dx, cl                                  ; Correct the segment offset based on the bytes per cluster
    mov cx, es                                  ; Shift left by 4 bits
    add ch, dl                                  ; Then add the lower half to the segment
    mov es, cx

    clc
    add di, bx                                  ; Add to the pointer offset
    jnc .clusterLoop 

  .fixBuffer:                                   ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000

    jmp .clusterLoop                            ; Load the next file cluster

  .fileLoaded:
    call unloadFat

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

  .loadFatError:
    pop di
    pop es
    jmp .error

  .readError:
    pop ax

    call unloadFat

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

  .fatSEG dw 0
  .fatOFF dw 0

;--------------------------------------------------
readClustersFAT16:
;
; Load the FAT16 table into memory, then follow the
; cluster chain and load into location es:di.
;
; Please note that this will dynamicly allocate
; and load the entire FAT into memory.
;
; Expects: ES:DI = Where to load the file
;          AX    = Cluster start
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

    push es
    push di

    mov dx, cs
    mov ds, dx

    call loadFat                                ; Allocate and read the FAT
    jc .loadFatError

    mov word [.fatOFF], di
    mov word [.fatSEG], es

    pop di
    pop es

  .clusterLoop:
    push ax

    dec ax
    dec ax
    xor dx, dx
    xor bh, bh                                  ; Calculate the first sector of the given cluster in ax
    mov bl, byte [sectorsPerCluster]            ; First subtract 2 from the cluster
    mul bx                                      ; Multiply the cluster by the sectors per cluster
    add ax, word [startOfData]                  ; Finally add the first data sector

    mov cx, bx                                  ; Sectors to read
    call readSectors                            ; Read the sectors
    jc .readError

    pop ax

  .loadNextCluster:
    push es
    push di

    mov di, word [.fatSEG]
    mov es, di                                  ; Tempararly set es:di to the fat buffer
    mov di, word [.fatOFF]

    xor dx, dx
    mov cx, 2                                   ; Get the next cluster for fat 
    mul cx                                      ; Multiply the cluster by 2

    xor bx, bx
    add di, ax                                  ; Add the offset into the fat table
    adc bx, dx                                  ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the fat table
    mov dx, es                                  ; Shift left by 4 bits
    add dh, bl                                  ; Then add the higher half to the segment
    mov es, dx

    mov ax, word [es:di]                        ; Load ax to the next cluster in the fat table

    pop di
    pop es

  .nextClusterCalculated:
    cmp ax, 0xfff8                              ; Are we at the end of the file?
    jae .fileLoaded

    xchg bx, ax
    xor dx, dx
    xor ah, ah                                  ; Calculate the size in bytes per cluster
    mov al, byte [sectorsPerCluster]            ; So, take the sectors per cluster
    mul word [bytesPerSector]                   ; And mul that by the bytes per sector
    xchg bx, ax                                 ; Bytes per cluster in bx:dx

    mov cl, 4
    shl dx, cl                                  ; Correct the segment offset based on the bytes per cluster
    mov cx, es                                  ; Shift left by 4 bits
    add ch, dl                                  ; Then add the lower half to the segment
    mov es, cx

    clc                                         ; Add to the pointer offset
    add di, bx
    jnc .clusterLoop 

  .fixBuffer:                                   ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000

    jmp .clusterLoop                            ; Load the next file cluster

  .fileLoaded:
    call unloadFat
    
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

  .loadFatError:
    pop di
    pop es
    jmp .error

  .readError:
    pop ax

    call unloadFat

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

  .fatSEG dw 0
  .fatOFF dw 0

;--------------------------------------------------
readClusters:
;
; This is a wrapper function to determine if to
; use FAT12 or FAT16 when reading clusters.
;
; Expects: ES:DI = Where to load the file
;          AX    = Cluster start
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
    mov ds, dx

    mov cx, ax

    cmp word [sectors], 0
    jne .smallSectors

  .largeSectors:                                ; Use huge sectors for fat16 
    mov ax, word [hugeSectors]
    mov dx, word [hugeSectors+2]
    jmp .calculateTotalClusters

  .smallSectors:                                ; Use sectors for fat12
    mov ax, word [sectors]
    xor dx, dx

   .calculateTotalClusters:
    xor bh, bh 
    sub ax, word [startOfData]                  ; Subtract the total number of sectors by the start of data
    sbb dx, 0
    mov bl, byte [sectorsPerCluster]            ; Divided by the sectors per cluster
    div bx

    xchg cx, ax                                 ; Current cluster number

    cmp cx, 4096                                ; Calculate the next FAT12 or FAT16 cluster
    jbe .FAT12

  .FAT16:
    call readClustersFAT16
    jmp .done
    
  .FAT12:
    call readClustersFAT12

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

;--------------------------------------------------
removeClustersFAT12:
;
; Load the FAT12 table into memory, then calculate
; and remove the used file clusters based on the
; starting cluster number.
;
; Please note that this will dynamicly allocate
; and load the entire FAT into memory.
;
; Expects: AX    = Starting cluster
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

    call loadFat                                ; Allocate and read the FAT
    jc .loadFatError

  .loadNextCluster:
    push es
    push di

    xor dx, dx                                  ; Get the next cluster for fat
    mov bl, 3                                   ; We want to multiply by 1.5 so divide by 3/2 
    mul bx                                      ; Multiply the cluster by the numerator
    dec bx                                      ; Return value in ax and remainder in dx
    div bx                                      ; Divide the cluster by the denominator

    xor bx, bx
    add di, ax                                  ; Add the offset into the fat table
    adc bx, 0                                   ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the fat table
    mov ax, es                                  ; Shift left by 4 bits
    add ah, bl                                  ; Then add the higher half to the segment
    mov es, ax

    mov ax, word [es:di]                        ; Load ax to the next cluster in fat

    or dx, dx                                   ; Is the cluster caluclated even?
    jz .evenCluster

  .oddCluster:
    mov bx, ax
    and bx, 0x000f
    mov word [es:di], bx

    mov cl, 4                                   ; Drop the first 4 bits of the next cluster
    shr ax, cl
    jmp .nextClusterCalculated

  .evenCluster:
    mov bx, ax
    and bx, 0xf000
    mov word [es:di], bx

    and ax, 0x0fff                              ; Drop the last 4 bits of next cluster

  .nextClusterCalculated:
    pop di
    pop es
    
    cmp ax, 0x0ff8
    jae .eof                                    ; Are we at the end of the file?

    jmp .loadNextCluster 

  .eof:
    call writeFat                               ; Write the updated fat to disk
    jc .writeError

    call unloadFat

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
    call unloadFat

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

;--------------------------------------------------
removeClustersFAT16:
;
; Load the FAT16 table into memory, then calculate
; and remove the used file clusters based on the
; starting cluster number.
;
; Please note that this will dynamicly allocate
; and load the entire FAT into memory.
;
; Expects: AX    = Starting cluster
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

    call loadFat                                ; Allocate and read the FAT
    jc .loadFatError

  .loadNextCluster:
    push es
    push di

    xor dx, dx
    mov cx, 2                                   ; Get the next cluster for fat 
    mul cx                                      ; Multiply the cluster by 2

    xor bx, bx
    add di, ax                                  ; Add the offset into the fat table
    adc bx, dx                                  ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the fat table
    mov dx, es                                  ; Shift left by 4 bits
    add dh, bl                                  ; Then add the higher half to the segment
    mov es, dx

    mov ax, word [es:di]                        ; Load ax to the next cluster in the fat table

    mov bx, ax
    and bx, 0x0000
    mov word [es:di], bx                        ; Zero out the cluster entry

    pop di
    pop es

  .nextClusterCalculated:
    cmp ax, 0xfff8
    jae .eof                                    ; Are we at the end of the file?

    jmp .loadNextCluster 

  .eof:
    call writeFat                               ; Write the updated fat to disk
    jc .writeError

    call unloadFat

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
    call unloadFat

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

;--------------------------------------------------
removeClusters:   
;
; This is a wrapper function to determine if to
; use FAT12 or FAT16 when removing clusters.
;
; Expects: AX    = Starting cluster
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
    mov ds, dx

    mov cx, ax

    cmp word [sectors], 0
    jne .smallSectors

  .largeSectors:                                ; Use huge sectors for fat16 
    mov ax, word [hugeSectors]
    mov dx, word [hugeSectors+2]
    jmp .calculateTotalClusters

  .smallSectors:                                ; Use sectors for fat12
    mov ax, word [sectors]
    xor dx, dx

   .calculateTotalClusters:
    xor bh, bh 
    sub ax, word [startOfData]                  ; Subtract the total number of sectors by the start of data
    sbb dx, 0
    mov bl, byte [sectorsPerCluster]            ; Divided by the sectors per cluster
    div bx

    xchg cx, ax                                 ; Current cluster number

    cmp cx, 4096                                ; Calculate the next FAT12 or FAT16 cluster
    jbe .FAT12

  .FAT16:
    call removeClustersFAT16
    jmp .done

  .FAT12:
    call removeClustersFAT12

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

;--------------------------------------------------
writeClustersFAT12:
;
; Take the data from location es:di and write it
; to the disk. This calculates the free clusters
; reqired into a list, then follows the cluster
; chain, lastly saving the updated fat table
; to the disk.
;
; Expects: ES:DI = Location of data
;          AX:DX = Filesize
;
; Returns:    CX = Cluster
;             CF = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx

    push es
    push di
    pop word [.loadOFF]
    pop word [.loadSEG]

    mov si, .freeClusters
    mov cx, 2048

  .zeroClusterLoop:                             ; Just to make sure no other clusters
    mov word [ds:si], 0                         ; are left over on further calls of
    inc si                                      ; this function
    inc si
    loop .zeroClusterLoop

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

  .loadFat:
    mov word [.clustersNeeded], ax
    mov dx, ax

    cmp dx, 0                                   ; If no clusters are needed, do nothing
    je .fileZero

    call loadFat                                ; Allocate and read the FAT
    jc .loadFatError
    
    push es
    push di

    add di, 3                                   ; Skip the fist two clusters
    mov bx, 2                                   ; Current cluster counter
    mov cx, word [.clustersNeeded]              ; Clusters needed
    mov dx, 0                                   ; Offset into free cluster list

  .findFreeCluster:
    mov ax, word [es:di]                        ; Grab the next word in FAT

    clc
    add di, 2
    jnc .checkOdd

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .checkOdd:
    and ax, 0x0fff                              ; Mask out for even cluster
    jz .foundFreeEven                           ; If zero, entry is free 

  .moreOdd:
    inc bx                                      ; If not free, increase cluster counter
    dec di                                      ; Decrease counter byte in FAT

    mov ax, word [es:di]                        ; Grab the next word in FAT

    clc
    add di, 2
    jnc .more

  .fixBuffer2:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .more:
    shr ax, 1                                   ; Shift out for odd cluster
    shr ax, 1
    shr ax, 1
    shr ax, 1
    or ax, ax
    jz .foundFreeOdd                            ; If zero, entry is free

  .moreEven:
    inc bx                                      ; If not free, increase cluster clounter
    jmp .findFreeCluster

  .foundFreeEven:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, dx

    mov word [ds:si], bx                        ; Put the cluster into the list

    dec cx                                      ; Check to see if we have all the clusters needed
    cmp cx, 0
    je .freeClustersFound

    inc dx                                      ; If not, continute to next cluster 
    inc dx
    jmp .moreOdd

  .foundFreeOdd:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, dx

    mov word [ds:si], bx                        ; Put the cluster into the list

    dec cx                                      ; Check to see if we have all the clusters needed
    cmp cx, 0
    je .freeClustersFound

    inc dx                                      ; If not, continute to next cluster 
    inc dx
    jmp .moreEven

  .freeClustersFound:
    pop di
    pop es

    mov cx, 0
    mov word [.count], 1
    mov ax, word [.clustersNeeded]

  .chainClusterLoop:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    xor dx, dx
    mov ax, word [ds:si]

  .loadNextCluster:
    push es                                     ; Save the current position in memory 
    push di                                     ; For es:di contains the FAT location
    push cx

    xor dx, dx                                  ; Get the next cluster for FAT
    mov bx, 3                                   ; We want to multiply by 1.5 so divide by 3/2 
    mul bx                                      ; Multiply the cluster by the numerator
    mov bl, 2                                   ; Return value in ax and remainder in dx
    div bx                                      ; Divide the cluster by the denominator

    xor bx, bx
    add di, ax                                  ; Add the offset into the FAT table
    adc bx, 0                                   ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the FAT table
    mov ax, es                                  ; Shift left by 4 bits
    add ah, bl                                  ; Then add the higher half to the segment
    mov es, ax
    
    pop cx

  .loadNextCluster2:  
    mov ax, word [es:di]                        ; Load ax to the next cluster in the FAT table
    mov bx, word [.count]                       ; Check here to see if we have all the
    cmp bx, word [.clustersNeeded]              ; clusters we need to allocate
    je .lastCluster

    or dx, dx                                   ; Is the cluster caluclated even?
    jz .evenCluster

  .oddCluster:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    and ax, 0x000f                              ; Zero out the bits for the next cluster
    mov bx, word [ds:si+2]                      ; Get the NEXT cluster
    shl bx, 1                                   ; Shift left for correct FAT cluster format
    shl bx, 1
    shl bx, 1
    shl bx, 1
    add ax, bx

    mov word [es:di], ax                        ; Store the cluster back into the loaded FAT table

    pop di
    pop es

    inc word [.count]
    inc cx
    inc cx
    jmp .chainClusterLoop

  .evenCluster:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    and ax, 0xf000                              ; Zero out the bits for the next cluster
    mov bx, word [ds:si+2]                      ; Get the NEXT cluster
    add ax, bx

    mov word [es:di], ax                        ; Store the cluster back into the loaded FAT table
    
    pop di
    pop es

    inc word [.count]
    inc cx
    inc cx
    jmp .chainClusterLoop

  .lastCluster:
    or dx, dx                                   ; Double check to see if the last cluster is even or odd
    jz .evenLast

  .oddLast:
    and ax, 0x000f                              ; Potental error, not sure why i dont add 0xff80
    ;add ax, 0xff80
    add ax, 0xfff0
    jmp .chainDone

  .evenLast:
    and ax, 0xf000
    add ax, 0xff8

  .chainDone:
    mov word [es:di], ax                        ; Finally store the last cluster into the FAT12 table
    pop di
    pop es

    call writeFat                               ; Then write the new FAT12 table to the disk 
    jc .writeFatError

    call unloadFat                              ; Kiss that FAT ass goodbye and unallocate it from mem

    mov di, word [.loadSEG]
    mov es, di                                  ; Set es:di to the file's data, so we may
    mov di, word [.loadOFF]                     ; write that aswell onto the disk

  .checkDir:
    ;mov cx, word [cwdCluster]
    ;cmp cx, 0                                  ; When cwd cluster is zero, its the root dir
    ;je .foo

    mov cl, byte [es:di+FAT_DIR_ATTRIBUTES]     ; Get the file atribute byte
    cmp cl, 0x10                                ; Check to see if its a directory
    jne .saveData

    mov cl, byte [es:di]                        ; Check for the directory entry thingy
    cmp cl, '.'
    jne .saveData

    mov cx, word [.freeClusters]                ; Update its current cluster
    mov word [es:di+FAT_DIR_CLUSTER_LO], cx

  .saveData:
    mov cx, 0

  .saveLoop:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    mov ax, word [ds:si]                        ; Grab the next cluster from the FAT table

    cmp ax, 0                                   ; Check for the last cluster to write
    je .fileSaved

    push cx                                     ; Offset into free clusters
    push ax                                     ; Current cluster

    dec ax
    dec ax
    xor dx, dx
    xor bh, bh                                  ; Calculate the first sector of the given cluster in ax
    mov bl, byte [sectorsPerCluster]            ; First subtract 2 from the cluster
    mul bx                                      ; Multiply the cluster by the sectors per cluster
    add ax, word [startOfData]                  ; Finally add the first data sector

    xor ch, ch
    mov cl, byte [sectorsPerCluster]            ; Sectors to write
    call writeSectors                           ; Write the sectors
    jc .writeSectorsError

    pop ax

    xchg bx, ax
    xor dx, dx
    xor ah, ah                                  ; Calculate the size in bytes per cluster
    mov al, byte [sectorsPerCluster]            ; So, take the sectors per cluster
    mul word [bytesPerSector]                   ; And mul that by the bytes per sector
    xchg bx, ax                                 ; Bytes per cluster in bx:dx

    mov cl, 4
    shl dx, cl                                  ; Correct the segment offset based on the bytes per cluster
    mov cx, es                                  ; Shift left by 4 bits
    add ch, dl                                  ; Then add the lower half to the segment
    mov es, cx

    clc
    add di, bx                                  ; Point to the next portion of data to write
    jnc .saveNextCluster

  .fixBuffer3:                                  ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000

  .saveNextCluster:
    pop cx
    inc cx
    inc cx
    jmp .saveLoop

  .fileSaved:
  .fileZero:
    mov cx, word [.freeClusters]

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeFatError:                               ; Error, unable to write the new fat to the disk
    call unloadFat
    jmp .error

  .writeSectorsError:                           ; Was able to write FAT
    pop cx                                      ; But, error on writing clusters to disk
    pop ax
    jmp .error

  .loadFatError:
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .count dw 0
  .loadSEG dw 0
  .loadOFF dw 0
  .clustersNeeded dw 0
  .freeClusters times 2048 dw 0

;--------------------------------------------------
writeClustersFAT16:
;
; Take the data from location es:di and write it
; to the disk. This calculates the free clusters
; reqired into a list, then follows the cluster
; chain, lastly saving the updated fat table
; to the disk.
;
; Expects: ES:DI = Location of data
;          AX:DX = Filesize
;
; Returns:    CX = Cluster
;             CF = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx

    push es
    push di
    pop word [.loadOFF]
    pop word [.loadSEG]

    mov si, .freeClusters                       ; TODO: dynamic allocation for free cluster list
    mov cx, 2048

  .zeroClusterLoop:                             ; Just to make sure no other clusters
    mov word [ds:si], 0                         ; are left over on further calls of
    inc si                                      ; this function
    inc si
    loop .zeroClusterLoop


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

  .loadFat:
    mov word [.clustersNeeded], ax
    mov dx, ax

    ;cmp dx, 0                                   ; If no clusters are needed, do nothing
    ;je .fileZero

    call loadFat                                ; Allocate and read the FAT16 table into memory
    jc .loadFatError

    push es
    push di

    add di, 4                                   ; Skip the fist two clusters
    mov bx, 2                                   ; Current cluster counter
    mov cx, word [.clustersNeeded]              ; Clusters needed
    mov dx, 0                                   ; Offset into free cluster list

  .findFreeCluster:
    mov ax, word [es:di]                        ; Grab the next word in FAT

    clc
    add di, 2
    jnc .checkFree

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx

  .checkFree:
    or ax, ax
    jz .foundFree                               ; If zero, entry is free

  .more:
    inc bx                                      ; If not free, increase cluster clounter
    jmp .findFreeCluster

  .foundFree:
    mov si, .freeClusters
    add si, dx                                  ; Offset into free cluster list

    mov word [ds:si], bx                        ; Put the cluster into the list

    dec cx                                      ; Check to see if we have all the clusters needed
    cmp cx, 0
    je .freeClustersFound

    inc dx                                      ; If not, continute to next cluster 
    inc dx
    jmp .more

  .freeClustersFound:
    pop di
    pop es

    mov cx, 0
    mov word [.count], 1
    mov ax, word [.clustersNeeded]

  .chainClusterLoop:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    mov ax, word [ds:si]                        ; Grab a free cluster from the free cluster list

  .loadNextCluster:
    push es
    push di
    push cx

    xor dx, dx
    mov cx, 2                                   ; Get the next cluster for FAT 
    mul cx                                      ; Multiply the cluster by 2

    xor bx, bx
    add di, ax                                  ; Add the offset into the FAT table
    adc bx, dx                                  ; Make sure to adjust for carry 

    mov cl, 4
    shl bl, cl                                  ; Correct the segment based on the offset into the FAT table
    mov dx, es                                  ; Shift left by 4 bits
    add dh, bl                                  ; Then add the higher half to the segment
    mov es, dx

    pop cx

  .loadNextCluster2:  
    mov ax, word [es:di]                        ; Load ax to the next cluster in the FAT table
    mov bx, word [.count]                       ; Check here to see if we have all the
    cmp bx, word [.clustersNeeded]              ; clusters we need to allocate
    je .lastCluster

    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    mov bx, word [ds:si+2]                      ; Get the NEXT cluster
    add ax, bx

    mov word [es:di], ax                        ; Store the cluster back into the loaded FAT table

    pop di
    pop es

    inc word [.count]
    inc cx
    inc cx
    jmp .chainClusterLoop

  .lastCluster:
    mov ax, 0xffff

  .chainDone:
    mov word [es:di], ax                        ; Finally store the last cluster into the FAT table
    pop di
    pop es

    call writeFat                               ; Then write the new FAT table to the disk 
    jc .writeFatError

    call unloadFat                              ; Kiss that FAT ass goodbye and unallocate it from mem

    mov di, word [.loadSEG]
    mov es, di                                  ; Set es:di to the file's data, so we may
    mov di, word [.loadOFF]                     ; write that aswell onto the disk

  .checkDir:
    ;mov cx, word [cwdCluster]
    ;cmp cx, 0                                  ; When cwd cluster is zero, its the root dir
    ;je .foo

    mov cl, byte [es:di+FAT_DIR_ATTRIBUTES]     ; Get the file atribute byte
    cmp cl, 0x10                                ; Check to see if its a directory
    jne .saveData

    mov cl, byte [es:di]                        ; Check for the directory entry thingy
    cmp cl, '.'
    jne .saveData

    mov cx, word [.freeClusters]                ; Update its current cluster
    mov word [es:di+FAT_DIR_CLUSTER_LO], cx

  .saveData:
    mov cx, 0

  .saveLoop:
    mov si, .freeClusters                       ; Point into the free cluster list
    add si, cx

    mov ax, word [ds:si]                        ; Grab the next cluster from the FAT table

    cmp ax, 0                                   ; Check for the last cluster to write
    je .fileSaved

    push cx                                     ; Offset into free clusters
    push ax                                     ; Current cluster

    dec ax
    dec ax
    xor dx, dx
    xor bh, bh                                  ; Calculate the first sector of the given cluster in ax
    mov bl, byte [sectorsPerCluster]            ; First subtract 2 from the cluster
    mul bx                                      ; Multiply the cluster by the sectors per cluster
    add ax, word [startOfData]                  ; Finally add the first data sector

    xor ch, ch
    mov cl, byte [sectorsPerCluster]            ; Sectors to write
    call writeSectors                           ; Write the sectors
    jc .writeSectorsError

    pop ax

    xchg cx, ax
    xor dx, dx
    xor bh, bh                                  ; Calculate the size in bytes per cluster
    mov ax, word [bytesPerSector]               ; So, take the bytes per sector
    mov bl, byte [sectorsPerCluster]            ; and mul that by the sectors per cluster
    mul bx
    xchg cx, ax

    push cx
    mov cl, 4
    shl dx, cl
    mov bx, es
    add bh, dl
    mov es, bx
    pop bx

    clc
    add di, bx                                  ; Point to the next portion of data to write
    jnc .saveNextCluster

  .fixBuffer2:                                  ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000

  .saveNextCluster:
    pop cx
    inc cx
    inc cx
    jmp .saveLoop

  .fileSaved:
  .fileZero:
    mov cx, word [.freeClusters]

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop bx
    pop ax

    clc                                         ; Clear carry, for no error
    ret

  .writeFatError:                               ; Error, unable to write the new fat to the disk
    call unloadFat
    jmp .error

  .writeSectorsError:                           ; Was able to write FAT
    pop ax                                      ; But, error on writing clusters to disk
    pop cx
    jmp .error

  .loadFatError:
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop bx
    pop ax

    stc                                         ; Set carry, error occured
    ret

  .count dw 0
  .loadSEG dw 0
  .loadOFF dw 0
  .clustersNeeded dw 0
  .freeClusters times 2048 dw 0

;--------------------------------------------------
writeClusters:   
;
; This is a wrapper function to determine if to
; use FAT12 or FAT16 when removing clusters.
;
;
; Expects: ES:DI = Location of data
;          AX:DX = Filesize
;   
; Returns:    CX = Cluster
;             CF = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx
    push si
    push di
    push es
    push ds

    push es
    push di
    push ax
    push dx
    
    mov dx, cs
    mov ds, dx
    
    mov cx, ax

    cmp word [sectors], 0
    jne .smallSectors

  .largeSectors:                                ; Use huge sectors for fat16 
    mov ax, word [hugeSectors]
    mov dx, word [hugeSectors+2]
    jmp .calculateTotalClusters

  .smallSectors:                                ; Use sectors for fat12
    mov ax, word [sectors]
    xor dx, dx

   .calculateTotalClusters:
    xor bh, bh 
    sub ax, word [startOfData]                  ; Subtract the total number of sectors by the start of data
    sbb dx, 0
    mov bl, byte [sectorsPerCluster]            ; Divided by the sectors per cluster
    div bx

    xchg cx, ax                                 ; Current cluster number

    pop dx
    pop ax
    pop di
    pop es

    cmp cx, 4096                                ; Calculate the next FAT12 or FAT16 cluster
    jbe .FAT12

  .FAT16:
    call writeClustersFAT16
    jmp .done

  .FAT12:
    call writeClustersFAT12

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop bx
    pop ax

    ret

;--------------------------------------------------  
fatFmtTimeAndDate:
;
; Get the time and date in fat format.
;
; Expects: Nothing
;
; Returns: AX    = Time
;          DX    = Date
;
;--------------------------------------------------  
    push bx                                     ; Save registers
    push cx

    call cmosReadTime

    mov al, ch
    and ax, 0000000000011111b                   ; Mask 5 bits for the hour
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1

    mov bl, cl
    and bx, 0000000000111111b                   ; Mask 6 bits for the minute
    shl bx, 1
    shl bx, 1
    shl bx, 1
    shl bx, 1
    shl bx, 1
    or ax, bx

    mov bl, dh
    and bx, 0000000000011111b                   ; Mask 5 bits for the second
    or ax, bx
    push ax

    call cmosReadDate   

    mov ax, cx
    sub ax , 1980
    and ax, 0000000001111111b                   ; Mask 7 bits for the year
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1
    shl ax, 1

    mov bl, dh
    and bx, 0000000000001111b                   ; Mask 4 bits for the month
    shl bx, 1
    shl bx, 1
    shl bx, 1
    shl bx, 1
    shl bx, 1
    or ax, bx

    mov bl, dl
    and bx, 0000000000011111b                   ; Mask 5 bits for the day
    or ax, bx

    pop dx
    xchg ax, dx

    pop cx                                      ; Restore registers
    pop bx

    ret
