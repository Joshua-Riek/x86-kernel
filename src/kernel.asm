;  kernel.asm
;
;  Entry point into the operating system
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

    %define BOOT_SEG   0x07c0                   ; (BOOT_SEG   << 4) + BOOT_OFF   = 0x007c00
    %define BOOT_OFF   0x0000
    
    %define STACK_SEG  0x0f00                   ; (STACK_SEG  << 4) + STACK_OFF  = 0x010000
    %define STACK_OFF  0x1000

    %define KERNEL_SEG 0x0100                   ; (KERNEL_SEG << 4) + KERNEL_OFF = 0x001000
    %define KERNEL_OFF 0x0000

    %ifidn __OUTPUT_FORMAT__, elf               ; WARNING: Assumes that the text segment is set to
      %define KERNEL_SEG 0x0000                 ; 0x7c00, used ONLY for debugging with GDB
      %define KERNEL_OFF $$
    %endif
    
    bits 16                                     ; Ensure 16-bit code, because fuck 32-bit
    cpu  8086                                   ; Assemble with the 8086 instruction set
    
;---------------------------------------------------
; Kernel entry-point
;---------------------------------------------------

entryPoint:
    jmp KERNEL_SEG:$+5                          ; Fix the cs:ip registers

    mov ax, KERNEL_SEG                          ; Set segments to the location of the bootloader
    mov ds, ax
    mov es, ax
    
    cli
    mov ax, STACK_SEG                           ; Get the the defined stack segment address
    mov ss, ax                                  ; Set segment register to the bottom  of the stack
    mov sp, STACK_OFF                           ; Set ss:sp to the top of the 4k stack
    sti
     
    mov byte [drive], dl                        ; Save the boot drive number

    call setupVideo                             ; Grab the cursor pos from bios

    call setupMemory

    
    mov si, __CURRENT_BUILD                     ; Get the address of the current build string
    call videoWriteStr                          ; Write string to standard output

    mov si, __GPL_NOTICE                        ; Get the address of the gpl3 header notice
    call videoWriteStr                          ; Write string to standard output

    call setupDisk                              ; Setup the disk manager
    jc .diskError

    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push si
    push di


    call driveGets
    mov di, 0x2000
    mov es, di
    mov di, 0x0


    
    mov si, foos
    call readFile
    
    ;jc .readFailure

    mov si, fo0s
    call writeFile

        pop di
    pop si
    pop es
    pop ds
    pop dx
    pop cx
    pop cx
    pop bx
    pop ax
    jmp cliLoop                                 ; Go to the user command line
    
  .hang:
    hlt
    jmp .hang

  .diskError:
    mov si, .diskErrorMsg                       ; Tell the user that their was a disk error
    call videoWriteStr                          ; Write string to standard output
    
  .reboot:
    mov si, .errorMsg                           ; Handle any error starting up the system
    call videoWriteStr                          ; Write string to standard output
    
    xor ax, ax
    int 0x16                                    ; Get a single keypress

    xor ax, ax
    int 0x19                                    ; Reboot the system

    hlt
    
  .diskErrorMsg db "Error while loading the bios paramater block!", 10, 13, 0
  .errorMsg     db "The operating system has halted.", 10, 13
                db "Please press any key to reboot.", 0

;---------------------------------------------------
; Included files
;---------------------------------------------------

%include "src\_succ.inc"
%include "src\disk.asm"
%include "src\fat.asm"
%include "src\string.asm"
%include "src\cli.asm"
%include "src\video.asm"
%include "src\keyboard.asm"
%include "src\memory.asm"    

;---------------------------------------------------
; Main kernel varables below
;---------------------------------------------------
    
__CURRENT_BUILD db "SuccOS kernel ", __SUCC_VERSION," [compiled on ", __DATE__, "]", 10, 13, 10, 13, 0
    
__GPL_NOTICE    db "Copyright (c) 2017-2019 Joshua Riek", 10, 13,
                db "This program comes with ABSOLUTELY NO WARRANTY; for details", 10, 13
                db "type 'warranty'. This is free software, and you are welcome to", 10, 13
                db "redistribute it under certain conditions; type 'redistrib'", 10, 13
                db "for details.", 10, 13, 10, 13, 0
      
__GPL_REDISTRIB db "This program is free software: you can redistribute it and/or modify", 10, 13,
                db "it under the terms of the GNU General Public License as published by", 10, 13,
                db "the Free Software Foundation, either version 3 of the License, or", 10, 13,
                db "(at your option) any later version.", 10, 13, 0
    
__GPL_WARRANTY  db "This program is distributed in the hope that it will be useful,", 10, 13,
                db "but WITHOUT ANY WARRANTY; without even the implied warranty of", 10, 13,
                db "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the", 10, 13,
                db "GNU General Public License for more details.", 10, 13, 0
    foos db "HAMLET  TXT"
    fo0s db "FOO     TXT"
driveGets:

    
    mov bx, 16
    mov cl, 0
    mov ch, 0

    mov si, _1
    call videoWriteStr
    mov ax, word [bytesPerSector]
    mov dx, 0
    call videoWriteNumPadding32
    
    mov si, _2
    call videoWriteStr
    xor ah, ah
    mov al, byte [sectorsPerCluster]
    mov dx, 0
    call videoWriteNumPadding32
    
    mov si, _3
    call videoWriteStr
    mov ax, word [reservedSectors]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _4
    call videoWriteStr
    xor ah, ah
    mov al, byte [fats]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _5
    call videoWriteStr
    mov ax, word [rootDirEntries]
    mov dx, 0
    call videoWriteNumPadding32
    
    mov si, _6
    call videoWriteStr
    mov ax, word [sectors]
    mov dx, 0
    call videoWriteNumPadding32
    
    mov si, _7
    call videoWriteStr
    xor ah, ah
    mov al, byte [mediaType]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _8
    call videoWriteStr
    mov ax, word [fatSectors]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _9
    call videoWriteStr
    mov ax, word [sectorsPerTrack]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _10
    call videoWriteStr
    mov ax, word [heads]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _11
    call videoWriteStr
    mov ax, word [hiddenSectors]
    mov dx, word [hiddenSectors+2]
    call videoWriteNumPadding32
    
    mov si, _12
    call videoWriteStr
    mov ax, word [hugeSectors]
    mov dx, word [hugeSectors+2]
    call videoWriteNumPadding32

    mov si, _13
    call videoWriteStr
    xor ah, ah
    mov al, byte [driveNum]
    mov dx, 0
    call videoWriteNumPadding32    

    mov si, _14
    call videoWriteStr
    xor ah, ah
    mov al, byte [reserved]
    mov dx, 0
    call videoWriteNumPadding32

    mov si, _15
    call videoWriteStr
    xor ah, ah
    mov al, byte [bootSignature]
    mov dx, 0
    call videoWriteNumPadding32

    ret
    

_1 db  10, 13, "bytesPerSector: ", 0                         ; dw
_2 db  10, 13, "sectorsPerCluster: ", 0                      ; db
_3 db  10, 13, "reservedSectors: ", 0                        ; dw
_4 db  10, 13, "fats: ", 0                                   ; db
_5 db  10, 13, "rootDirEntries: ", 0                         ; dw
_6 db  10, 13, "sectors: ", 0                                ; dw
_7 db  10, 13, "mediaType: ", 0                              ; db
_8 db  10, 13, "fatSectors: ", 0                             ; dw
_9 db  10, 13, "sectorsPerTrack: ", 0                        ; dw
_10 db 10, 13, "heads: ", 0                                  ; dw
_11 db 10, 13, "hiddenSectors: ", 0                          ; dd
_12 db 10, 13, "hugeSectors: ", 0                            ; dd
_13 db 10, 13, "driveNum: ", 0                               ; db
_14 db 10, 13, "reserved: ", 0                               ; db
_15 db 10, 13, "bootSignature: ", 0                          ; db
 
