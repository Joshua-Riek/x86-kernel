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

    %define KERNEL_SEG 0x1000                   ; (KERNEL_SEG << 4) + KERNEL_OFF = 0x001000
    %define KERNEL_OFF 0x0000

    %ifidn __OUTPUT_FORMAT__, elf               ; WARNING: Assumes that the text segment is set to
      %define KERNEL_SEG 0x0000                 ; 0x1000, used ONLY for debugging with GDB
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

    call setupDisk                              ; Setup the disk manager
    jc .diskError

    call setupMemory

    mov si, __CURRENT_BUILD                     ; Get the address of the current build string
    call videoWriteStr                          ; Write string to standard output
    
    mov si, __GPL_NOTICE                        ; Get the address of the gpl3 header notice
    call videoWriteStr                          ; Write string to standard output
    
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
%include "src\keyboard.asm"
%include "src\string.asm"
%include "src\memory.asm"    
%include "src\serial.asm"
%include "src\video.asm"
%include "src\cmos.asm"
%include "src\disk.asm"
%include "src\test.asm"
%include "src\fat.asm"
%include "src\cli.asm"
    
;---------------------------------------------------
; Main kernel varables below
;---------------------------------------------------

__CURRENT_BUILD db "SuccOS kernel ", __SUCC_VERSION," [compiled on ", __DATE__, "]", 10, 13, 10, 13, 0
    
__GPL_NOTICE    db "Copyright (c) 2017-2020 Joshua Riek", 10, 13,
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

inits:
    mov ah, 0
    mov al, 11100011b
    int 0x14

    ; AH=bit 7 clear on success
sendSerial:
    mov ah, 0x01
    mov dx, 0
    int 0x14
    ret
    ; AH=bit 7 clear on success

getSerial:
    mov ah, 0x02
    mov dx, 0
    int 0x14

    ret
