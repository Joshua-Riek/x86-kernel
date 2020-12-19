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
main:
global entryPoint
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

    call setupVideo                             ; Grab the cursor pos and screen info from bios

    call setupInt0x21                           ; Setup my dos emulation
           
    call setupDisk                              ; Setup the disk manager
    jc .diskError

    call setupMemory                            ; Setup the memory manager
    jc .memError
    
    call setupKbdCtrl                           ; Setup the keyboard manager
    jc .kbdError

    mov si, __CURRENT_BUILD                     ; Get the address of the current build string
    call videoWriteStr
    
    mov si, __GPL_NOTICE                        ; Get the address of the gpl3 header notice
    call videoWriteStr

    jmp cliLoop                                 ; Go to the user command line

  .hang:
    hlt
    jmp .hang

  .memError:
    mov si, .memErrorMsg                        ; Tell the user that their was a mem error
    call videoWriteStr                          ; Write string to standard output
    jmp .reboot
        
  .kbdError:
    mov si, .kbdErrorMsg                        ; Tell the user that their was a kbd error
    call videoWriteStr                          ; Write string to standard output
    jmp .reboot

  .diskError:
    mov si, .diskErrorMsg                       ; Tell the user that their was a disk error
    call videoWriteStr                          ; Write string to standard output
    jmp .reboot

  .reboot:
    mov si, .errorMsg                           ; Handle any error starting up the system
    call videoWriteStr                          ; Write string to standard output

    call videoUpdateBiosCur                     ; Update the bios cursor before reboot
    
    call kbdCtrlWaitUntillKey                   ; Get a single keypress
    
    mov ah, 0x0e                                ; Teletype output
    mov al, 0x0d                                ; Carriage return
    int 0x10                                    ; Video interupt
    mov al, 0x0a                                ; Line feed
    int 0x10                                    ; Video interupt
    int 0x10                                    ; Video interupt

    xor ax, ax
    int 0x19                                    ; Reboot the system

    hlt

  .memErrorMsg  db "Error while getting the low system memory!", 10, 13, 0
  .kbdErrorMsg  db "Error preforming the keyboard self test!", 10, 13, 0
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
%include "src\math.asm"
%include "src\fat.asm"
%include "src\cli.asm"
%include "src\dos.asm"
    
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
