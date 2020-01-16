;  keyboard.asm
;
;  This file contains simple BIOS keyboard functions.
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
    
%define BIOS_SEG                0x0040          ; (BIOS_SEG  << 4) + BIOS_OFF  = 0x0400
%define BIOS_OFF                0x0000
    
%define KEYBOARD_HEAD_PTR       0x001a          ; Next incoming key value in kbd buffer
%define KEYBOARD_TAIL_PTR       0x001c          ; Last key value in kbd buffer

%define KEYBOARD_TOGGLE_FLAGS   0x0017          ; Flags for the shift and toggle keys
%define KEYBOARD_CURRENT_FLAGS  0x0018          ; Current state of key positions for toggle and shift keys
%define KEYBOARD_ALT_SCRATCH    0x0019          ; Used for decimal nums with the alt key and num pad keys
%define KEYBOARD_CTRL_BREAK     0x0071          ; If the Ctrl-Break key is pressed bit 7 is set
    
%define KEYBOARD_BUFF_START     0x0080          ; Offset to beginning of the buffer (AT+ only)
%define KEYBOARD_BUFF_END       0x0082          ; Offset to end of the buffer (AT+ only)

%define KEYBOARD_STATUS         0x0096          ; Keyboard status info (AT+ only)
%define KEYBOARD_INTERNAL_FLAGS 0x0097          ; Flags for the kbd controller and LED states (AT+ only)

;---------------------------------------------------
kbdFlushBuffer:
;
; Flush the most recent key in the keyboard buffer.
;    
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push ds
    push ax
    cli                                         ; Clear interupts while changing things in the bios data area

    mov ax, BIOS_SEG                            ; Set the data segment to the bios data area
    mov ds, ax

    mov ax, word [ds:KEYBOARD_TAIL_PTR]         ; When the keyboard head pointer is equal to the
    mov word [ds:KEYBOARD_HEAD_PTR], ax         ; keyboard tail pointer their are no keys in the buffer
 
    sti
    pop ax
    pop ds
    
    ret

;---------------------------------------------------
kbdCheckBuffer:
;
; Check the keyboard buffer.
;    
; Expects: Nothing
;
; Returns: AX    = Zero when empty
;
;---------------------------------------------------
    push ds
    cli                                         ; Clear interupts while changing things in the bios data area

    mov ax, BIOS_SEG                            ; Set the data segment to the bios data area
    mov ds, ax

    mov ax, word [ds:KEYBOARD_HEAD_PTR]         ; Subtract the keyboard tail ptr by the head ptr
    sub ax, word [ds:KEYBOARD_TAIL_PTR]         ; When ax is equal to zero then the buffer is empty

    sti
    pop ds
    
    ret

;---------------------------------------------------
kbdGetChar:
;
; Get a character and scan code from the bios keyboard buffer,
; when the buffer is empty this returns zero, else it will return
; the character and scan code.
;    
; Expects: Nothing
;
; Returns: Ax    = Zero if no scan code is available
;          AH    = Scan code
;          AL    = Ascii code
;
;---------------------------------------------------
    push ds
    push bx
    push dx
    cli                                         ; Clear interupts while changing things in the bios data area
    
    xor dx, dx                                  ; Clear for divison
    xor ax, ax                                  ; Ensure ax is clear for the return value
    
    mov bx, BIOS_SEG                            ; Set the data segment to the bios data area
    mov ds, bx
    
    mov bx, word [ds:KEYBOARD_HEAD_PTR]         ; If the keyboard head pointer is equal to the
    cmp bx, word [ds:KEYBOARD_TAIL_PTR]         ; keyboard tail pointer their are no keys in the buffer
    jz .done

    push word [ds:bx]                           ; Save the scan code and ascii char on the stack
    mov ax, bx

    mov bx, word [ds:KEYBOARD_BUFF_END]         ; Subtracted the end and start of the keyboard buffer 
    sub bx, word [ds:KEYBOARD_BUFF_START]       ; This will dertermine the size of the buffer (normally a 16-word FIFO)
    
    add ax, 2                                   ; Point to the next char in the buffer 
    sub ax, word [ds:KEYBOARD_BUFF_START]       ; Calculate the offset for the next char

    div bx                                      ; Take the offset by the size of the buffer (need remander) 

    add dx, word [ds:KEYBOARD_BUFF_START]       ; Add the remander to the start of the buffer for a new char
    mov word [ds:KEYBOARD_HEAD_PTR], dx         ; The keyboard trail can now accept a new incoming key :3
    mov word [ds:KEYBOARD_TAIL_PTR], dx         ; The keyboard trail can now accept a new incoming key :3

    pop ax                                      ; Restore the scan code and ascii char 

  .done:
    pop dx
    pop bx
    pop ds
    sti

    ret

;---------------------------------------------------
kbdStoreKey:
;
; Shove a character and scan code into the bios 
; keyboard buffer, only when the buffer is not full.
;    
; Expects: CH    = Scan code
;          CL    = Ascii code
;
; Returns: AX    = 0 (not stored, no room in buffer)
;                = 1 (successfully stored)
;
;---------------------------------------------------
    push ds
    push bx
    push dx
    cli                                         ; Clear interupts while changing things in the bios data area
   
    xor dx, dx                                  ; Clear for divison
    xor ax, ax                                  ; Ensure ax is clear for the return value

    mov bx, BIOS_SEG                            ; Set the data segment to the bios data area
    mov ds, bx

    mov bx, word [ds:KEYBOARD_HEAD_PTR]         ; If the keyboard head pointer is equal to the
    cmp bx, word [ds:KEYBOARD_TAIL_PTR]         ; keyboard tail pointer their are no keys in the buffer
    jnz .done

    mov ax, bx                                  ; Move the tail ptr into ax for divison

    mov bx, word [ds:KEYBOARD_BUFF_END]         ; Subtracted the end and start of the keyboard buffer 
    sub bx, word [ds:KEYBOARD_BUFF_START]       ; This will dertermine the size of the buffer (normally a 16-word FIFO)
    
    sub ax, word [ds:KEYBOARD_BUFF_START]       ; Calculate the offset for the char

    div bx                                      ; Take the offset by the size of the buffer (need remander) 

    add dx, word [ds:KEYBOARD_BUFF_START]       ; Add the remander to the start of the buffer
    mov bx, dx

    mov word [ds:bx], cx                        ; Place the scancode and ascii char into the buffer
    
    add word [ds:KEYBOARD_TAIL_PTR], 2          ; A new key is in the buffer, so set the head to it

    mov ax, 1

  .done:
    pop dx
    pop bx
    pop ds
    sti

    ret

;---------------------------------------------------
kbdWaitUntillKey:
;
; Waits untill a key is pressed and then returns the
; ascii and scancode.
;    
; Expects: Nothing
;
; Returns: AH    = Scan code
;          AL    = Ascii code
;
;---------------------------------------------------
    call kbdFlushBuffer                   ; Flush the most recent char (to be safe)

  .wait:
    call kbdGetChar                           ; Try to read a key from the bios kbd buffer
    or ax, ax                                 ; When empty, just loop forever and ever :)
    jz .wait
    
    ret
    
;---------------------------------------------------
kbdCaptureInput:
;
; Get the input from the keyboard and fill
; into the buffer.
;
; Expects: DS:SI = Input buffer
;
; Returns: DS:SI = Updated with keys pressed
;
;---------------------------------------------------
    push ax
    push bx
    push cx
    push dx
    push si

    xor cl, cl                                  ; Ready for character counter

  .loop:
    call kbdWaitUntillKey                      ; Wait for a keypress    

    cmp al, 0x08                                ; Check for a back space
    je .backspace
    cmp al, 0x0d                                ; Check for a carriage return
    je .done
    cmp al, 0x0a                                ; Check for a line feed
    je .done
    cmp cl, 0x3f                                ; Check for input buffer limit
    je .loop

    call videoWriteChar                         ; Print out the character

    mov byte [ds:si], al                        ; Store the character into the buffer
    inc si                                      ; Increase buffer offset
    inc cl                                      ; Increase character counter
    jmp .loop

  .backspace:
    cmp cl, 0                                   ; If char count is zero, backspace is not allowed
    je .loop
    dec si                                      ; Decrease buffer offset
    mov byte [ds:si], 0                         ; Store a null byte into the buffer
    dec cl                                      ; Decrease character counter

    push ds
    push cs
    pop ds
    dec byte [curX]
;    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
;    call videoWriteChar
    mov al, 0x20                                ; Write a white space
    call videoWriteChar
;    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
;    call videoWriteChar
    dec byte [curX]
    call videoUpdateCur
    pop ds
    
    jmp .loop

  .done:
    mov byte [ds:si], 0                         ; Ensure string ends with a null byte
    
    mov al, 0x0d                                ; Print out a carriage return
    call videoWriteChar
    mov al, 0x0a                                ; Print out a line feed
    call videoWriteChar

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
