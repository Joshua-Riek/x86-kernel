;  keyboard.asm
;
;  This file handles the keyboard controller.
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
    
%define KBD_CTRL_DATA_PORT             0x60     ; Keyboard controller ports
%define KBD_CTRL_STATUS_PORT           0x64
%define KBD_CTRL_COMMAND_PORT          0x64

%define KBD_CTRL_STATUS_PARITY         0x80     ; Keyboard controller status mask
%define KBD_CTRL_STATUS_TIMEOUT        0x40
%define KBD_CTRL_STATUS_AUX_BUF        0x20
%define KBD_CTRL_STATUS_LOCKED         0x10
%define KBD_CTRL_STATUS_CMD_DATA       0x08
%define KBD_CTRL_STATUS_SYSTEM         0x04
%define KBD_CTRL_STATUS_IN_BUF         0x02
%define KBD_CTRL_STATUS_OUT_BUF        0x01

%define KBD_CTRL_CMD_READ              0x20     ; Common keyboard controller commands
%define KBD_CTRL_CMD_WRITE             0x60
%define KBD_CTRL_CMD_SELF_TEST         0xaa
%define KBD_CTRL_CMD_INTERFACE_TEST    0xab
%define KBD_CTRL_CMD_DISABLE           0xad
%define KBD_CTRL_CMD_ENABLE            0xae
%define KBD_CTRL_CMD_READ_IN_PORT      0xc0
%define KBD_CTRL_CMD_READ_OUT_PORT     0xd0
%define KBD_CTRL_CMD_WRITE_OUT_PORT    0xd1
%define KBD_CTRL_CMD_READ_TEST_INPUTS  0xe0
%define KBD_CTRL_CMD_SYSTEM_RESET      0xfe
%define KBD_CTRL_CMD_MOUSE_DISABLE     0xa7
%define KBD_CTRL_CMD_MOUSE_ENABLE      0xa8
%define KBD_CTRL_CMD_MOUSE_PORT_TEST   0xa9
%define KBD_CTRL_CMD_MOUSE_WRITE       0xd4 
    
%define PIC_MASTER_COMMAND             0x20
%define PIC_MASTER_DATA                0x21
%define PIC_SLAVE_COMMAND              0xa0
%define PIC_SLAVE_DATA                 0xa1

%define KBD_ENC_INPUT_BUF              0x60     ; Keyboard encoder commands
%define KBD_ENC_CMD_REG                0x60
%define KBD_ENC_CMD_SET_LED            0xed
%define KBD_ENC_CMD_ECHO               0xee
%define KBD_ENC_CMD_SCAN_CODE_SET      0xf0
%define KBD_ENC_CMD_ID                 0xf2
%define KBD_ENC_CMD_AUTODELAY          0xF3
%define KBD_ENC_CMD_ENABLE             0xf4
%define KBD_ENC_CMD_RESETWAIT          0xf5
%define KBD_ENC_CMD_RESETSCAN          0xf6
%define KBD_ENC_CMD_ALL_AUTO           0xf7
%define KBD_ENC_CMD_ALL_MAKEBREAK      0xf8
%define KBD_ENC_CMD_ALL_MAKEONLY       0xf9
%define KBD_ENC_CMD_ALL_MAKEBREAK_AUTO 0xfa
%define KBD_ENC_CMD_SINGLE_AUTOREPEAT  0xfb
%define KBD_ENC_CMD_SINGLE_MAKEBREAK   0xfc
%define KBD_ENC_CMD_SINGLE_BREAKONLY   0xfd
%define KBD_ENC_CMD_RESEND             0xfe
%define KBD_ENC_CMD_RESET              0xff
%define IO_DELAY nop

%define KBD_FLAGS_INSERT_MASK          0x80     ; Bit 7 = insert on
%define KBD_FLAGS_CAPS_MASK            0x40     ; Bit 6 = caps lock on
%define KBD_FLAGS_NUM_MASK             0x20     ; Bit 5 = num lock on
%define KBD_FLAGS_SCROLL_MASK          0x10     ; Bit 4 = scroll lock on
%define KBD_FLAGS_ALT_MASK             0x08     ; Bit 3 = alt key down
%define KBD_FLAGS_CTRL_MASK            0x04     ; Bit 2 = ctrl key down
%define KBD_FLAGS_RSHIFT_MASK          0x02     ; Bit 1 = left shift down
%define KBD_FLAGS_LSHIFT_MASK          0x01     ; Bit 0 = right shift down

kbdScanCodeTable db 0x00, 0x1b, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36
                 db 0x37, 0x38, 0x39, 0x30, 0x2d, 0x3d, 0x08, 0x09
                 db 0x71, 0x77, 0x65, 0x72, 0x74, 0x79, 0x75, 0x69
                 db 0x6f, 0x70, 0x5b, 0x5d, 0x0d, 0x00, 0x61, 0x73
                 db 0x64, 0x66, 0x67, 0x68, 0x6a, 0x6b, 0x6c, 0x3b
                 db 0x27, 0x60, 0x00, 0x5c, 0x7a, 0x78, 0x63, 0x76
                 db 0x62, 0x6e, 0x6d, 0x2c, 0x2e, 0x2f, 0x00, 0x2a
                 db 0x00, 0x20, 0x00, 0x0f, 0x10, 0x11, 0x12, 0x13
                 db 0x14, 0x15, 0x16, 0x17, 0x18, 0x00, 0x00, 0x86
                 db 0x81, 0x85, 0xad, 0x83, 0x9f, 0x84, 0xab, 0x8b
                 db 0x82, 0x8c, 0x8e, 0xff, 0x1c, 0x00, 0x00, 0x19
                 db 0x1a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x0e, 0x0b, 0x02, 0x0c, 0x03, 0x00, 0x04, 0x06
                 db 0x01, 0x05, 0x7f, 0xaf, 0x8d, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    
kbdCtrlTable     db 0x00, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1e
                 db 0x00, 0x00, 0x00, 0x00, 0x1f, 0x00, 0x7f, 0x0f
                 db 0x11, 0x17, 0x05, 0x00, 0x00, 0x19, 0x15, 0x09
                 db 0x0f, 0x10, 0x1b, 0x1d, 0x0a, 0x00, 0x01, 0x13
                 db 0x04, 0x06, 0x07, 0x08, 0x0a, 0x0b, 0x0c, 0x00
                 db 0x00, 0x00, 0x00, 0x1c, 0x1a, 0x18, 0x03, 0x16
                 db 0x02, 0x0e, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x20, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0xe0, 0xe0, 0xe0, 0xe0, 0xe0
                 db 0x00, 0x00, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
                 db 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00
                 db 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00
                 db 0x00, 0xe0, 0x0e, 0x0e, 0x00, 0x00, 0x00, 0x00
                 db 0x00
    
kbdShiftTable    db 0x00, 0x38, 0x32, 0x34, 0x36, 0x39, 0x37, 0x07
                 db 0x08, 0x09, 0x0a, 0x31, 0x33, 0x0d, 0x30, 0x0f
                 db 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17
                 db 0x18, 0x18, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x35
                 db 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x22
                 db 0x28, 0x29, 0x2a, 0x2b, 0x3c, 0x5f, 0x3e, 0x3f
                 db 0x29, 0x21, 0x40, 0x23, 0x24, 0x25, 0x5e, 0x26
                 db 0x2a, 0x28, 0x3a, 0x3a, 0x3c, 0x2b, 0x3e, 0x3f
                 db 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47
                 db 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f
                 db 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57
                 db 0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x5e, 0x5f
                 db 0x7e, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47
                 db 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f
                 db 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57
                 db 0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x2e

         lastKey dw 0
        kbdFlags db 0

;---------------------------------------------------
setupKbdCtrl:
;
; Replace the current keyboard related
; interrupt (IRQ1) with my custom handler.
;
; Expects: Nothing
;
; Returns: CF    = Carry flag set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push es

    xor ax, ax                                  ; Clear ax
    mov es, ax                                  ; Set extra segment to zero
    
    cli
    mov word [es:0x09*4], kbdCtrlHandler        ; Offset of interupt handler
    mov word [es:0x09*4+2], cs                  ; Segment of interupt handler
    sti

    call kbdCtrlSelfTest                        ; Run a keyboard self test, result in carry flag

    pop es                                      ; Restore registers
    pop ax
    ret
    
;---------------------------------------------------
kbdCtrlRead:
;
; Read a byte from the keyboard controller.
;    
; Expects: Nothing
;
; Returns: AL    = Byte from read
;
;--------------------------------------------------- 
  .wait:                                        ; Read from the status port and check the
    in al, KBD_CTRL_STATUS_PORT                 ; status of the output buffer, must wait untill
    IO_DELAY                                    ; its ready to read
    test al, KBD_CTRL_STATUS_OUT_BUF
    jz .wait

    in al, KBD_CTRL_DATA_PORT                   ; Now read the byte from the data port
    IO_DELAY
    
    ret
    
;---------------------------------------------------
kbdCtrlWrite:
;
; Send a byte to the keyboard controller.
;    
; Expects: AL    = Byte to write
;
; Returns: Nothing
;
;---------------------------------------------------
    push dx

    mov dl, al
    
  .wait:                                        ; Read from the status port and check the
    in al, KBD_CTRL_STATUS_PORT                 ; status of the output buffer, must wait untill
    IO_DELAY                                    ; its ready to read
    test al, KBD_CTRL_STATUS_OUT_BUF
    jz .wait

    mov al, dl
    out KBD_CTRL_DATA_PORT, al                  ; Send the byte to the data port
    IO_DELAY

    pop dx
    ret
    
;---------------------------------------------------
kbdCtrlSendCmd:
;
; Send a command to the keyboard controller.
;    
; Expects: BL    = Command byte
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax

  .wait:                                        ; Read from the status port and check the
    in al, KBD_CTRL_STATUS_PORT                 ; status of the input buffer, must wait untill
    IO_DELAY                                    ; its ready to accept a command
    test al, KBD_CTRL_STATUS_IN_BUF
    jnz .wait

    mov al, bl                                  ; Grab the command to send to the kbd ctrl
    out KBD_CTRL_COMMAND_PORT, al               ; Send the command via command port
    IO_DELAY
    
  .accept:                                      ; Same as kbdCtrlSendCmd.wait, this is just 
    in al, KBD_CTRL_STATUS_PORT                 ; a safeguard to ensure that the kbd ctrl has
    IO_DELAY                                    ; accepted the command sent
    test al, KBD_CTRL_STATUS_IN_BUF
    jnz .accept

    pop ax
    ret
    
;---------------------------------------------------
kbdCtrlSelfTest:
;
; Preform a self test of the keyboard controller.
;
; Expects: Nothing
;
; Returns: CF    = Carry set on error
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx

    cli                                         ; Clear interrupts
    mov bl, KBD_CTRL_CMD_SELF_TEST              ; Send the self test command to the kbd controller
    call kbdCtrlSendCmd

  .wait:
    in al, KBD_CTRL_STATUS_PORT                 ; Read from the status port
    IO_DELAY                                    ; Test to see if the input buffer is now full
    and al, KBD_CTRL_STATUS_OUT_BUF
    cmp al, 1
    jne .wait

    in al, KBD_ENC_INPUT_BUF                    ; Read the keyboard encoder input buffer
    IO_DELAY                                    ; Test for the byte 0x55
    cmp al, 0x55
    jne .error

    sti                                         ; Restore interrupts
    clc                                         ; Clear carry, no errors
    
    pop bx                                      ; Restore registers
    pop ax
    ret

  .error:
    sti                                         ; Restore interrupts
    stc                                         ; Set carry, test failed
    
    pop bx                                      ; Restore registers
    pop ax
    ret
    
;---------------------------------------------------
kbdCtrlSetLeds:
;
; Set the keyboard leds.
;    
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push ds

    mov bx, cs                                  ; Correct the data segment 
    mov ds, bx

    mov al, byte [kbdFlags]                     ; Get the current keyboard flags
    shr al, 1                                   ; Shift to lower three bits
    shr al, 1
    shr al, 1
    shr al, 1
    mov bl, KBD_ENC_CMD_SET_LED                 ; Send the led write command to the kbd encoder
    call kbdEncSendCmd

    mov bl, al                                  ; Send the led state to the kbd encoder
    call kbdEncSendCmd

    pop ds                                      ; Restore registers
    pop bx
    pop ax
    ret
        
;---------------------------------------------------
kbdCtrlWaitUntillKey:
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
    push bx                                     ; Save registers
    push ds

    mov bx, cs                                  ; Correct the data segment 
    mov ds, bx

    mov bl, KBD_CTRL_CMD_ENABLE                 ; Enable the kbd controller
    call kbdCtrlSendCmd

    mov bx, word [lastKey]
  .wait:                                        ; Compare the last and most
    mov ax, word [lastKey]                      ; recent keypress
    cmp ax, bx
    je .wait
    
    mov word [lastKey], 0                       ; Remove the last key

    mov bl, KBD_CTRL_CMD_DISABLE                ; Disable the kbd controller
    call kbdCtrlSendCmd
    
    pop ds                                      ; Restore registers
    pop bx
    ret
       
;---------------------------------------------------
kbdCtrlCaptureInput:
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
    call kbdCtrlWaitUntillKey                   ; Wait for a keypress    

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
    ;dec byte [curX]
    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
    call videoWriteChar
    mov al, 0x20                                ; Write a white space
    call videoWriteChar
    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
    call videoWriteChar
    ;dec byte [curX]
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
    
;---------------------------------------------------
kbdCtrlHandler:
;
; This is a custom interrupt service/ handler for
; the keyboard controller.
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

    mov bx, cs                                  ; Correct the data segment 
    mov ds, bx

    in al, KBD_CTRL_DATA_PORT                   ; Now read the byte from the data port
    IO_DELAY
    
    mov ah, al
    and ah, 0x80                                ; Check the scan code to see if its a break code (bit 7 is set)
    jz .scanCode

  .breakCode:
    sub al, 0x80                                ; Convert break code to its make code
    
    cmp al, 0x1d                                ; Check for the left ctrl scan code
    je .clearCtrlState
    cmp al, 0x1d                                ; Check for the right ctrl scan code (0xe0, 0x1d)
    je .clearCtrlState
    cmp al, 0x2a                                ; Check for the left shift scan code
    je .clearLShiftState
    cmp al, 0x36                                ; Check for the right shift scan code
    je .clearRShiftState
    cmp al, 0x38                                ; Check for the left alt scan code
    je .clearAltState
    cmp al, 0x38                                ; Check for the right alt scan code (0xe0, 0x38)
    je .clearAltState

    jmp .done
    
  .clearCtrlState:                              ; Clear the ctrl bit
    and byte [kbdFlags], ~(KBD_FLAGS_CTRL_MASK)
    jmp .done
    
  .clearLShiftState:                            ; Clear the left shift bit
    and byte [kbdFlags], ~(KBD_FLAGS_LSHIFT_MASK)
    jmp .done
    
  .clearRShiftState:                            ; Clear the right shift bit
    and byte [kbdFlags], ~(KBD_FLAGS_RSHIFT_MASK)
    jmp .done
    
  .clearAltState:                               ; Clear the alt bit
    and byte [kbdFlags], ~(KBD_FLAGS_ALT_MASK)
    jmp .done  
    
  .scanCode:
    cmp al, 0x1d                                ; Check for the left ctrl scan code
    je .setCtrlState
    cmp al, 0x1d                                ; Check for the right ctrl scan code (0xe0, 0x1d)
    je .setCtrlState
    cmp al, 0x2a                                ; Check for the left shift scan code
    je .setLShiftState
    cmp al, 0x36                                ; Check for the right shift scan code
    je .setRShiftState
    cmp al, 0x38                                ; Check for the left alt scan code
    je .setAltState
    cmp al, 0x38                                ; Check for the right alt scan code (0xe0, 0x38)
    je .setAltState
    cmp al, 0x3a                                ; Check for the caplock scan code
    je .toggleCapslock 
    cmp al, 0x45                                ; Check for the numlock scan code
    je .toggleNumlock
    cmp al, 0x46                                ; Check for the scrlock scan code
    je .toggleScrlock
    
    call scanToAscii                            ; Now, convert to ascii
    mov word [lastKey], ax                      ; Store the last scan and ascii code

    jmp .done
    
  .setCtrlState:                                ; Set the ctrl state bit
    or byte [kbdFlags], KBD_FLAGS_CTRL_MASK
    jmp .done
    
  .setLShiftState:                              ; Set the left shift state bit
    or byte [kbdFlags], KBD_FLAGS_LSHIFT_MASK
    jmp .done

  .setRShiftState:                              ; Set the right shift state bit
    or byte [kbdFlags], KBD_FLAGS_RSHIFT_MASK
    jmp .done
    
  .setAltState:                                 ; Set the alt state bit
    or byte [kbdFlags], KBD_FLAGS_ALT_MASK
    jmp .done
    
  .toggleCapslock:                              ; Toggle the capslock flag bit
    xor byte [kbdFlags], KBD_FLAGS_CAPS_MASK
    call kbdCtrlSetLeds                         ; Set kbd leds
    jmp .done

  .toggleNumlock:                               ; Toggle the numlock flag bit
    xor byte [kbdFlags], KBD_FLAGS_NUM_MASK
    call kbdCtrlSetLeds                         ; Set kbd leds
    jmp .done
    
  .toggleScrlock:                               ; Toggle the numlock flag bit
    xor byte [kbdFlags], KBD_FLAGS_SCROLL_MASK
    call kbdCtrlSetLeds                         ; Set kbd leds

  .done:
    mov al, PIC_MASTER_COMMAND
    out PIC_MASTER_COMMAND, al                  ; Acknowlege the interrupt to the PIC
    IO_DELAY
    
    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    iret

;---------------------------------------------------
kbdEncSendCmd:
;
; Send a command to the keyboard encoder.
;    
; Expects: BL    = Command byte
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax

  .wait:                                        ; Read from the status port and check the
    in al, KBD_CTRL_STATUS_PORT                 ; status of the input buffer, must wait untill
    IO_DELAY                                    ; its ready to accept a command
    test al, KBD_CTRL_STATUS_IN_BUF
    jnz .wait

    mov al, bl                                  ; Grab the command to send to the kbd ctrl
    out KBD_ENC_CMD_REG, al                     ; Send the command via command port
    IO_DELAY
    
  .accept:                                      ; Same as kbdCtrlSendCmd.wait, this is just 
    in al, KBD_CTRL_STATUS_PORT                 ; a safeguard to ensure that the kbd ctrl has
    IO_DELAY                                    ; accepted the command sent
    test al, KBD_CTRL_STATUS_IN_BUF
    jnz .accept

    pop ax
    ret

;---------------------------------------------------
scanToAscii:
;
; Convert the passed scan code into it's correct
; ascii code, checks status of keyboard flags.
;    
; Expects: AL    = Scan code
;
; Returns: AH    = Scan code
;          AL    = Ascii code
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push ds

    mov bx, cs                                  ; Correct the data segment so we can refrence shit
    mov ds, bx

    mov ah, al                                  ; Save the scan code into ah
    xor bh, bh                                  ; Get the scan code into bl for index
    mov bl, ah
    mov al, byte [kbdScanCodeTable+bx]          ; Grab the acsii scan code from the table

    test al, 0x80                               ; Check for the high bit
    jz .checkShift

  .checkShift:
    test byte [kbdFlags], KBD_FLAGS_RSHIFT_MASK ; Check for the right shift bit
    jnz .checkShift2
    test byte [kbdFlags], KBD_FLAGS_LSHIFT_MASK ; Check for the left shift bit
    jnz .checkShift2
    jmp .checkNumlock
    
  .checkShift2:
    cmp al, 0x21                                ; Check if the key is < ' '
    jb .checkNumlock
    cmp al, 0x7e                                ; Check if the key is > '~'
    ja .checkNumlock
    jmp .doShift
    
  .checkNumlock:
    test al, 0x80                               ; Test for the high bit
    jz .checkCapslock
    test byte [kbdFlags], KBD_FLAGS_NUM_MASK    ; Check for the num lock bit
    jz .checkCapslock
    cmp al, 0xad                                ; Dont shift if key is '-'
    je .checkCapslock
    jmp .doShift
    
  .checkCapslock:
    test byte [kbdFlags], KBD_FLAGS_CAPS_MASK   ; Check for the caps lock bit
    jz .checkCtrl
    cmp al, 0x61                                ; Check if the key is >= 'a'
    jb .checkCtrl
    cmp al, 0x7a                                ; Check if the key is <= 'z'
    ja .checkCtrl
      
  .doShift:
    and ax, 0x7f
    mov ah, al                                  ; Save the scan code into ah
    xor bh, bh                                  ; Get the scan code into bl for index
    mov bl, ah
    mov al, byte [kbdShiftTable+bx]             ; Get the ascii char from the shift table
    
  .checkCtrl:
    test byte [kbdFlags], KBD_FLAGS_CTRL_MASK   ; Check for the ctrl bit
    jz .checkAlt
    xor bh, bh                                  ; Get the scan code into bl for index
    mov bl, ah
    mov al, byte [kbdCtrlTable+bx]              ; Grab the byte from the table
    jmp .done

  .checkAlt:
    test byte [kbdFlags], KBD_FLAGS_ALT_MASK    ; Check for the alt bit
    jz .done 
    xor al, al                                  ; Set the ascii char to zero

  .done:
    and al, 0x7f                                ; Clear the highest bit
    
    pop ds
    pop bx                                      ; Restore registers
    ret

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
kbdBiosFlushBuffer:
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
kbdBiosCheckBuffer:
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
kbdBiosGetChar:
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
kbdBiosStoreKey:
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
kbdBiosWaitUntillKey:
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
    call kbdBiosFlushBuffer                   ; Flush the most recent char (to be safe)

  .wait:
    call kbdBiosGetChar                           ; Try to read a key from the bios kbd buffer
    or ax, ax                                 ; When empty, just loop forever and ever :)
    jz .wait
    
    ret
    
;---------------------------------------------------
kbdBiosCaptureInput:
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
    call kbdBiosWaitUntillKey                   ; Wait for a keypress    

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
    ;dec byte [curX]
    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
    call videoWriteChar
    mov al, 0x20                                ; Write a white space
    call videoWriteChar
    mov al, 0x08                                ; Write a back space (move cursor X pos back by one)
    call videoWriteChar
    ;dec byte [curX]
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

    ; Ctrl break testing below
    ; looks like shit :)
    setupInt0x15:
    push ax                                     ; Save registers
    push es
    
    xor ax, ax                                  ; Clear ax
    mov es, ax                                  ; Set extra segment to zero
    
    cli
    push word [es:0x15*4]
    push word [es:0x15*4+2]
    pop word [oldInt0x15+2]
    pop word [oldInt0x15]
    
    mov word [es:0x15*4], int0x15Hook           ; Offset of interupt handler
    mov word [es:0x15*4+2], cs                  ; Segment of interupt handler
    sti

    pop es                                      ; Restore registers
    pop ax

    ret
    
int0x15Hook:
    pushf
    push ax
    push ds
    
    cmp ah, 0x4f                                ; Intercept translation of scan code
    jne .done

    cmp al, 0x2e                                ; Check for the 'C' character scan code
    jne .done
    
    mov ax, BIOS_SEG                            ; Set the data segment to the bios data area
    mov ds, ax

    test byte [ds:KEYBOARD_TOGGLE_FLAGS], 4     ; Check to see if ctrl is down
    jz .done
        push ax

    mov al, 'b'
    call writeSerial
        mov al, 13
    call writeSerial
            mov al, 10
    call writeSerial

    pop ax
    jmp .done2
.done:
    pop ds
    pop ax
    popf
    push word [cs:oldInt0x15+2]
    push word [cs:oldInt0x15]
    retf
.done2:
    pop ds
    pop ax
    popf
    retf 2

    oldInt0x15 dd 0
