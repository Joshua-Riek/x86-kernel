;  dos.asm
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

setupInt0x21:
    push ax                                     ; Save registers
    push es

    xor ax, ax                                  ; Clear ax
    mov es, ax                                  ; Set extra segment to zero

    cli
    mov word [es:0x21*4], int0x21               ; Offset of interupt handler
    mov word [es:0x21*4+2], cs                  ; Segment of interupt handler
    sti

    pop es                                      ; Restore registers
    pop ax

    ret

int0x21:
    cmp ah, 0x01                                ; Read a char from standard input, with echo
    je ah_01
    cmp ah, 0x02                                ; Write a char to standard output
    je ah_02
    cmp ah, 0x03                                ; Read a char from first serial port
    je ah_03
    cmp ah, 0x04                                ; Write a char to first serial port
    je ah_04
;   cmp ah, 0x05                                ; Write char to printer
;   je ah_05
    cmp ah, 0x06                                ; Direct console input or output
    je ah_06
    cmp ah, 0x07                                ; Char input without echo
    je ah_07
    cmp ah, 0x08                                ; Char input without echo
    je ah_07
    cmp ah, 0x09                                ; Write string to standard output
    je ah_09
    cmp ah, 0x0a                                ; Capture input to string 
    je ah_0a
    cmp ah, 0x0b                                ; Check standard input status
    je ah_0b
    cmp ah, 0x0c                                ; Clear kbd buffer, invoke kbd function
    je ah_0c
;   cmp ah, 0x0d                                ; Reset disk (writes modified disk buffers to disk)
;   je ah_0d
;   cmp ah, 0x0e                                ; Select disk
;   je ah_0e
    ;cmp ah, 0x0f                                ; Open file using FCB
    ;je ah_0f
    ;cmp ah, 0x10                                ; Close file using FCB
    ;je ah_10
    ;cmp ah, 0x11                                ; Find first matching file using FCB
    ;je ah_11
    ;cmp ah, 0x12                                ; Find next matching file using FCB
    ;je ah_12
    ;cmp ah, 0x13                                ; Delete file using FCB
    ;je ah_13
    ;cmp ah, 0x14                                ; Sequential read from FCB file
    ;je ah_14
    ;cmp ah, 0x15                                ; Sequential write to FCB file
    ;je ah_15
    ;cmp ah, 0x16                                ; Create file using FCB
    ;je ah_16
    ;cmp ah, 0x17                                ; Rename file using FCB
    ;je ah_17
    ;cmp ah, 0x18                                ; Null function for CP/M compatibility
    ;je ah_18
    ;cmp ah, 0x19                                ; Get current default drive
    ;je ah_19
    ;cmp ah, 0x1a                                ; Set disk transfer area address
    ;je ah_1a
    ;cmp ah, 0x1b                                ; Get allocation information for default drive
    ;je ah_1b
    ;cmp ah, 0x1c                                ; Get allocation information for specific drive
    ;je ah_1c
    ;cmp ah, 0x1d                                ; Null function for CP/M compatibility
    ;je ah_1d
    ;cmp ah, 0x1e                                ; Null function for CP/M compatibility
    ;je ah_1e
    ;cmp ah, 0x1f                                ; Get drive parameter block for default drive
    ;je ah_1f
    ;cmp ah, 0x20                                ; Null function for CP/M compatibility
    ;je ah_20
    ;cmp ah, 0x21                                ; Read random record from FCB file
    ;je ah_21
    ;cmp ah, 0x22                                ; Write random record to FCB file
    ;je ah_22
    ;cmp ah, 0x23                                ; Get file size for FCB
    ;je ah_23
    ;cmp ah, 0x24                                ; Set random record number for FCB
    ;je ah_24
    ;cmp ah, 0x25                                ; Set interrupt vector
    ;je ah_25
    ;cmp ah, 0x26                                ; Create new program segment prefix
    ;je ah_26
    ;cmp ah, 0x27                                ; Random block read from FCB file
    ;je ah_27
    ;cmp ah, 0x28                                ; Random block write to FCB file
    ;je ah_28
    ;cmp ah, 0x29                                ; Parse filename into FCB
    ;je ah_29
    ;cmp ah, 0x2a                                ; Get system date
    ;je ah_2a
    ;cmp ah, 0x2b                                ; Set system date
    ;je ah_2b
    ;cmp ah, 0x2c                                ; Get time
    ;je ah_2c
    ;cmp ah, 0x2d                                ; Set time
    ;je ah_2d
    ;cmp ah, 0x2e                                ; Set/reset verify switch
    ;je ah_2e
    ;cmp ah, 0x2f                                ; Get disk transfer address
    ;je ah_2f
    ;cmp ah, 0x30                                ; Get DOS version number
    ;je ah_30
    ;cmp ah, 0x31                                ; Terminate process and remain resident
    ;je ah_31
    ;cmp ah, 0x32                                ; Get pointer to drive param table
    ;je ah_32
    ;cmp ah, 0x33                                ; Get/set Ctrl-Break check state and get boot drive
    ;je ah_33
    ;cmp ah, 0x34                                ; Get address to DOS critical flag
    ;je ah_34
    ;cmp ah, 0x35                                ; Get vector
    ;je ah_35
    ;cmp ah, 0x36                                ; Get disk free space
    ;je ah_36
    ;cmp ah, 0x37                                ; Get/set switch character
    ;je ah_37
    ;cmp ah, 0x38                                ; Get/set country dependant information
    ;je ah_38
    ;cmp ah, 0x39                                ; Create subdirectory (mkdir)
    ;je ah_39
    ;cmp ah, 0x3a                                ; Remove subdirectory (rmdir)
    ;je ah_3a
    ;cmp ah, 0x3b                                ; Change current subdirectory (chdir)
    ;je ah_3b

    ;cmp ah, 0x3c
    ;je createFile
    ;cmp ah, 0x3d
    ;je openFile
    ;cmp ah, 0x3e
    ;je closeFile
    ;cmp ah, 0x3f
    ;je readFile
    ;cmp ah, 0x41
    ;je removeFile
    ;cmp ah, 0x42
    ;je seekFile

    ;cmp ah, 0x4e                                ; Find first matching file
    ;je ah_4e
    
    ;cmp ah, 0x56
    ;je renameFile

    iret

;---------------------------------------------------
ah_01:
;
; Read character from standard input, with echo,
; result is stored into al. Waits untill a key is
; pressed.
;
; Expects: AH    = 0x01
;
; Returns: AL    = Character from keypress
;
;---------------------------------------------------
    push dx                                     ; Save register
    mov dh, ah

    mov ah, 0x07                                ; Wait for keyboard input
    int 0x21                                    ; Call to DOS services

    mov dl, al                                  ; Character to display
    mov ah, 0x02                                ; Write a char to standard output
    int 0x21                                    ; Call to DOS services

    mov ah, dh
    pop dx                                      ; Restore register
    iret

;---------------------------------------------------
ah_02:
;
; Write a character to the standard output.
;
; Expects: AH    = 0x02
;          DL    = Character to write
;
; Returns: AL    = DL
;
;---------------------------------------------------    
    mov al, dl                                  ; Write the char to the standard output
    call videoWriteChar

    iret

;---------------------------------------------------
ah_03:
;
; Read character from serial port, result is stored
; into al. Waits untill data is available.
;
; Expects: AH    = 0x03
;
; Returns: AL    = Character read
;
;---------------------------------------------------
    push dx                                     ; Save register
    mov dh, ah

    call serialRead                             ; Read from the serial port

    mov ah, dh
    pop dx                                      ; Restore register
    iret

;---------------------------------------------------
ah_04:
;
; Write a character to the standard output.
;
; Expects: AH    = 0x04
;          DL    = Character to write
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save register

    mov al, dl
    call serialWrite                            ; Write to the serial port

    pop ax                                      ; Restore register
    iret

;---------------------------------------------------
ah_06:
;
; Write a character to the standard output, or
; request a chacacter from the standard input.
;
; Expects: AH    = 0x06
;          DL    = 0x00-0xfe character to output
;                = 0xff if console input request
;
; Returns: AL    = Input character if console input request (DL=0xff)
;          ZF    = 0x00 if console request character in al
;                = 0x01 if no character was ready for input
;
;---------------------------------------------------
    cmp dl, 0xff                                ; Check for console input request
    jne ah_02

    push dx                                     ; Save register
    mov dh, ah

    call kbdBiosGetChar                          ; Check the bios keyboard buffer and return in ax

    mov ah, dh
    pop dx                                      ; Restore register

    or al, al                                   ; Set the zero flag :)
    iret

;---------------------------------------------------
ah_07:
;
; Wait for a character to be returned directly form
; accessing the standard input, without echo.
;
; Expects: AH    = 0x07
;
; Returns: AL    = Character from input request
;
;---------------------------------------------------
    push dx                                     ; Save register
    mov dh, ah

  .wait:
    mov ah, 0x06                                ; Direct console input or output
    mov dl, 0xff                                ; Request input
    int 0x21                                    ; Call to DOS services
    or al, al
    jz .wait                                    ; When empty, just loop forever and ever :)

    mov ah, dh
    pop dx                                      ; Restore register
    iret

;---------------------------------------------------
ah_09:
;
; Write a string to the standard output, must be
; terminated by '$' or a null byte.
;
; Expects: AH    = 0x09
;          DS:DX = String to display 
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push dx
    push si

    mov si, dx                                  ; Move the string from ds:dx, to ds:si

  .loop:
    mov dl, byte [ds:si]                        ; Load byte from si to al
    inc si                                      ; Increase offset into string
    cmp dl, 0                                   ; If al is empty stop looping
    je .done                                    ; Done looping and return
    cmp dl, '$'                                 ; DOS string termination character
    je .done                                    ; If equal, stop looping
    mov ah, 0x02                                ; Write a char to standard output
    int 0x21                                    ; Call to DOS services
    jmp .loop

  .done:
    pop si                                      ; Restore registers
    pop dx
    pop ax

    iret
  
;---------------------------------------------------
ah_0a:
;
; Get the input from the keyboard and fill into the buffer
; in ds:dx, first byte is the buffer size, second is the
; number of chars actually read.
;
;
; Expects: AH    = 0x0a
;          DS:DX = Input buffer.
;
; Returns: DS:DX = Updated input buffer.
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push dx
    push si

    mov si, dx                                  ; Move the input buffer into si
    
  .loop:
    mov ah, 0x07                                ; Char input without echo
    int 0x21                                    ; Call to DOS services
    
    cmp al, 0x0d                                ; Check for a carriage return
    je .done
    cmp al, 0x0a                                ; Check for a line feed
    je .done
    cmp al, 0x08                                ; Check for a back space
    je .backspace

    mov bl, byte [ds:si+1]                      ; Grab the number of characters read
    cmp bl, byte [ds:si+0]                      ; Is the cursor X pos is greater than the chars read?
    jge .loop                                   ; This forces input to loop untill enter or back space

    mov dl, al                                  ; Move the character into dl
    mov ah, 0x02                                ; Write a char to standard output
    int 0x21                                    ; Call to DOS services
    
    call videoUpdateCur                         ; Update the cursor

    xor bh, bh
    mov bl, byte [ds:si+1]                      ; Grab the number of chars read
    mov byte [ds:si+bx+2], dl                   ; Move the char into the buffer 
    inc byte [ds:si+1]                          ; Increase character counter
    jmp .loop

  .backspace:
    cmp byte [ds:si+1], 0                       ; If char count is zero, backspace is not allowed
    je .loop
    dec byte [ds:si+1]                          ; Decrease character counter
    mov ah, 0x02                                ; Write a char to standard output
    mov dl, 0x08                                ; Write a back space (move cursor X pos back by one)
    int 0x21                                    ; Call to DOS services
    mov dl, 0x20                                ; Write a white space
    int 0x21                                    ; Call to DOS services
    mov dl, 0x08                                ; Write a back space (move cursor X pos back by one)
    int 0x21                                    ; Call to DOS services

    call videoUpdateCur                         ; Now update cursor pos

    jmp .loop                                   ; Go back to the input loop

  .done:
    pop si                                      ; Restore registers
    pop dx
    pop bx
    pop ax

    iret
 
;---------------------------------------------------
ah_0b:
;
; Check the status of the input buffer, to tell 
; if their is an available character or not.
;
; Expects: AH    = 0x0b
;
; Returns: AL    = 0x00 if no character available
;                = 0xff if character available
;
;---------------------------------------------------
    push dx                                     ; Save register
    mov dh, ah

    call kbdBiosCheckBuffer                     ; Check the bios kbd buffer for a key

    or al, al                                   ; See if al is equal to zero
    jz .done                                    ; If so, skip and return zero

    mov al, 0xff                                ; Return that a character is availble

  .done:
    mov ah, dh
    pop dx                                      ; Restore register
    iret

;--------------------------------------------------
ah_0c:
;
; Flush the keyboard buffer and read the standard
; input.
;
; Expects: AH    = 0x0c
;          AL    = 0x01, 0x06, 0x07, 0x08, 0x0a
;
; Returns: See return values for int 0x21, when al is
;          equal to 0x01, 0x06, 0x07, 0x08, 0x0a.
;
;--------------------------------------------------

    call kbdBiosFlushBuffer                     ; Flush the bios kbd buffer 

    cmp al, 0x01                                ; Jump to the correct function
    je ah_01
    cmp al, 0x06
    je ah_06
    cmp al, 0x07
    je ah_07
    cmp al, 0x08
    je ah_07
    cmp al, 0x0a
    je ah_0a

    iret
