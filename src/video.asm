;  video.asm
;
;  Simple video driver.  
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
    
    curY       db 0                             ; Cursor Y pos value
    curX       db 0                             ; Cursor X pos value
    color      db 7                             ; Text color
    vramAddr   dw 0xb800                        ; Segment address of the video ram
    maxCols    db 80                            ; Max number of cols (x)
    maxRows    db 25                            ; Max number of rows (y)
    pixelWidth db 2                             ; Bytes of vram to skip to go one pixel right
    pitch      dw 2*80                          ; Bytes of vram to skip to go down one pixel
    screen times 2000 dw 0                      ; Screen buffer

;---------------------------------------------------
setupVideo:
;
; Get the cursor pos from the bios, then save position. 
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
    push ds
    push es
    
    mov ax, cs
    mov ds, ax
    
    mov bh, 0x00                                ; Page zero
    mov ah, 0x0e                                ; Teltype input
    mov al, 0x20                                ; Blank space char
    int 0x10                                    ; Video interupt
    
    mov al, 0x08                                ; Backspace char
    int 0x10                                    ; Video interupt
    
    mov ah, 0x03                                ; Get cursor pos and shape
    int 0x10                                    ; Video interupt

    mov byte [curX], dl                         ; Current cursor x pos
    mov byte [curY], dh                         ; Current cursor y pos

    mov bx, 0x40                                ; BIOS memory segment
    mov es, bx
    
    mov ax, word [es:0x10]                      ; Equipment check in the bios data area
    and ax, 0x30

    cmp ax, 0x30                                ; Monocrome video card
    je .mono
    cmp ax, 0x20                                ; Color video card
    je .color
     
  .color:
    mov word [vramAddr], 0xb800                 ; Color vram address
    jmp .done
    
  .mono:
    mov word [vramAddr], 0xb000                 ; Adjust vram address for monocrome

  .done:
    pop es                                      ; Restore registers
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax

    clc                                         ; Clear carry flag on no error
    ret

;---------------------------------------------------
videoWriteChar:
;
; Write a character into video memory.
;    
; Expects: AL    = Acsii char to print
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push di
    push es
    push ds

    mov dl, al                                  ; Save the char 

    mov ax, cs                                  ; Set the data segment to the code segment
    mov ds, ax                                  ; This is to ensure data refrences 

    mov ax, word [vramAddr]                     ; Video memory segment
    mov es, ax 

    mov ax, 2
    mov bl, byte [maxCols]
    mul bl                                      ; Take the size of a char by the screen width
    mov bl, byte [curY]                         ; Get the cursor Y pos
    mul bl                                      ; Multiply ax * bl = ax
    mov cx, ax                                  ; Store output in cx

    mov ax, 2                                   ; The size of a char
    mov bl, byte [curX]                         ; Get the cursor X pos
    mul bl                                      ; Multiply ax * bl = ax

    xor di, di                                  ; Video memory offset
    add di, ax                                  ; Add the cursor X pos offset
    add di, cx                                  ; Add the cursor Y pos offset

    mov bh, byte [color]                        ; Attribute (Background | Foreground)
    xchg bl, dl                                 ; Get the char back from dl

    cmp bl, 0x0a                                ; Is the character a line feed?
    je .lf                                      ; If so, make a new line

    cmp bl, 0x0d                                ; Is the character a new line?
    je .nl                                      ; If so, make a new line

    cmp bl, 0x08
    je .bs
    
    cmp bl, 0x09                                ; Is the character a horizontal tab?
    je .tab                                     ; If so, make a tab

    mov cl, byte [maxCols]
    cmp byte [curX], cl                         ; Is the cursor X pos is greater than 80?
    jge .row2                                   ; If so then make a new line

    mov word [es:di], bx                        ; Write into the video memory address
 
    inc byte [curX]                             ; Increase the cursor X pos
    jmp .done

  .tab:
    xor dx, dx                                  ; Clear remander

    xor ah, ah
    mov al, byte [curX]                         ; Get the cursor x pos
    or ax, ax                                   ; If the curosor is zero then just continue
    jz .tabPadLen
    
    mov bx, 4                                   ; Divide by the tab size (4)
    div bx
    
  .tabPadLen:
    mov cx, 4                                   ; Base of tab size (4)
    sub cx, dx                                  ; Subtract by remander 

    mov al, ' '                                 ; Fill with a white space character

  .tabLoop:
    call videoWriteChar                         ; Write it to the screen cx times
    loop .tabLoop
    
    jmp .done

  .row2:
    mov byte [curX], 0                          ; Reset the cursor X pos to zero
    inc byte [curY]                             ; Increase the cursor Y pos
    
    call videoScroll                            ; See if the the screen must scroll up a line

    mov ax, bx
    call videoWriteChar                         ; Write it to the screen cx times

    jmp .done

  .bs:
    cmp byte [curX], 0                          ; Ensure cursor not zero
    je .done
    dec byte [curX]                             ; Decrese cursor
    jmp .done
    
  .lf:
    mov byte [curX], 0                          ; Reset the cursor X pos to zero
    jmp .done

  .nl:
    inc byte [curY]                             ; Increase the cursor Y pos
    
    call videoScroll                            ; See if the the screen must scroll up a line

  .done:
    call videoUpdateCur                         ; Update the cursor
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;---------------------------------------------------
videoWriteStr:
;
; Print out a string to the screen.
;    
; Expects: DS:SI = String to print
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push si

  .loop:
    lodsb                                       ; Load byte from si to al
    or al, al                                   ; If al is empty stop looping
    jz .done                                    ; Done looping and return
    call videoWriteChar                         ; Write the char into video memory
    jmp .loop                                   ; Loop untill string is null

  .done:
    pop si                                      ; Restore registers
    pop ax
    
    ret
        
;---------------------------------------------------
videoWriteNumPadding32:
;
; Write a 32-bit number into video memory with padding.
;
; Expects: AX:DX = Number to display
;          BX    = Base of number
;          CL    = Char to pad with
;          CH    = Padding length
;
; Return: Nothing
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

    push dx
    mov dx, cs                                  ; Set the data segment to the code segment
    mov ds, dx                                  ; This is to ensure correct data refrences 
    mov es, dx
    pop dx
    
    mov si, .buffer1
    call itoa                                   ; Convert the 32 bit number in dx:ax

    mov al, cl                                  ; Move the char to pad with into al
    mov cl, ch                                  ; Move the padding length into cl
    xor ch, ch                                  ; Clear the higher half of cx
    mov di, .buffer2                            ; Padd out the string 
    call padStr

    mov si, .buffer2                            ; Print the buffer that is now padded
    call videoWriteStr

    mov si, .buffer1                            ; Finally, take both buffers
    mov di, .buffer2                            ; and clear them for further usage
    mov cx, 32
    
  .zeroLoop:
    mov byte [ds:si], 0
    mov byte [es:di], 0
    inc si
    inc di
    loop .zeroLoop
    
    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
        
    ret

  .buffer1 times 32 db 0
  .buffer2 times 32 db 0

;---------------------------------------------------
videoWriteNumPadding:
;
; Write a 16-bit number into video memory with padding.
;
; Expects: AX    = Number to display
;          BX    = Base of number
;          CL    = Char to pad with
;          CH    = Padding length
;
; Returns: Nothing
;
;---------------------------------------------------
    push dx                                     ; Save register

    xor dx, dx                                  ; We only want a 16-bit number
    call videoWriteNumPadding32                 ; Now print out the number

    pop dx                                      ; Restore register
    ret

;---------------------------------------------------
videoWriteNum32:
;
; Write a 32-bit number into video memory.
;
; Expects: AX:DX = Number to display
;          BX    = Base of number
;
; Returns: Nothing
;
;---------------------------------------------------
    push cx                                     ; Save register

    xor cx, cx                                  ; We dont want padding
    call videoWriteNumPadding32                 ; Now print out the 32-bit number

    pop cx                                      ; Restore register
    ret
    
;---------------------------------------------------
videoWriteNum:
;
; Write a 16-bit number into video memory.
;
; Expects: AX    = Number to display
;          BX    = Base of number
;
; Returns: Nothing
;
;---------------------------------------------------
    push cx                                     ; Save registers
    push dx

    xor dx, dx                                  ; We only want a 16-bit number
    xor cx, cx                                  ; We dont want padding
    call videoWriteNumPadding32                 ; Now print out the number

    pop dx                                      ; Restore registers
    pop cx
    ret
       
;---------------------------------------------------
videoScroll:
;
; Scroll the screen up one line, if cursor is on the
; last available line.
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
    push di
    push si
    push es
    push ds

    mov ax, cs                                  ; Set the data segment to the code segment
    mov ds, ax                                  ; This is to ensure correct data refrences 

    cmp byte [curY], 25                         ; See if scrolling the screen is even needed
    jnge .done
    
    mov ax, word [vramAddr]                    ; Video memory segment
    mov es, ax

    xor ah, ah
    mov al, byte [maxRows]
    mov bx, word [pitch]                        ; Get the offset of the bottom line
    mul bx
    mov si, ax
    mov di, 0                                   ; Take the top line 
    
  .moveLine:
    mov ax, word [es:di+bx]                     ; Grab a byte from es:di + size of char * screen width
    mov word [es:di], ax                        ; Shove the byte to the top line into es:di

    add di, 2                                   ; Increase the video memory offset

    cmp di, si                                  ; Continue untill everything has been moved up by one
    jl .moveLine

    xor ah, ah
    mov al, byte [maxRows]                      ; Get the offset of the bottom -1 line
    dec ax
    mul bx
    mov di, ax                                  ; This is the bottom line
    
  .clearLine:
    mov ah, byte [color]                        ; Grab the current text color
    mov al, ' '                                 ; Fill with a whitespace
    
    mov word [es:di], ax                        ; Shove the byte to the bottom line in es:di

    add di, 2                                   ; Increase the video memory offset
    
    cmp di, si                                  ; Continue untill the bottom line is cleared
    jle .clearLine
    
    mov byte [curY], 24                         ; Decrease the cursor ypos

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

    ret 

;---------------------------------------------------
videoClearScreen:
;
; Clear the screen.
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

    mov ax, cs                                  ; Set the data segment to the code segment
    mov ds, ax                                  ; This is to ensure correct data refrences     

    mov di, word [vramAddr]                     ; Video memory segment
    mov es, di 
    mov di, 0                                   ; Video memory offset

    cld                                         ; Clear direction flag
    mov cx, 2000                                ; Times to reapeat
    mov ah, byte [color]                        ; Set current color
    mov al, ' '                                 ; Fill with blank spaces
    rep stosw

    mov byte [curX], 0                          ; Reset the cursor X pos to zero
    mov byte [curY], 0                          ; Reset the cursor Y pos to zero

    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
;---------------------------------------------------
videoSaveScreen:
;
; Save the contents of the screen into memory.
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

    cld                                         ; Clear direction flag
    mov di, cs                                  ; Current segment
    mov es, di
    lea di, [screen]                            ; Current offset

    mov si, word [es:vramAddr]                  ; Video memory segment
    mov ds, si 
    mov si, 0                                   ; Video memory offset

    xor ah, ah
    xor bh, bh
    mov al, byte [maxCols]
    mov bl, byte [maxRows]
    mul bx
    mov cx, ax                                  ; Video resolution
    rep movsw                                   ; Copy from ds:si to es:di 

    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
;---------------------------------------------------
videoRestoreScreen:
;
; Restore the contents of the screen from memory.
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

    cld                                         ; Clear direction flag
    mov si, cs                                  ; Current segment
    mov ds, si
    lea si, [screen]                            ; Current offset
    
    mov di, word [vramAddr]                  ; Video memory segment
    mov es, di 
    mov di, 0                                   ; Video memory offset

    xor ah, ah
    xor bh, bh
    mov al, byte [maxCols]
    mov bl, byte [maxRows]
    mul bx
    mov cx, ax                                  ; Video resolution
    rep movsw                                   ; Copy bytes from ds:si to es:di 

    pop es                                      ; Restore registers
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
;---------------------------------------------------
videoUpdateBiosCur:
;
; Update the BIOS cursor.
;    
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push es
    push ds
    
    mov bx, cs                                  ; Set the data segment to the code segment
    mov ds, bx                                  ; This is to ensure correct data refrences     
    
    mov bx, 0x40                                ; BIOS memory segment
    mov es, bx

    mov bh, byte [curY]
    mov bl, byte [curX]
    mov word [es:0x50], bx

    pop ds                                      ; Restore registers
    pop es
    pop bx

    ret

;---------------------------------------------------
videoUpdateCur:
;
; Update the hardware cursor.
; https://wiki.osdev.org/VGA_Hardware#The_CRT_Controller
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
    push ds
    
    mov ax, cs                                  ; Set the data segment to the code segment
    mov ds, ax                                  ; This is to ensure correct data refrences     

    xor ah, ah
    mov al, byte [curY]                         ; Get the Y pos
    mov cl, byte [maxCols]                      ; Take the cursor Y pos * 80 (cols)
    mul cl                                      ; Multiply ax * cl = ax
    xor ch, ch
    mov cl, byte [curX]                         ; Get the X pos
    add ax, cx                                  ; Add the cursor X pos
    mov cx, ax                                  ; Move output to the cx register

    mov al, 0x0f                                ; Cursor location low
    mov dx, 0x03d4                              ; Write into CRT index register
    out dx, al

    mov al, cl                                  ; Current cursor position low
    mov dx, 0x03d5                             	; Write into the data register
    out dx, al                                  ; Low byte
 
    mov al, 0x0e                                ; Cursor location high
    mov dx, 0x03d4                              ; Write into CRT index register
    out dx, al

    mov al, ch                                  ; Current cursor position high
    mov dx, 0x03d5                              ; Write into the data register
    out dx, al                                  ; High byte

    pop ds                                      ; Restore registers
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret

