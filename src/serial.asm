;  serial.asm
;
;  Handle serial port stuff
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

%define COM1 0x3f8 
%define COM2 0x2f8 
%define COM3 0x3e8 
%define COM4 0x2e8
    
;---------------------------------------------------
initSerial:
;
; Setup the serial com ports
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
    
    mov dx, COM1 + 1
    mov al, 0x00                                ; Disable all interrupts
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, COM1 + 3
    mov al, 0x80                                ; Enable DLAB (set baud rate divisor)
    out dx, al                                  ; Send the data to the port passed

    mov dx, COM1 + 0
    mov al, 0x0c                                ; Set divisor to 12 (lo byte) 
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, COM1 + 1
    mov al, 0x00                                ; 9600 baud (hi byte)
    out dx, al                                  ; Send the data to the port passed

    mov dx, COM1 + 3
    mov al, 0x03                                ; 8 bits, no parity, one stop bit
    out dx, al                                  ; Send the data to the port passed

    mov dx, COM1 + 2
    mov al, 0xc7                                ; Enable FIFO, clear them, with 14-byte threshold
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, COM1 + 4
    mov al, 0x0b                                ; IRQs enabled, RTS/DSR set
    out dx, al                                  ; Send the data to the port passed
    
    pop dx                                      ; Restore registers
    pop cx
    pop bx
    pop ax
    ret

;---------------------------------------------------
writeSerial:
;
; Write some data to the serial port
;
; Expects: AL    = Data
;
; Returns: None
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx

    mov cl, al

  .while:
    mov dx, COM1 + 5
    in al, dx                                   ; Check the Line Status Register
                                                ; Read the data from the port passed
    and al, 0x20
    or al, al                                   ; Continue looping untill the transmitter
    jz .while                                   ; is not doing anything 

    mov dx, COM1
    mov al, cl                                  ; Grab the param
    out dx, al                                  ; Send the data to the port passed

    pop dx                                      ; Restore registers
    pop cx
    pop bx
    pop ax
    
    ret
    
;---------------------------------------------------
readSerial:
;
; Read some data from the serial port
;
; Expects: Nothing
;
; Returns: AL    = Data
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push dx

    mov ch, ah

  .while:
    mov dx, COM1 + 5
    in al, dx                                   ; Check the Line Status Register
                                                ; Read the data from the port passed
    and al, 0x01
    or al, al                                   ; Continue looping untill their is data
    jz .while                                   ; that can be read

    mov dx, COM1
    in al, dx                                   ; Read the data from the port passed

    mov ah, ch
    
    pop dx                                      ; Restore registers
    pop cx
    pop ax
    
    ret
    
;---------------------------------------------------
writeSerialStr:
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
    call writeSerial                            ; Write the char into the serial port
    jmp .loop                                   ; Loop untill string is null

  .done:
    pop si                                      ; Restore registers
    pop ax
    
    ret
    
;---------------------------------------------------
writeSerialNumPadding32:
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
    call writeSerialStr

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
logAllocMem:
;
; Log memory allocation to the serial port.
;
; Params:  None
;
; Returns: None
;
;---------------------------------------------------
    push si

    mov si, addrStr
    call writeSerialStr
    
    call debugPrint
    
    pop si
    ret
    
;---------------------------------------------------
logFreeMem:
;
; Log memory unallocation to the serial port.
;
; Params:  None
;
; Returns: None
;
;---------------------------------------------------
    push si
    
    mov si, remStr
    call writeSerialStr

    call debugPrint
    pop si
    ret
 
debugPrint: 
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov cx, cs
    mov ds, cx

    push ax
    push dx

    mov ax, es                                  ; Handle the segment addr first
    xor dx, dx                                  ; Clear remander
    mov bx, 16                                  ; Shift left
    mul bx                                      ; Multiplying by 16 is the same as (x << 4)

    clc
    add ax, di                                  ; Add the offset addr to the segment addr
    jnc .noCarry1                               ; Check for carry
    
    inc dx                                      ; Correct segment for 64k boundry

  .noCarry1:
    mov bx, 16
    mov cl, '0'
    mov ch, 5
    call writeSerialNumPadding32

    mov al, '-'
    call writeSerial
    mov al, '0'
    call writeSerial
    mov al, 'x'
    call writeSerial

    pop dx
    pop ax

    push ax
    push dx

    call memAddressToBlock                      ; Convert address to block/ index
    call memBytesToBlocks32                     ; Convert size in bytes to size in blocks
    add cx, bx
    call memBlockToAddress
    
    mov ax, es                                  ; Handle the segment addr first
    xor dx, dx                                  ; Clear remander
    mov bx, 16                                  ; Shift left
    mul bx                                      ; Multiplying by 16 is the same as (x << 4)

    clc
    add ax, di                                  ; Add the offset addr to the segment addr
    jnc .noCarry2                               ; Check for carry
    
    inc dx                                      ; Correct segment for 64k boundry

  .noCarry2:
    mov bx, 16
    mov cl, '0'
    mov ch, 5
    call writeSerialNumPadding32

    mov si, lenStr
    call writeSerialStr

    pop dx
    pop ax
    xchg ax, dx
    mov bx, 10
    mov cl, ' '
    mov ch, 0
    call writeSerialNumPadding32

    mov al, 10
    call writeSerial
    mov al, 13
    call writeSerial
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

    addrStr db "[+] Allocated address:   0x", 0
    lenStr  db " => Size in Bytes: ", 0
    remStr  db "[-] Unallocated address: 0x", 0

 
