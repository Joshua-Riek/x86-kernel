;---------------------------------------------------
;
; serial.asm
;
; Handle serial port shit.
;
; This file is part of the SuccOS C library.
;
; Copyright (c) 2017-2018 Joshua Riek. Anyone is free to
; copy, modify, publish, use, compile, or distribute
; this software, either in source code form or as a
; compiled binary and for non-commercial use only.
; No warranty is given.
;
;---------------------------------------------------

;---------------------------------------------------
; Serial contants
;---------------------------------------------------
    
%define COM1 0x3f8 
%define COM2 0x2f8 
%define COM3 0x3e8 
%define COM4 0x2e8

;---------------------------------------------------
; Serial functions
;---------------------------------------------------
;
; initSerial IN=> None; OUT=> None
; writeSerial IN=> AL=Data; OUT=> None
; writeSerialStr IN=> DS:SI=Ptr to str; OUT=> None
; writeSerialNumPadding32 IN=> AX:DX=Num, BX=Base, CH=Pad len, CL=Pad char; OUT=> None
; writeSerialNumPadding IN=> AX=Num, BX=Base, CH=Pad len, CL=Pad char; OUT=> None
; logAllocMem IN=> None; OUT=> None
; logFreeMem IN=> None; OUT=> None
    
;---------------------------------------------------
initSerial:
;
; Setup the serial com ports
;
; Params:  None
;
; Returns: None
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    
    mov dx, 0x3f8 + 1
    mov al, 0x00                                ; Disable all interrupts
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, 0x3f8 + 3
    mov al, 0x80                                ; Enable DLAB (set baud rate divisor)
    out dx, al                                  ; Send the data to the port passed

    mov dx, 0x3f8 + 0
    mov al, 0x03                                ; Set divisor to 3 (lo byte) 
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, 0x3f8 + 1
    mov al, 0x00                                ; 38400 baud (hi byte)
    out dx, al                                  ; Send the data to the port passed

    mov dx, 0x3f8 + 3
    mov al, 0x03                                ; 8 bits, no parity, one stop bit
    out dx, al                                  ; Send the data to the port passed

    mov dx, 0x3f8 + 2
    mov al, 0xC7                                ; Enable FIFO, clear them, with 14-byte threshold
    out dx, al                                  ; Send the data to the port passed
    
    mov dx, 0x3f8 + 4
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
; Write some data to the com1 serial port
;
; Params:  AL    = Data
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
    mov dx, 0x3f8 + 5
    in al, dx
    and al, 0x20

    or al, al
    jz .while

    mov dx, 0x3f8
    mov al, cl                                  ; Grab the char param
    out dx, al                                  ; Send the data to the port passed

    pop dx                                      ; Restore registers
    pop cx
    pop bx
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
writeSerialNumPadding:
;
; Write a 16-bit number into serial port with padding.
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
    call writeSerialNumPadding32                ; Now print out the number

    pop dx                                      ; Restore register
    ret
writeSerialNum32:
    push cx
    xor cx, cx
    call writeSerialNumPadding32
    pop cx
    ret
 writeSerialNum:
    push cx
    push dx
    xor cx, cx
    xor dx, dx
    call writeSerialNumPadding
    pop dx
    pop cx
    ret
    
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

 
