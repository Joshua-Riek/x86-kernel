
;---------------------------------------------------
;u32x16mul:
;
; Preform unsigned 32-bit by 16-bit multiplication
; using only 16-bit registers.
;
; Expects: DX:AX = Multiplicand 
;          BX    = Multiplier
;
; Returns: DX:AX = Product
;
    ;---------------------------------------------------

assert:
    nop
    jmp assert

%macro SET32 3
    mov %1, %3 & 0xffff                         ; Low word
    mov %2, %3 >> 16                            ; High word
%endmacro

%macro SET16 2
    mov %1, %2
%endmacro

%macro EQ32 3
    cmp %1, %3 & 0xffff                         ; Check for low word
    jne assert
    cmp %2, %3 >> 16                            ; Check for high word
    jne assert
%endmacro
    
foos:

    SET32 ax, dx, 0x12345                                            
    SET16 bx, 0x1337
    call u32x16mul
    EQ32 ax, dx, 0x15dcb2d3

    SET32 ax, dx, 0x12345                                             
    SET16 bx, -0x1337
    call u32x16mul
    EQ32 ax, dx, 0x0d684d2d

    ;SET32 ax, dx, -0xdead                                             
    SET16 bx, 0x1337
    call u32x16mul
    EQ32 ax, dx, 0xef4951d5

    SET32 ax, dx, 0xdead                                             
    SET16 bx, -0x1337
    call u32x16mul
    EQ32 ax, dx, 0xcdf651d5



    nop
    nop
    ret

%macro writeFileMacro 2+
    jmp %%endstr2
  %%str1:
    db %1, 0
  %%endstr1:
  %%str2: 
    db %2
  %%endstr2:
    mov si, %%str1
    mov di, %%str2
    mov dx, %%endstr2-%%str2
    xor ax, ax
    call writeFile
%endmacro
    
%macro createDirMacro 1
    jmp %%endstr1
  %%str1:
    db %1, 0
  %%endstr1:
    mov si, %%str1
    call createDir
%endmacro
    
%macro changeDirMacro 1
    jmp %%endstr1
  %%str1:
    db %1, 0
  %%endstr1:
    mov si, %%str1
    call changeDir
%endmacro
%ifdef penis
fileTesting:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es


    writeFileMacro "1.TXT", "\1.TXT!", 13, 10
    writeFileMacro "2.TXT", "\2.TXT!", 13, 10
    writeFileMacro "3.TXT", "\3.TXT!", 13, 10
    writeFileMacro "4.TXT", "\4.TXT!", 13, 10
    writeFileMacro "5.TXT", "\5.TXT!", 13, 10
    writeFileMacro "6.TXT", "\6.TXT!", 13, 10
    writeFileMacro "7.TXT", "\7.TXT!", 13, 10
    writeFileMacro "8.TXT", "\8.TXT!", 13, 10
    writeFileMacro "9.TXT", "\9.TXT!", 13, 10
    
    createDirMacro "FOO"
    changeDirMacro "FOO"
    createDirMacro "BAR"

    writeFileMacro "1.TXT", "\FOO\1.TXT", 13, 10
    writeFileMacro "2.TXT", "\FOO\2.TXT", 13, 10
    writeFileMacro "3.TXT", "\FOO\3.TXT", 13, 10
    writeFileMacro "4.TXT", "\FOO\4.TXT", 13, 10
    writeFileMacro "5.TXT", "\FOO\5.TXT", 13, 10
    writeFileMacro "6.TXT", "\FOO\6.TXT", 13, 10
    writeFileMacro "7.TXT", "\FOO\7.TXT", 13, 10
    writeFileMacro "8.TXT", "\FOO\8.TXT", 13, 10
    writeFileMacro "9.TXT", "\FOO\9.TXT", 13, 10

    changeDirMacro "BAR"
    writeFileMacro "1.TXT", "\FOO\BAR\1.TXT", 13, 10
    writeFileMacro "2.TXT", "\FOO\BAR\2.TXT", 13, 10
    writeFileMacro "3.TXT", "\FOO\BAR\3.TXT", 13, 10
    writeFileMacro "4.TXT", "\FOO\BAR\4.TXT", 13, 10
    writeFileMacro "5.TXT", "\FOO\BAR\5.TXT", 13, 10
    writeFileMacro "6.TXT", "\FOO\BAR\6.TXT", 13, 10
    writeFileMacro "7.TXT", "\FOO\BAR\7.TXT", 13, 10
    writeFileMacro "8.TXT", "\FOO\BAR\8.TXT", 13, 10
    writeFileMacro "9.TXT", "\FOO\BAR\9.TXT", 13, 10
    writeFileMacro "10.TXT", "\FOO\BAR\10.TXT", 13, 10
    writeFileMacro "11.TXT", "\FOO\BAR\11.TXT", 13, 10
    writeFileMacro "12.TXT", "\FOO\BAR\12.TXT", 13, 10
    writeFileMacro "13.TXT", "\FOO\BAR\13.TXT", 13, 10
    writeFileMacro "14.TXT", "\FOO\BAR\14.TXT", 13, 10
    writeFileMacro "15.TXT", "\FOO\BAR\15.TXT", 13, 10
    writeFileMacro "16.TXT", "\FOO\BAR\16.TXT", 13, 10

    changeDirMacro ".."
    writeFileMacro "10.TXT", "\FOO\10.TXT", 13, 10
    writeFileMacro "11.TXT", "\FOO\11.TXT", 13, 10
    writeFileMacro "12.TXT", "\FOO\12.TXT", 13, 10
    writeFileMacro "13.TXT", "\FOO\13.TXT", 13, 10
    writeFileMacro "14.TXT", "\FOO\14.TXT", 13, 10
    writeFileMacro "15.TXT", "\FOO\15.TXT", 13, 10
    writeFileMacro "16.TXT", "\FOO\16.TXT", 13, 10

    changeDirMacro ".."
    writeFileMacro "10.TXT", "\10.TXT", 13, 10
    writeFileMacro "11.TXT", "\11.TXT", 13, 10
    writeFileMacro "12.TXT", "\12.TXT", 13, 10
    writeFileMacro "13.TXT", "\13.TXT", 13, 10
    writeFileMacro "14.TXT", "\14.TXT", 13, 10
    writeFileMacro "15.TXT", "\15.TXT", 13, 10
    writeFileMacro "16.TXT", "\16.TXT", 13, 10
    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
%endif
    ; system detection
    ; System type:
    ; System BIOS:
    ; Diskette a:
    ; Diskette b:
    ; Hard Drive 0:
    ; Hard Drive 0:
    ; Video Adapter:
    ; Video Vendor:
     setupDebug:
     cli
    mov word [es:0x01*4], int1Service        ; Offset of interupt handler
    mov word [es:0x01*4+2], cs                  ; Segment of interupt handler

    ;    mov word [es:0x03*1], int3Service        ; Offset of interupt handler
    ;mov word [es:0x03*1+2], cs                  ; Segment of interupt handler

    mov word [es:0x03*4], int3Service           ; Offset of interupt handler
    mov word [es:0x03*4+2], cs                  ; Segment of interupt handler

    sti
ret

int1Service:
    ; stack: ret, flags, ax, cx, dx, bx, sp, bp, si, di, ds, es
    push ax ;18
    push cx ;16
    push dx ;14
    push bx ;12
    push sp ; 10
    push bp ;8
    push si ; 6
    push di ;4
    push ds ;2
    push es ;0

    mov bp, sp

    xor dx, dx
    mov bx, 16
    mov ch, 4
    mov cl, '0'
    
    mov al, 'A'
    call videoWriteChar
    mov al, 'X'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+18]
    call videoWriteNumPadding32

    mov al, ' '
    call videoWriteChar
    mov al, 'B'
    call videoWriteChar
    mov al, 'X'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+12]
    call videoWriteNumPadding32
        mov al, ' '
    call videoWriteChar
    mov al, 'C'
    call videoWriteChar
    mov al, 'X'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+16]
    call videoWriteNumPadding32

    mov al, ' '
    call videoWriteChar
    mov al, 'D'
    call videoWriteChar
    mov al, 'I'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+14]
    call videoWriteNumPadding32

    mov al, ' '
    call videoWriteChar
    mov al, 'S'
    call videoWriteChar
    mov al, 'I'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+6]
    call videoWriteNumPadding32

    mov al, ' '
    call videoWriteChar
    mov al, 'D'
    call videoWriteChar
    mov al, 'I'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+4]
    call videoWriteNumPadding32


    mov al, ' '
    call videoWriteChar
    mov al, 'D'
    call videoWriteChar
    mov al, 'S'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+2]
    call videoWriteNumPadding32
    
    mov al, ' '
    call videoWriteChar
    mov al, 'E'
    call videoWriteChar
    mov al, 'S'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+0]
    call videoWriteNumPadding32


    

    mov al, ' '
    call videoWriteChar
    mov al, 'C'
    call videoWriteChar
    mov al, 'S'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+22]
    call videoWriteNumPadding32

    mov al, ' '
    call videoWriteChar
    mov al, 'I'
    call videoWriteChar
    mov al, 'P'
    call videoWriteChar
    mov al, '='
    call videoWriteChar

    mov ax, word [ss:bp+20]
    sub ax, 0x1000
    call videoWriteNumPadding32
    
    mov al, 10
    call videoWriteChar
    mov al, 13
    call videoWriteChar


    ;push ax                                     ; Save registers
    ;push bx
    ;push cx
    ;push dx
    ;push sp
    ;push bp
    ;push si
    ;push di
    ;push ds
    ;push es

    ;sti
    ;call kbdCtrlWaitUntillKey
    mov ax, 0
    int 0x16
    
    cmp al, 0x1b
    je .esc
    jmp .cont
    
  .esc:
    and word [ss:bp+24], 0xfeff

  .cont:
     ;   call videoWriteChar


    
    ;pop es
   ; pop ds
    ;pop di
    ; pop si

    pop es
    pop ds
    pop di
    pop si
    pop bp
    pop sp
    pop bx
    pop dx
    pop cx
    pop ax
    iret


int3Service:
    ; stack: ret, flags, ax, cx, dx, bx, sp, bp, si, di, ds, es

    push ax
    push cx
    push dx
    push bx
    push sp
    push bp
    push si
    push di
    push ds
    push es

    mov bp, sp
    or word [ss:bp+24], 0x100
    ;mov ax, 0
   ; int 0x16
    ;call videoWriteChar

    pop es
    pop ds
    pop di
    pop si
    pop bp
    pop sp
    pop bx
    pop dx
    pop cx
    pop ax
    nop
    nop
    nop
    
    iret
    
