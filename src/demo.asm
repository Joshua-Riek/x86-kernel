


    mov ax, cs
    mov ds, ax
   
    mov si, str2
    call print

    retf
str2 db "fuwusy wuwusy haws a bwer", 10, 13, 0
;---------------------------------------------------
print:
;
; Print out a simple string.
;
; Expects: DS:SI = String to print
;
; Returns: None
;
;---------------------------------------------------
    lodsb                                       ; Load byte from ds:si to al
    or al, al                                   ; If al is empty stop looping
    jz .done                                    ; Done looping and return
    mov ah, 0x0e                                ; Teletype output
    int 0x10                                    ; Video interupt
    jmp print                                   ; Loop untill string is null
  .done:
    ret

    
