;  string.asm
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

;---------------------------------------------------
uitoa:
;
; Converts a 32-bit number and base to a string.
;
; Expects: AX:DX = 32-bit number
;          DS:SI = String buffer to fill
;          BX    = Base of number
;
; Returns: DS:SI = Number converted to string
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es

    mov cx, cs
    mov es, cx

    mov word [es:.hiWord], dx                   ; Save the high word of the number
    mov word [es:.loWord], ax                   ; Save the low word of the number

    mov di, .baseDigits                         ; Pointer to the base digits

    xor cx, cx                                  ; Clear the coutner register (how many nums to loop)

    cmp ax, 0                                   ; Number must not be zero
    je .zero 
    cmp bx, 16                                  ; Number must be greater than 16
    jg .zero

  .while:                                       ; Repeat untill number in ax is zero
    xor dx, dx                                  ; Zero out the remander
    mov ax, word [es:.hiWord]                   ; Fill ax with the high word of the 32-bit number
    div bx                                      ; Divide
    mov word [es:.hiWord], ax                   ; Store the high word 
    mov ax, word [es:.loWord]                   ; Fill ax with the low word of the 32-bit number
    div bx                                      ; Divide
    mov word [es:.loWord], ax                   ; Store the low word
    inc cx                                      ; Increase the counter register
    push dx                                     ; Push the number onto the stack
    cmp ax, 0                                   ; Is ax zero? If not jump back to .while
    jne .while

  .loop:
    pop dx                                      ; Pop numbers off stack in reverse order

    push bx
    mov bx, dx
    mov al, byte [es:di+bx]                     ; Use dx as an index into di (baseDigits)
    mov byte [ds:si], al
    inc si
    pop bx

    loop .loop                                  ; Repeat cx times

    mov bl, 0
    mov byte [ds:si], bl                        ; Zero terminate the string
    inc si

    pop es                                      ; Restore registers
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret

  .zero:
    cmp dx, 0                                   ; Number must not be zero
    jne .while
    
    mov al, '0'
    mov byte [ds:si], al
    inc si

    mov bl, 0
    mov byte [ds:si], bl
    inc si

    pop es                                      ; Restore registers
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ret
    
  .hiWord dw 0
  .loWord dw 0
  .baseDigits db '0123456789abcdef'             ; String of base digits

;---------------------------------------------------
itoa:
;
; Converts a 32-bit number and base to a string.
;
; Expects: AX:DX = 32-bit number
;          DS:SI = String buffer to fill
;          BX    = Base of number
;
; Returns: DS:SI = Number converted to string
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es

    mov cx, cs
    mov es, cx

    mov di, .baseDigits                         ; Pointer to the base digits

    xor cx, cx                                  ; Clear the coutner register (how many nums to loop)

    mov word [es:.hiWord], dx                   ; Save the high word of the number
    mov word [es:.loWord], ax                   ; Save the low word of the number

    cmp ax, 0                                   ; Number must not be zero
    je .check 
    cmp bx, 16                                  ; Number must be less than 16
    jg .zero
    cmp dx, 0                                   ; Check for a negative number
    jl .negitive

  .while:                                       ; Repeat untill number in ax is zero
    xor dx, dx                                  ; Zero out the remander
    mov ax, word [es:.hiWord]                   ; Fill ax with the high word of the 32-bit number
    div bx                                      ; Divide
    mov word [es:.hiWord], ax                   ; Store the high word 
    mov ax, word [es:.loWord]                   ; Fill ax with the low word of the 32-bit number
    div bx                                      ; Divide
    mov word [es:.loWord], ax                   ; Store the low word
    inc cx                                      ; Increase the counter register
    push dx                                     ; Push the number onto the stack
    cmp ax, 0                                   ; Is ax zero? If not jump back to .while
    jne .while
    cmp word [es:.hiWord], 0
    jne .while

  .loop:
    pop dx                                      ; Pop numbers off stack in reverse order

    push bx
    mov bx, dx
    mov al, byte [es:di+bx]                     ; Use dx as an index into di (baseDigits)
    mov byte [ds:si], al
    inc si
    pop bx

    loop .loop                                  ; Repeat cx times

    mov bl, 0
    mov byte [ds:si], bl                        ; Zero terminate the string
    inc si

    pop es                                      ; Restore registers
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ret

  .check:
    cmp dx, 0                                   ; I feel like this should not work
    je .zero

    cmp dx, 0                                   ; Check for a negative number
    jl .negitive
    jmp .while

  .negitive:
    cmp bx, 10                                  ; Ensure base 10 number
    jne .while
    
    push bx                                     ; Save the conversion base

    mov bl, '-'                                 ; Add the negative symbol to the start of the string
    mov byte [ds:si], bl                        ; Store the indexed item into the output array

    inc si                                      ; Increase the address offset
    not ax                                      ; Convert negitive by ones complement
    inc ax                                      ; Add one
    not dx

    mov word [es:.hiWord], dx                   ; Store the high word 
    mov word [es:.loWord], ax                   ; Store the low word 

    pop bx                                      ; Restore the conversion base
    jmp .while                                  ; Convert number to base
    
  .zero:    
    mov al, '0'
    mov byte [ds:si], al
    inc si

    mov bl, 0
    mov byte [ds:si], bl
    inc si

    pop es                                      ; Restore registers
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
  .baseDigits db '0123456789abcdef'             ; String of base digits
  .hiWord dw 0
  .loWord dw 0

;---------------------------------------------------
atoi:
;
; Converts a string to a base int.
;
; Expects: DS:SI = String to convert
;          BX    = Base of number
;
; Returns: DX:AX = Number converted
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push si
    push di
    
    call strLen                                 ; Get the length of the string
    mov di, si                                  ; Copy the current string address
    add di, cx                                  ; The end of the string

    cmp si, di                                  ; Length of string is zero
    je .done
    
    xor ax, ax                                  ; Result in dx:ax
    xor dx, dx

    cmp byte [ds:si], '-'
    jne .loop
    inc si
  .loop:
    xor ch, ch
    mov cl, byte [ds:si]
    
    cmp cl, '0'                                 ; Check if the input is a decimal digit (ax >= 48 && ax <= 57)
	jl .hex
    cmp cl, '9'
    jg .hex
    
  .decimal:                                     ; Handle decimal digits
    sub cl, '0'
    jmp .multiply

  .hex:                                         ; Handle hexadecimal digits
    sub cl, 'A'
    add cl, 10
    
  .multiply:
    cmp cl, 16                                  ; Ensure valid number
    jge .done

    push cx
    push bx
    push si
    push di

    mov si, ax                                  ; Low word of result
    mov di, dx                                  ; High word of result
    mov cx, bx                                  ; Base of number
    
    clc
    xor dx, dx                                  ; Multiply the base by the Low word
    mov ax, cx                                  ; Base of number
    mov bx, si                                  ; Low word
    mul bx
    push ax
    push dx
                                                ; Multiply Low word by the High word
    mov ax, cx                                  ; Base of number
    mov bx, di                                  ; High word
    mul bx
    pop bx
    add ax, bx                                  ; Add the partial product
    adc dx, 0                                   ; Adjust if carry

    pop dx
    xchg dx, ax

    pop di
    pop si
    pop bx
    pop cx
    
    add al, cl
    
    inc si
    cmp si, di
    jne .loop

  .done:
    pop di                                      ; Restore registers
    pop si

    cmp byte [ds:si], '-'
    jne .negate
    not ax                                      ; Convert negitive by ones complement
    inc ax                                      ; Add one
    not dx
    
  .negate:
    pop cx
    pop bx
    
    ret

;---------------------------------------------------
convertFilename83:
;
; Convert the filename into a fat formatted
; filename (8.3 format).
;
; Expects: DS:SI = Filename to convert
;          ES:DI = Converted filename buffer
;
; Returns: ES:DI = Updated filename
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    
    call strLen                                 ; Get the length of the string in si
    cmp cx, 0                                   ; See if the string is empty (like my soul)
    jz .error

    xor cx, cx                                  ; Handle the first 8 chars

  .dot:
    mov al, byte [ds:si]
    cmp al, '.'
    jne .copyFilename

    lodsb
    stosb                                       ; If not, store it into the tmp string
    inc cx                                      ; Increase counter

    mov ax, word [ds:si]
    cmp word [ds:si], 0x002e
    je .2dot

    cmp byte [ds:si], 0
    jne .copy

    lodsb

    jmp .padExt

  .2dot:
    lodsb
    stosb
    lodsb
    inc cx

    jmp .padExt

  .copy:
    xor cx, cx                                  ; Handle the first 8 chars

  .copyFilename:
    lodsb                                       ; Load the a byte from si
    call charToUpper
    or al, al                                   ; Pad out the rest of the string if null
    jz .padExt
    cmp al, '.'                                 ; Check for the extension
    je .extFound
    stosb                                       ; If not, store it into the tmp string
    inc cx                                      ; Increase counter
    cmp cx, 8                                   ; Ensure that their is an extension
    jg .padExt2
    jmp .copyFilename
 
  .extFound:   
    cmp cx, 8                                   ; Check to see if padding is needed
    je .copyExt

  .padFilename:
    mov al, ' '
    stosb                                       ; Store a space into the tmp string
    inc cx                                      ; Increase the counter
    cmp cx, 8                                   ; Padd filename untill length of 8
    jl .padFilename

  .copyExt:
    lodsb                                       ; Load the extension char
    call charToUpper                            ; Ensure uppercase
    or al, al                                   ; See if we need to just pad out the remander
    jz .padExt
    stosb                                       ; Store it
    inc cx                                      ; Loop untill length of 11
    cmp cx, 11
    jge .done
    jmp .copyExt

  .padExt2:
    dec si                                      ; Go back a byte 
    dec di                                      ; And check to see if we need to null terminate
    dec cx
    cmp byte [ds:si+3], ' '
    jne .copyExt

    mov byte [es:di], 0
    
  .padExt:
    mov al, ' '                                 ; Pad with a whitespace
    stosb                                       ; Store it
    inc cx                                      ; Continue untill length of 11
    cmp cx, 11
    jge .done
    jmp .padExt
    
  .done:
    mov byte [es:di], 0

    pop di                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret

  .error:
    pop di                                      ; Restore registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
;---------------------------------------------------
padStr:
;
; Pad a string with ascii chars.
;
; Expects: AL    = Char to pad with
;          CX    = Padding length
;          DS:SI = Input string
;          ES:DI = Output string
;
; Returns: DS:DI = Padded string
;
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di

    mov dl, al                                  ; Save the char to padd with for later
    
    push cx
    call strLen                                 ; Get the length of the new string
    mov ax, cx
    pop cx                                      ; Restore padding length

    sub cx, ax                                  ; Subtract the length of the string and the padding needed
    
    cmp cx, 0                                   ; Skip if less than or equal to zero
    jle .paddingDone

  .paddingLoop:
    mov byte [es:di], dl                        ; Put the padding into the output string
    inc di
    loop .paddingLoop

  .paddingDone:
    mov cx, ax                                  ; Get the length of the string back
    
  .copyLoop:
    mov dl, byte [ds:si]                        ; Grab a byte from the origonal string
    inc si
    mov byte [es:di], dl                        ; Shove it into the output string
    inc di
    loop .copyLoop

    mov byte [es:di], 0                         ; Null terminate
    inc di
    
    pop di                                      ; Restore stack
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    
;---------------------------------------------------
parseStr:
;
; Split the string into tokens.
;
; Expects: DS:SI = String to parse
;
; Returns: AX    = Pointer to first string
;          BX    = Pointer to second string
;          CX    = Pointer to third string
;          DX    = Pointer to fourth string
;
;---------------------------------------------------
    push si                                     ; Pointer to string
    
    xor al, al                                  ; Clear return values
    xor bx, bx
    xor cx, cx
    xor dx, dx
    
    call findSpace                              ; See if the first string starts with a space
    jc .none

    dec si
    mov ax, si                                  ; First string token (AX = "FOO")
    inc si
    
    push ax                                     ; Save the first token on the stack

    call findStr                                ; Attempt to find text in the string
    jc .finish
    call findSpace                              ; Attempt to find another space between the text
    jc .finish
    
    dec si
    mov bx, si                                  ; Second string token (BX = "OwO")
    inc si

    call findStr                                ; Attempt to find text in the string
    jc .finish
    call findSpace                              ; Attempt to find another space between the text
    jc .finish
    
    dec si
    mov cx, si                                  ; Third string token (CX = "BAZ")
    inc si
    
    call findStr                                ; Attempt to find text in the string
    jc .finish
    call findSpace                              ; Attempt to find another space between the text
    jc .finish
    
    dec si
    mov dx, si                                  ; Fourth string token (DX = "UwU")
    inc si
    
    call findStr                                ; Attempt to find text in the string
    jc .finish
    
  .finish:
    pop ax                                      ; Restore the first token

  .done:
    pop si                                      ; Restore string pointer
    ret

  .none:
    pop si                                      ; Restore string pointer
    ret

;---------------------------------------------------
findSpace:
;
; Find a space in a string.
;
; Expects: DS:SI = String to parse
;
; Returns: DS:SI = Pointer to found space
;
;---------------------------------------------------
    mov al, byte [ds:si]                        ; Grab a char from ds:si
    call charToUpper                            ; Convert the char to uppercase
    mov byte [ds:si], al
    inc si
    cmp al, 0                                   ; Check for the end of the string
    je .end
    cmp al, ' '                                 ; Check for a space
    je findSpace

    clc
    ret
    
  .end:
    stc
    ret
    
;---------------------------------------------------
findStr:
;
; Find the next string.
;
; Expects: DS:SI = String to parse
;
; Returns: DS:SI = Pointer to found string
;
;---------------------------------------------------
    mov al, byte [ds:si]                        ; Grab a char from the string
    call charToUpper                            ; Convert the char to uppercase
    mov byte [ds:si], al
    inc si
    cmp al, 0                                   ; Check for the end of the string
    je .end
    cmp al, ' '                                 ; Check for a space
    jne findStr
    dec si
    mov byte [ds:si], 0                         ; Terminate with a zero
    inc si
    
    clc
    ret
    
  .end:
    stc
    ret
    
;---------------------------------------------------
strCmp:
;
; Checks to see if the two passed strings
; are equal to eachother or not.
;
; Expects: DS:SI = First string to compare
;          ES:DI = Second string to compare
;
; Returns: Carry flag (CF) set if equal
;
;---------------------------------------------------
    push ax
    push bx
    push si
    push di

    xor ax, ax
    xor bx, bx
    
  .cmp:
    mov al, byte [ds:si]                        ; Byte from si
    mov bl, byte [es:di]                        ; Byte from di
    cmp al, bl					                ; Are both bytes equal?
    jne .nequal					                ; If not, we are finished
    cmp al, 0	                                ; If string is empty, we are finished
    je .equal
    inc di					                    ; Point to next char
    inc si					                    ; Point to next char
    jmp .cmp

  .nequal:
    clc                                         ; Clear carry flag on not equal
    jmp .done
    
  .equal:
    stc                                         ; Set carry flag on equal

  .done:
    pop di
    pop si
    pop bx
    pop ax
    ret

;--------------------------------------------------
strLen:
;
; Get the length of a string.
;
; Expects: DS:SI = String
;
; Returns: CX    = Length of string
;
;--------------------------------------------------
    push ax
    push si
    
    xor cx, cx                                  ; Store length in cx

  .loop:
    lodsb                                       ; Load char from si to al
    or al, al                                   ; If al is empty stop looping
    jz .done
    inc cx                                      ; Increase the counter
    jmp .loop                                   ; Loop again

  .done:
    pop si
    pop ax
    ret
    
;---------------------------------------------------
charToLower:
;
; Checks to see if the passed character is an
; lowercase letter or not and converts.
;
; Expects: AL   = Character to convert
;
; Returns: AL   = Lowercase character
;
;--------------------------------------------------
    cmp al, 65
    jl .done                                    ; Check if the input is a lowercase letter (ax >= 65 && ax <= 90)
    cmp al, 90
    jng .add32

  .add32:
    add al, 32                                  ; Add the number 32 to make the char lowercase

  .done:
    ret

;---------------------------------------------------
charToUpper:
;
; Checks to see if the passed character is an
; uppercase letter or not and converts.
;
; Expects: AL   = Character to convert
;
; Returns: AL   = Uppercase character
;
;--------------------------------------------------
    cmp al, 97
    jl .done                                    ; Check if the input is a uppercase letter (ax >= 97 && ax <= 122)
    cmp al, 122
    jng .sub32

  .sub32:
    sub al, 32                                  ; Subtract the number 32 to make the char uppercase

  .done:
    ret
