;  math.asm
;
;  Math is pain.  
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

;---------------------------------------------------
u32x16mul:
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
    push bx                                     ; Save registers
    push cx
    
    mov cx, dx                                  ; Save the hi word of the multiplicand
    xor dx, dx
    mul bx                                      ; Multiply the low word of the multiplicand by the multiplier
    push dx
    xchg ax, cx                                 ; Swap partial product with the hi word of the multiplicand

    mul bx                                      ; Multiply the hi word of the multiplicand by the multiplier
    pop bx
    add ax, bx                                  ; Add the partial product 
    adc dx, 0                                   ; Adjust if carry
    
    mov dx, cx
    xchg dx, ax

    pop cx                                      ; Restore registers
    pop bx
    
    ret
    
;---------------------------------------------------
s32x16mul:
;
; Preform signed 32-bit by 16-bit multiplication
; using only 16-bit registers.
;
; Expects: DX:AX = Multiplicand 
;          BX    = Multiplier
;
; Returns: DX:AX = Product
;
;---------------------------------------------------    
    push bx                                     ; Save registers
    push cx
    push si
    push di
    
    cmp dx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb si, si                                  ; Set the sign to use (dest - (src + CF))
    xor ax, si                                  ; Negate the lo word of the multiplicand
    xor dx, si                                  ; Negate the hi word of the multiplicand
    sub ax, si                                  ; Add one to the lo word of the multiplicand
    sbb dx, si                                  ; Add carry with the hi word of the multiplicand

    cmp bx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb di, di                                  ; Set the sign to use (dest - (src + CF))
    xor bx, di                                  ; Negate the multiplier
    sub bx, di                                  ; Add carry to the multiplier
    
    xor si, di                                  ; Sign to use for the product

    call u32x16mul                              ; Preform the multiplication

    xor ax, si                                  ; Negate the lo word of the product
    xor dx, si                                  ; Negate the hi word of the product
    sub ax, si                                  ; Add one to the low word
    sbb dx, si                                  ; Adjust carry with the hi word

    pop di                                      ; Restore registers
    pop si
    pop cx
    pop bx
    
    ret

;---------------------------------------------------
u32x16div:
;
; Preform unsigned 32-bit by 16-bit divison using
; only 16-bit registers.
;
; Expects: DX:AX = Dividend 
;          BX    = Divisor
;
; Returns: DX:AX = Quotient
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    
    mov cx, ax                                  ; Save the low word of the dividend
    mov ax, dx
    xor dx, dx                                  ; Divide the hi word of the dividend by the divisor
    div bx
    xchg ax, cx                                 ; Swap partial product with the low word of the dividend

    div bx                                      ; Divide the low word of the dividend by the divisor
    mov bx, dx
    mov dx, cx

    pop cx                                      ; Restore register
    pop bx
    
    ret
    
;---------------------------------------------------
s32x16div:
;
; Preform signed 32-bit by 16-bit divison using
; only 16-bit registers.
;
; Expects: DX:AX = Dividend 
;          BX    = Divisor
;
; Returns: DX:AX = Quotient
;
;---------------------------------------------------
    push bx                                     ; Save registers
    push cx
    push si
    push di
    
    cmp dx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb si, si                                  ; Set the sign to use (dest - (src + CF))
    xor ax, si                                  ; Negate the lo word of the dividend
    xor dx, si                                  ; Negate the hi word of the dividend
    sub ax, si                                  ; Add one to the lo word of the dividend
    sbb dx, si                                  ; Add carry with the hi word of the dividend

    cmp bx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb di, di                                  ; Set the sign to use (dest - (src + CF))
    xor bx, di                                  ; Negate the divisor
    sub bx, di                                  ; Add carry to the divisor
    
    xor si, di                                  ; Sign to use for the quotient

    call u32x16div                              ; Preform the division

    xor ax, si                                  ; Negate the lo word of the quotient
    xor dx, si                                  ; Negate the hi word of the quotient
    sub ax, si                                  ; Add one to the low word
    sbb dx, si                                  ; Adjust carry with the hi word

    pop di                                      ; Restore registers
    pop si
    pop cx
    pop bx

    ret

;---------------------------------------------------
u32x32div:
;
; Preform unsigned 32-bit by 32-bit divison using
; only 16 bit registers.
;
; Expects: DX:AX = Dividend 
;          CX:BX = Divisor
;
; Returns: DX:AX = Quotient
;
;---------------------------------------------------
    test cx, cx                                 ; Check for 32-bit divisor
    jnz .binaryDiv
    test bx, bx                                 ; Check for 16-bit divisor
    jnz u32x16div
                                                ; Fall through if the divisor is zero
  .divByZero: 
    xor ax, ax
    mov bx, ax
    mov cx, ax
    mov dx, ax
    
    ret

  .binaryDiv:                                   ; Divide a 32-bit number by a 32-bit number
    push si                                     ; Save registers
    push di
    push bp
    
    mov bp, 32                                  ; Counter
    xor di, di                                  ; Remainder
    xor si, si

  .bitLoop:
    shl ax, 1                                   ; Shift the dividend
    rcl dx, 1                                   ; Shift remainder & quotient by 1 bit R = R << 1
    rcl di, 1
    rcl si, 1
    
    cmp si, cx                                  ; Is remainder greater than or equal to the divisor R >= D
    ja .goesInto                                ; Check the high words of the remainder and divisor
    jb .nextBit                                 ; Then check the low words
    cmp di, bx
    jb .nextBit
    
  .goesInto:
    sub di, bx                                  ; Subtract the remander by the divisor R = R - D
    sbb si, cx
    inc ax                                      ; Set the low bit of the quotient Q(i) = 1
    
  .nextBit:
    dec bp                                      ; Loop untill zero (dec sets zero flag)
    jnz .bitLoop

    mov cx, si                                  ; Return the remainder
    mov bx, di                                  ; Quotient allready in dx:ax
    
    pop bp                                      ; Restore registers
    pop di
    pop si
    
    ret
    
;---------------------------------------------------
s32x32div:
;
; Preform unsigned 32-bit by 32-bit divison using
; only 16 bit registers.
;
; Expects: DX:AX = Dividend 
;          CX:BX = Divisor
;
; Returns: DX:AX = Quotient
;
;---------------------------------------------------
    push si
    push di
    
    cmp dx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb si, si                                  ; Set the sign to use (dest - (src + CF))
    xor ax, si                                  ; Negate the lo word of the dividend
    xor dx, si                                  ; Negate the hi word of the dividend
    sub ax, si                                  ; Add one to the lo word of the dividend
    sbb dx, si                                  ; Add carry with the hi word of the dividend

    cmp bx, 1 << 15                             ; Check for the sign bit
    cmc                                         ; Complement carry flag for sign
    sbb di, di                                  ; Set the sign to use (dest - (src + CF))
    xor bx, di                                  ; Negate the lo word of the divisor
    xor cx, di                                  ; Negate the hi word of the divisor
    sub bx, di                                  ; Add one to the lo word of the divisor
    sbb cx, di                                  ; Add carry with the hi word of the divisor
    
    xor si, di                                  ; Sign to use for the quotient

    call u32x32div                              ; Preform the divison

    xor ax, si                                  ; Negate the lo word of the quotient
    xor dx, si                                  ; Negate the hi word of the quotient
    sub ax, si                                  ; Add one to the low word
    sbb dx, si                                  ; Adjust carry with the hi word

    pop di
    pop si
    
    ret

;---------------------------------------------------
u32x16add:
;
; Preform unsigned 32-bit addition using
; only 16 bit registers. 
;
; Expects: DX:AX = First addend
;          BX    = Second addend
;
; Returns: DX:AX = Sum
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared

    add ax, bx                                  ; Add the low word of the first addend by the second addend
    adc dx, 0                                   ; Adjust the hi word of the first addend if carry 

    ret
    
;---------------------------------------------------
s32x16add:
;
; Preform signed 32-bit by 16-bit addition using
; only 16-bit registers. 
;
; Expects: DX:AX = First addend
;          BX    = Second addend
;
; Returns: DX:AX = Sum
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared

    add ax, bx                                  ; Add the low word of the first addend by the second addend
    adc dx, 0                                   ; Adjust the hi word of the first addend if carry 
    cwd                                         ; Now extend the sign bit to get the sum
    
    ret

;---------------------------------------------------
u32x32add:
;
; Preform unsigned 32-bit by 32-bit addition using
; only 16-bit registers. 
;
; Expects: DX:AX = First addend
;          BX    = Second addend
;
; Returns: DX:AX = Sum
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared

    add ax, bx                                  ; Add the low word of the first addend by the low word of the second addend
    adc dx, cx                                  ; Add the hi word of the first addend by the hi word of the second addend and adjust carry

    ret

;---------------------------------------------------
s32x32add:
;
; Preform signed 32-bit by 32-bit addition using
; only 16-bit registers. 
;
; Expects: DX:AX = First addend
;          BX    = Second addend
;
; Returns: DX:AX = Sum
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared

    add ax, bx                                  ; Add the low word of the first addend by the low word of the second addend
    adc dx, cx                                  ; Add the hi word of the first addend by the hi word of the second addend and adjust carry
    cwd                                         ; Now extend the sign bit to get the sum (not sure if needed for 32x32)
    
    ret
    
;---------------------------------------------------
u32x16sub:
;
; Preform unsigned 32-bit by 16-bit subtraction using
; only 16-bit registers. 
;
; Expects: DX:AX = Minuend 
;          BX    = Subtrahend
;
; Returns: DX:AX = Diffrence
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared 

    sub ax, bx                                  ; Subtract the low word of the minuend by the subtrahend
    sbb dx, 0                                   ; Adjust the hi word of the minuend if carry 

    ret
    
;---------------------------------------------------
s32x16sub:
;
; Preform signed 32-bit by 16-bit subtraction using
; only 16-bit registers. 
;
; Expects: DX:AX = Minuend 
;          BX    = Subtrahend
;
; Returns: DX:AX = Diffrence
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared 

    sub ax, bx                                  ; Subtract the low word of the minuend by the subtrahend
    sbb dx, 0                                   ; Adjust the hi word of the minuend if carry 
    cwd                                         ; Now extend the sign bit to get the diffrence

    ret

;---------------------------------------------------
u32x32sub:
;
; Preform unsigned 32-bit by 32-bit subtraction using
; only 16-bit registers. 
;
; Expects: DX:AX = Minuend 
;          CX:BX = Subtrahend
;
; Returns: DX:AX = Diffrence
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared 

    sub ax, bx                                  ; Subtract the low word of the minuend by the low word of the subtrahend
    sbb dx, cx                                  ; Subtract the hi word of the minuend by the hi word of the subtrahend and adjust carry

    ret
    
;---------------------------------------------------
s32x32sub:
;
; Preform signed 32-bit by 32-bit subtraction using
; only 16-bit registers.
;
; Expects: DX:AX = Minuend 
;          CX:BX = Subtrahend
;
; Returns: DX:AX = Diffrence
;
;---------------------------------------------------
    clc                                         ; Ensure carry flag is cleared 

    sub ax, bx                                  ; Subtract the low word of the minuend by the low word of the subtrahend
    sbb dx, cx                                  ; Subtract the hi word of the minuend by the hi word of the subtrahend and adjust carry
    cwd                                         ; Now extend the sign bit to get the diffrence (not sure if needed for 32x32)

    ret
    
;---------------------------------------------------
u32x32mul:
;
; Preform 32-bit by 32-bit multiplication using
; only 16 bit registers. 
;
; Expects: DX:AX = First 32-bit number
;          CX:BX = Second 32-bit number
;
; Returns: DX:CX:BX:AX = 64-bit product
;
;---------------------------------------------------
    push ds
    push cs
    pop ds

    mov word [.num1+2], dx
    mov word [.num1], ax
    
    mov word [.num2+2], cx
    mov word [.num2], bx
    
    xor dx, dx                                  ; Multiply the Low words
    mov ax, word [.num1]                        ; Low word
    mul word [.num2]                            ; Low word
    mov bx, dx                                  ; Save remander
    
    push ax                                     ; Save Low word
                                                ; Multiply Low word by the High word
    mov ax, word [.num1]                        ; Low word
    mul word [.num2+2]                          ; High word
    add ax, bx                                  ; Add the partial product
    adc dx, 0                                   ; Adjust if carry

    mov bx, ax                                  ; Partial product
    mov cx, dx
                                                ; Multiply High word by the Low word
    mov ax, word [.num1+2]                      ; High word
    mul word [.num2]                            ; Low word
    add ax, bx                                  ; Add to the partal product
    adc cx, dx                                  ; Adjust if carry
    push ax
                                                ; Multiply High word by the High word
    mov ax, word [.num1+2]                      ; High word
    mul word [.num2+2]                          ; High word
    add ax, cx                                  ; Add the partial product
    adc dx, 0                                   ; Adjust if carry

    mov cx, ax
    pop bx
    pop ax

    pop ds
    ret

  .num1 dd 0
  .num2 dd 0
