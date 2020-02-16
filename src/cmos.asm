;  cmos.asm
;
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

%define CMOS_ADDRESS_PORT        0x70           ; CMOS ports
%define CMOS_DATA_PORT           0x71

%define CMOS_RTC_SECONDS         0x00           ; Time and date CMOS registers
%define CMOS_RTC_SECONDS_ALARM   0x01
%define CMOS_RTC_MINUTES         0x02
%define CMOS_RTC_MINUTES_ALARM   0x03
%define CMOS_RTC_HOURS           0x04
%define CMOS_RTC_HOURS_ALARM     0x05
%define CMOS_RTC_DAY_OF_WEEK     0x06
%define CMOS_RTC_DAY_OF_MONTH    0x07
%define CMOS_RTC_MONTH           0x08
%define CMOS_RTC_YEAR            0x09
%define CMOS_STATUS_REG_A        0x0a    
%define CMOS_STATUS_REG_B        0x0b   
%define CMOS_STATUS_REG_C        0x0c   
%define CMOS_STATUS_REG_D        0x0d
    
%define CMOS_DIAGNOSTIC_STATUS   0x0e           ; CMOS memory registers
%define CMOS_SHUTDOWN_STATUS     0x0f
%define CMOS_FLOPPY_DRIVE_TYPES  0x10
%define CMOS_SYS_CONFIG_SETTINGS 0x11
%define CMOS_HARD_DRIVE_TYPES    0x12
%define CMOS_TYPEMATIC_PARAMS    0x13
%define CMOS_INSTALLED_EQUPMENT  0x14
%define CMOS_LOW_MEMORY_LSB      0x15
%define CMOS_LOW_MEMORY_MSB      0x16
%define CMOS_HIGH_MEMORY_LSB     0x17
%define CMOS_HIGH_MEMORY_MSB     0x18
    
%define NMI_DISABLE_BIT          0x01 
%define NMI_ENABLE_BIT           0x00
    
;---------------------------------------------------
cmosRead:
;
; Read the contents from a chosen CMOS register.
;
; Expects: AL    = CMOS address to read
;
; Returns: AH    = Contents from read
;
;---------------------------------------------------
    push bx
    mov al, bl
    
    cli                                         ; Disable interrupts
    or al, NMI_DISABLE_BIT << 7                 ; Disable NMI
    out CMOS_ADDRESS_PORT, al                   ; Access the CMOS memory address
    jmp short $+2
    in al, CMOS_DATA_PORT                       ; Read from the CMOS
    mov ah, al                                  ; Move the result into ah
    jmp short $+2
    mov al, NMI_ENABLE_BIT << 7                 ; Enable NMI
    out CMOS_ADDRESS_PORT, al                   ; Access the CMOS memory address
    sti                                         ; Enable interrupts

    mov al, bl
    pop bx
    ret

;---------------------------------------------------
cmosWrite:
;
; Write to the chosen CMOS register.
;
; Expects: AL    = CMOS address to write
;          AH    = Value to write
;
; Returns: Nothing
;
;---------------------------------------------------
    push ax
    
    cli                                         ; Disable interrupts
    or al, NMI_DISABLE_BIT << 7                 ; Disable NMI
    out CMOS_ADDRESS_PORT, al                   ; Access the CMOS memory address
    jmp short $+2
    mov al, ah                                  ; Get the CMOS register to write
    out CMOS_DATA_PORT, al                      ; Access the CMOS memory data
    jmp short $+2
    mov al, NMI_ENABLE_BIT << 7                 ; Enable NMI
    out CMOS_ADDRESS_PORT, al                   ; Access the CMOS memory address
    sti                                         ; Enable interrupts

    pop ax
    ret

;---------------------------------------------------  
cmosReadDate:
;
; Get the system date from the CMOS.
;
; Expects: Nothing
;
; Returns: AL    = Day of the week
;          CX    = Year
;          DH    = Month
;          DL    = Day
; 
;---------------------------------------------------  
    mov al, CMOS_RTC_YEAR                       ; Read the year from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    xor ch, ch
    mov cl, al
    cmp cl, 80                                  ; Check for either 1980s or 2000s
    jl .2000s

  .1900s:
    add cx, 1900                                ; Correct centruy for the 1900s
    jmp .centuryFound
    
  .2000s:
    add cx, 2000                                ; Correct centruy for the 2000s
    
  .centuryFound:
    mov al, CMOS_RTC_MONTH                      ; Read the month from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    mov dh, al
    mov al, CMOS_RTC_DAY_OF_MONTH               ; Read the current day of the month from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    mov dl, al
    mov al, CMOS_RTC_DAY_OF_WEEK                ; Read the current day of the week from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd

    xor ah, ah
    
    ret
    
;---------------------------------------------------
cmosReadTime:
;
; Get the system time from the CMOS.
;
; Expects: Nothing
;
; Returns: CH    = Hour
;          CL    = Minute
;          DH    = Second
; 
;---------------------------------------------------
    push ax                                     ; Save register

    mov al, CMOS_RTC_HOURS                      ; Read the hours from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    mov ch, al
    mov al, CMOS_RTC_MINUTES                    ; Read the minutes from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    mov cl, al
    mov al, CMOS_RTC_SECONDS                    ; Read the seconds from the RTC
    call cmosRead
    mov al, ah                                  ; Convert into a binary coded decimal
    call bcd
    
    mov dh, al
    xor dl, dl
    
    pop ax                                      ; Restore register
    
    ret
        
;---------------------------------------------------
cmosDelay:
;
; Wait for a period of time in seconds.
;
; Expects: CX    = Seconds to wait for
;
; Returns: Nothing
; 
;---------------------------------------------------
    push ax                                     ; Save registers
    push bx
    
  .while:
    mov al, CMOS_RTC_SECONDS                    ; Read the seconds from the RTC
    call cmosRead

    mov bh, ah                                  ; Current seconds
    
  .wait:
    mov al, CMOS_RTC_SECONDS                    ; Read the seconds from the RTC
    call cmosRead
    
    cmp ah, bh                                  ; Wait for the next value from the RTC
    je .wait
    
    loop .while

    pop bx                                      ; Restore registers
    pop ax
    ret

;---------------------------------------------------
bcd:
;
; Convert to a BCD value.
;
; Expects: AL    = Number to convert
;
; Returns: AL    = Converted number
; 
;---------------------------------------------------
    push bx                                     ; Save registers
    push dx

    xor dx, dx
    xor bx, bx
    xor ah, ah
    
    push ax                                     ; Calculate ((x & 0xf0) >> 4) * 10
    and ax, 0xf0                                ; Get higher BCD bits
    shr al, 1                                   ; Rotate right by 4 bits
    shr al, 1
    shr al, 1
    shr al, 1
    mov bl, 10                                  ; Multiply output by 10
    mul bl

    pop bx                                      ; Calculate (x & 0x0f)
    and bx, 0x0f                                ; Get lower BCD bits

    add ax, bx                                  ; Add the two together

    pop dx                                      ; Restore registers
    pop bx
    ret

