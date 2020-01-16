;  cli.asm
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
 
    requiredParamErr  db "Required parameter missing", 13, 10, 0
    fileNotFoundOrErr db "Duplicate file name or file not found", 13, 10, 0
    readSectorErr     db "Failure to read sectors on drive ", 0
    writeSectorErr    db "Failure to write sectors on drive ", 0
    badCommandErr     db "Bad command or filename", 13, 10, 0
    fileNotFoundErr   db "File not found", 13, 10, 0
    currentDate       db "Current date is ", 0
    currentTime       db "Current time is ", 0
    files             db " File(s) ", 0
    bytes             db " bytes", 0
    dayOfWeek         db "Sun ", 0, "Mon ", 0, "Tue ", 0,
                      db "Wed ", 0, "Thu ", 0, "Fri ", 0,
                      db "Sat ", 0
    
    cmdDir            db "DIR", 0
    cmdRename         db "RENAME", 0
    cmdRen            db "REN", 0
    cmdErase          db "ERASE", 0
    cmdDel            db "DEL", 0
    cmdType           db "TYPE", 0
    cmdEdit           db "EDIT", 0
    cmdDate           db "DATE", 0
    cmdTime           db "TIME", 0
    cmdHelp           db "HELP", 0
    cmdWarranty       db "WARRANTY", 0
    cmdRedistrib      db "REDISTRIB", 0
    cmdDump           db "DUMP", 0
    cliBuff times 66  db 0
cmdNew db "NEW", 0
    
    cmdDirDesc  db 9, 9, "List files and folders in a directory", 13, 10, 0
    cmdRenDesc  db 9,    "Change the name of files and directories", 13, 10, 0
    cmdDelDesc  db 9, 9, "Remove one or more files", 13, 10, 0
    cmdTypeDesc db 9,    "Display the contents of a text file", 13, 10, 0
    cmdTimeDesc db 9,    "View the computer's time", 13, 10, 0
    cmdDateDesc db 9,    "Look at the current date of the computer", 13, 10, 0
    cmdHelpDesc db 9,    "List the available commands", 13, 10, 0

    ; TODO: Time set/view
    ;       Date set/view
    
cliLoop:
    mov al, 0x0a                                ; Line feed
    call videoWriteChar
    mov al, 0x0d                                ; Newline
    call videoWriteChar
    
  .displayPrompt:
    mov al, byte [drive]
    call driveToAscii                           ; Current drive letter to ascii
    
    call videoWriteChar
    mov al, 0x3a                                ; Ascii ':'
    call videoWriteChar
    mov al, 0x5c                                ; Ascii '\'
    call videoWriteChar
    mov al, 0x3e                                ; Ascii '>'
    call videoWriteChar

  .captureInput:
    mov si, cliBuff                             ; Now capture the user input
    call kbdCaptureInput

    mov al, 0x0a                                ; Line feed
    call videoWriteChar
    mov al, 0x0d                                ; Newline
    call videoWriteChar

  .parseInput:
    mov si, cliBuff
    mov di, cliBuff
    call parseString                            ; Parse through the user input string

    mov di, si

  .parseCommands:
    cmp al, 0                                   ; Do nothing if input empty
    je .displayPrompt

    mov si, cmdDir
    call strCmp
    jc doDir

    mov si, cmdRename                            ; Rename
    call strCmp
    jc doRename
    
    mov si, cmdRen
    call strCmp
    jc doRename
    
    mov si, cmdDel                               ; Del
    call strCmp
    jc doDel

    mov si, cmdErase
    call strCmp
    jc doDel

    mov si, cmdNew
    call strCmp
    jc doNew
;    mov si, cmdType                             ; Type
;    call strCmp
;    jc doType

;    mov si, cmdTime                             ; Time
;    call strCmp
;    jc doTime

;    mov si, cmdDate                             ; Date
;    call strCmp
;    jc doDate
    
    mov si, cmdHelp                             ; Help
    call strCmp
    jc doHelp
    
    mov si, cmdWarranty                         ; Warranty
    call strCmp
    jc doWarranty
    
    mov si, cmdRedistrib                        ; Redistrib
    call strCmp
    jc doRedistrib

    mov si, cmdDump
    call strCmp
    jc doDump
    
    mov si, badCommandErr                       ; Bad command
    call videoWriteStr
    jmp cliLoop

doDir:
    push es
    push ds
    push di
    push si
    push ax
    push bx
    push cx
    push dx
    
 
    
    xor dx, dx
    mov ax, 32
    mul word [cs:rootDirEntries]                ; Calculate the size of the root dir in bytes

    xchg ax, bx
    call memBytesToBlocks                       ; Convert bytes to blocks

    mov bx, ax
    call memAllocBlocks                         ; Allocate memory
    jc .memError                                ; Out of memory
    
    mov es, ax
    mov di, dx

    mov cx, word [rootDirSize]                  ; Read the size in sectors of the directory
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call readSectors                            ; Read the sectors 
    jc .error

    push es
    push di
    push bx
    
    xor bx, bx
    xor cx, cx
    xor dx, dx
    
    push bx
    push dx
    push cx

  .searchDir:
    mov al, byte [es:di]                        ; Grab the first byte of the first file
    
    cmp al, 0x00                                ; Skip over an empty entry
    je .nextFile
    cmp al, 0xe5                                ; Skip over the free entry marker
    je .nextFile

    pop cx
    pop dx
    pop bx
    
    inc cx                                      ; Increase counter for files in directory
    
    clc
    mov ax, word [es:di+dirFat.filesize]        ; Grab the current file's filesize
    add bx, ax                                  ; Add to the total filesize of the dir
    jnc .noCarry
    inc dx                                      ; Increase higher half of the 32-bit filesize
    
  .noCarry:
    push bx
    push dx
    push cx
  
    mov cx, 8                                   ; Length of the file name  
    
  .printName:
    mov al, byte [es:di]                        ; Grab the first byte of the file name
    call videoWriteChar
    inc di                                      ; Increase the offset to the next character
    loop .printName                             ; Repeat untill name is written
    
    mov al, ' '                                 ; Write a new line onto the screen
    call videoWriteChar

    mov cx, 3                                   ; Length of the extension
    
  .printExt:
    mov al, byte [es:di]                        ; Grab the first byte of the ext
    call videoWriteChar
    inc di                                      ; Increase the offset to the next character
    loop .printExt                              ; Repeat untill ext is written

    sub di, 11                                  ; Subtract the length of the name and ext

  .printFilesize:
    mov al, 0x09                                ; Horizontal tab
    call videoWriteChar

    mov ax, word [es:di+dirFat.filesize]        ; Lower half of the filesize, max is 64kb
    mov bx, 10                                  ; Decimal int
    mov ch, 5                                   ; Pad length
    mov cl, ' '                                 ; Pad with spaces
    call videoWriteNumPadding

    mov al, 0x20                                ; White space 
    call videoWriteChar
    mov al, 0x20                                ; White space 
    call videoWriteChar
    mov al, 0x20                                ; White space 
    call videoWriteChar

  .printDate:
    mov ax, word [es:di+dirFat.modifiedDate]    ; Grab the date of the file
    push ax                                     ; Save the date

    and ax, 0000000111100000b                   ; Mask bits for the month
    shr ax, 1                                   ; Right shift 5 places
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    mov ch, 2                                   ; Pad length
    mov cl, ' '                                 ; Pad with spaces
    call videoWriteNumPadding

    mov al, '-'                                 ; Split with a slash
    call videoWriteChar

    pop ax
    push ax
    
    and ax, 0000000000011111b                   ; Mask bits for the day
    mov ch, 2                                   ; Pad length
    mov cl, '0'                                 ; Pad with spaces
    call videoWriteNumPadding
     
    mov al, '-'                                 ; Split with a slash
    call videoWriteChar
    mov al, ' '

    pop ax

    and ax, 1111111000000000b                   ; Mask bits for the year
    shr ax, 1                                   ; Right shift 9 places
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    cmp ax, 20                                  ; Check for either 1980s or 2000s
    jl .1980s

  .2000s:
    sub ax, 20                                  ; Sub 20 for the 2000s
    mov ch, 2                                   ; Pad length
    mov cl, '0'                                 ; Pad with spaces
    call videoWriteNumPadding
    jmp .printTime
    
  .1980s:
    add ax, 80                                  ; Add 80 for the 1980s
    call videoWriteNum
    
  .printTime:
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    
    mov ax, word [es:di+dirFat.modifiedTime]    ; Grab the time of the file
    push ax      

    and ax, 1111100000000000b                   ; Mask bits for the month
    shr ax, 1                                   ; Right shift 11 places
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1
    cmp ax, 12                                  ; Convert 24 hour time to 12 hour time
    jg .pm

  .am:
    cmp ax, 0                                   ; Must fix value if zero
    jne .fixa
    add ax, 12                                  ; Fix value so it's 12:00a and not 0:00a
    
  .fixa:
    mov dx, 'a'                                 ; Put the 'a' for am into bx
    jmp .hour
    
  .pm:
    sub ax, 12                                  ; Correct the value for pm
    mov dx, 'p'                                 ; Put the 'p' for pm into bx
    
  .hour:
    push dx

    mov ch, 2                                   ; Pad length
    mov cl, ' '                                 ; Pad with spaces
    call videoWriteNumPadding
     
    mov al, ':'                                 ; Split with a slash
    call videoWriteChar
    
    pop dx
    pop ax
    push dx
    
    and ax, 0000011111100000b                   ; Mask bits for the month
    shr ax, 1                                   ; Right shift 5 places
    shr ax, 1
    shr ax, 1
    shr ax, 1
    shr ax, 1

    mov ch, 2                                   ; Pad length
    mov cl, '0'                                 ; Pad with spaces
    call videoWriteNumPadding
     
    pop ax
    call videoWriteChar
    
    mov al, 0x0a
    call videoWriteChar                         ; Line feed
    mov al, 0x0d
    call videoWriteChar                         ; Newline
    
  .nextFile:
    add di, 32
    mov byte al, [es:di]                        ; Grab the first byte of the next file

    cmp al, 0                                   ; Continue untill zero
    jnz .searchDir

    pop cx
  
    mov al, 0x09                                ; Write a tab
    call videoWriteChar  
    mov al, 0x09                                ; Write a tab
    call videoWriteChar  

    mov ax, cx                                  ; Write the number of files in the dir
    call videoWriteNum
    mov si, files                               ; Write the 'file(s)' string
    call videoWriteStr
    
    mov al, 0x09                                ; Write a tab
    call videoWriteChar  
    mov al, 0x09                                ; Write a tab
    call videoWriteChar   

    pop dx                                      ; The 32-bit filesize is in ax:dx now
    pop ax

    mov ch, cl
    mov cl, ' '                                 ; Padd with spaces
    call videoWriteNumPadding32                 ; Write the total size of the files in the dir

    mov si, bytes                               ; Write the 'bytes' string
    call videoWriteStr
    
    mov al, 0x0a
    call videoWriteChar                         ; Line feed
    mov al, 0x0d
    call videoWriteChar                         ; Newline

    pop bx
    pop dx
    pop ax
    call memFreeBlocks                          ; Free memory
    
    push es
    push ds
    push di
    push si
    push ax
    push bx
    push cx
    push dx
    jmp cliLoop
    
.memError:
.error:
  
;---------------------------------------------------
doRename:
;
; Command to rename a file.
;
; Expects: BX    = File to rename
;          CX    = New filename
;
; Returns: Nothing
;
;---------------------------------------------------

    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError
    
    or cx, cx                                   ; Ensure that the user inputted a filename
    jz paramError

    push ax                                     ; Save registers
    push bx
    push cx
    push di
    push es

    
    mov ax, bx
    mov bx, cx
    call renameFile
    jc .renameFailure
      
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    jmp cliLoop

  .renameFailure:
    push si
    
    mov si, fileNotFoundOrErr                   ; Write out an error message
    call videoWriteStr
    
    pop si
    
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    jmp cliLoop


;---------------------------------------------------
doNew:
;
; Command to delete a file.
;
; Expects: BX    = File to remove
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError

    push ax                                     ; Save registers
    push bx
    push cx
    push di
    push es
    
    xor dx, dx
    mov es, dx
    mov di, 0x5600
    
    mov ax, bx
    call createFile
    jc .deleteFailure
      
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    
    jmp cliLoop
    
  .deleteFailure:
    push si
    
    mov si, fileNotFoundOrErr                   ; Write out an error message
    call videoWriteStr
    
    pop si
    
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    jmp cliLoop


;---------------------------------------------------
doDel:
;
; Command to delete a file.
;
; Expects: BX    = File to remove
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError

    push ax                                     ; Save registers
    push bx
    push cx
    push di
    push es

    xor dx, dx
    mov es, dx
    mov di, 0x5600
    
    mov ax, bx
    call deleteFile
    jc .deleteFailure
      
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    
    jmp cliLoop
    
  .deleteFailure:
    push si
    
    mov si, fileNotFoundOrErr                   ; Write out an error message
    call videoWriteStr
    
    pop si
    
    pop es                                      ; Restore registers
    pop di
    pop cx
    pop bx
    pop ax
    jmp cliLoop
    
paramError:
    push si                                     ; Save registers
    
    mov si, requiredParamErr                    ; Write out an error message
    call videoWriteStr
    
    pop si                                      ; Restore registers
    
    jmp cliLoop


    
doHelp:
    push si

    mov si, cmdDir
    call videoWriteStr                          ; Print the command name
    mov si, cmdDirDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdRename
    call videoWriteStr                          ; Print the command name
    mov si, cmdRenDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdDel
    call videoWriteStr                          ; Print the command name
    mov si, cmdDelDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdType
    call videoWriteStr                          ; Print the command name
    mov si, cmdTypeDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdTime
    call videoWriteStr                          ; Print the command name
    mov si, cmdTimeDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdDate
    call videoWriteStr                          ; Print the command name
    mov si, cmdDateDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdHelp
    call videoWriteStr                          ; Print the command name
    mov si, cmdHelpDesc
    call videoWriteStr                          ; Print the command description

    pop si
    jmp cliLoop


doDump:
    push si
    

    push ds
    push es

    mov si, bx
    mov bx, 16
    call atoi

    push ax
    
    mov si, cx
    mov bx, 16
    call atoi

    pop bx
    mov es, bx
    mov di, ax
    
    mov si, .buffer

    
    mov cx, 0
  .processBytes:
    push cx
    xor dx, dx                                  ; Zero out the remander
    mov ax, cx                                  ; Set ax with the counter/ index
    mov bx, 16                                  ; Length of the hexdump
    div bx                                      ; Divide
    cmp dx, 0                                   ; Check for remainder
    jne .displayByte
    cmp cx, 0                                   ; Check counter for zeroth line
    je .displayAddr

    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    mov al, ' '                                 ; Whitespace
    call videoWriteChar

    mov si, .buffer                             ; Display the ascii buffer
    call videoWriteStr
    
    mov al, 0x0a                                ; Line feed
    call videoWriteChar
    mov al, 0x0d                                ; Newline
    call videoWriteChar

  .displayAddr:
    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    
    mov ax, es                                  ; Segment address
    mov bx, 16                                  ; Base 16 (hex format)
    mov cl, '0'                                 ; Pad with ascii zeros
    mov ch, 4                                   ; Length of pad 
    call videoWriteNumPadding

    mov al, ':'                                 ; Colon
    call videoWriteChar
    
    mov ax, di                                  ; Offset address
    mov bx, 16                                  ; Base 16 (hex format)
    mov cl, '0'                                 ; Pad with ascii zeros
    mov ch, 4                                   ; Length of pad 
    call videoWriteNumPadding

    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    
  .displayByte:                                 ; Display the current byte in the data
    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    
    xor ah, ah                                  ; Clear higher half of the number
    mov al, byte [es:di]                        ; Grab the byte to display
    mov bx, 16                                  ; Base 16 (hex format)
    mov cl, '0'                                 ; Pad with ascii zeros
    mov ch, 2                                   ; Length of pad 
    call videoWriteNumPadding

    mov bx, dx

    cmp al, 0x20
    jle .nonPrintable
    cmp al, 0x7e
    jg .nonPrintable

  .printable:                                   ; Add the printable char to the buffer
    mov byte [ds:si+bx], al
    jmp .nextByte
    
  .nonPrintable:                                ; Add a period to the buffer
    mov byte [ds:si+bx], '.'
    
  .nextByte:
    pop cx

    inc di
    
    inc cx
    cmp cx, (16*5)
    jne .processBytes

    mov al, ' '                                 ; Whitespace
    call videoWriteChar
    mov al, ' '                                 ; Whitespace
    call videoWriteChar

    mov si, .buffer                             ; Display the ascii buffer
    call videoWriteStr
    
    mov al, 0x0a                                ; Line feed
    call videoWriteChar
    mov al, 0x0d                                ; Newline
    call videoWriteChar
    
    pop es
    pop ds
    pop si
    jmp cliLoop
    .buffer times 18 db 0
    
;---------------------------------------------------
doWarranty:
;
; Display the GPL warranty notice.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push si
    
    mov si, __GPL_WARRANTY
    call videoWriteStr

    pop si
    jmp cliLoop
    
;---------------------------------------------------
doRedistrib:
;
; Display the GPL redistribute notice.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    push si
    
    mov si, __GPL_REDISTRIB
    call videoWriteStr

    pop si
    jmp cliLoop
    
;---------------------------------------------------
driveToAscii:
;
; Convert the drive number to an ascii letter.
;
; Expects: DL    = Drive number
;
; Returns: AL    = Drive ascii letter
;
;---------------------------------------------------
    cmp al, 0x00
    je .disketteA
    cmp al, 0x01
    je .disketteB
    cmp al, 0x02
    je .disketteC
    cmp al, 0x03
    je .disketteD
    cmp al, 0x80
    je .driveC
    cmp al, 0x81
    je .driveD
    jmp .unknown?

  .disketteA:
    mov al, "A"                                 ; Diskette drive A
    jmp .done
    
  .disketteB:
    mov al, "B"                                 ; Diskette drive B
    jmp .done

  .disketteC:
    mov al, "C"                                 ; Diskette drive C
    jmp .done

  .disketteD:
    mov al, "D"                                 ; Diskette drive D
    jmp .done
    
  .driveC:
    mov al, "C"                                 ; Hard drive     C
    jmp .done
    
  .driveD:
    mov al, "D"                                 ; Hard drive     D
    jmp .done
    
  .unknown?:
    mov al, "?"                                 ; Unknown drive  ?

  .done:
    ret
    
