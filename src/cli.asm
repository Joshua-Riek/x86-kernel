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
    
;---------------------------------------------------
; Cli functions
;---------------------------------------------------   
;
; doDir IN=> None; OUT=> None
; doRename IN=> BX=Ptr to str, CX=Ptr to str; OUT=> None
; doDel IN=> BX=Ptr to str; OUT=> None
; doType IN=> BX=Ptr to str; OUT=> None
; doCopy IN=> BX=Ptr to str, CX=Ptr to str; OUT
; doCls IN=> None; OUT=> None
; doTime IN=> None; OUT=> None
; doDate IN=> None; OUT=> None
; doHelp IN=> None; OUT=> None
; doCd IN=> BX=Ptr to str; OUT=> None
; doWarranty IN=> None; OUT=> None
; doRedistrib IN=> None; OUT=> None
; driveToAscii IN=> DL=Drive; OUT=> AL=Ascii drive letter
    
;---------------------------------------------------
; Cli varables
;---------------------------------------------------

    requiredParamErr  db "Required parameter missing", 13, 10, 0
    fileNotFoundOrErr db "Duplicate file name or file not found", 13, 10, 0
    readSectorErr     db "Error reading from device", 0
    writeSectorErr    db "Error writing to device", 0
    badCommandErr     db "Bad command or filename", 13, 10, 0
    needFileNameErr   db "Missing file name", 13, 10, 0
    fileNotFoundErr   db "File not found", 13, 10, 0
    memoryErr         db "Insufficient disk space", 13, 10, 0
    fileCreateErr     db "File creation error", 13, 10, 0    
    badDirErr         db "Invalid directory", 13, 10, 0
    badMkDirErr       db "Unable to create directory", 13, 10, 0
    badRmDirErr       db "Not a directory or the directory is not empty", 13, 10, 0
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
    cmdCopy           db "COPY", 0
    cmdCls            db "CLS", 0
    cmdEdit           db "EDIT", 0
    cmdDate           db "DATE", 0
    cmdTime           db "TIME", 0
    cmdHelp           db "HELP", 0
    cmdCd             db "CD", 0
    cmdChdir          db "CHDIR", 0
    cmdMd             db "MD", 0
    cmdMkdir          db "MKDIR", 0
    cmdRd             db "RD", 0
    cmdRmdir          db "RMDIR", 0
    cmdWarranty       db "WARRANTY", 0
    cmdRedistrib      db "REDISTRIB", 0
    cmdDump           db "DUMP", 0
    
    cmdDirDesc        db 9, 9, "List the contents of a directory", 13, 10, 0
    cmdRenDesc        db 9,    "Renames a file or directory", 13, 10, 0
    cmdDelDesc        db 9, 9, "Deletes a file", 13, 10, 0
    cmdTypeDesc       db 9,    "Display the contents of a file", 13, 10, 0
    cmdCopyDesc       db 9,    "Copy a file to an alternate location", 13, 10, 0
    cmdClsDesc        db 9, 9, "Clears the screen", 13, 10, 0
    cmdTimeDesc       db 9,    "View the system time", 13, 10, 0
    cmdDateDesc       db 9,    "View the system date", 13, 10, 0
    cmdHelpDesc       db 9,    "List the available commands", 13, 10, 0
    cmdCdDesc         db 9, 9, "Change directories", 13, 10, 0
    cmdMdDesc         db 9, 9, "Creates a directory", 13, 10, 0
    cmdRdDesc         db 9, 9, "Removes an empty directory", 13, 10, 0
    
    cliBuff times 96  db 0
    pathBuf times 96  db 0

;---------------------------------------------------
cliLoop:
;
; This is the main command line interface loop.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    mov cx, cs
    mov ds, cx
    mov es, cx
    
    mov si, cliBuff
    mov cx, 96
    
  .zeroLoop:
    mov byte [ds:si], 0
    inc si
    loop .zeroLoop

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
    mov si, pathBuf
    call videoWriteStr
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
    mov ah, ' '
    mov si, cliBuff
    mov di, cliBuff
    call parseStr                               ; Parse through the user input string

    mov di, si

  .parseCommands:
    mov al, byte [ds:si]
    cmp al, 0                                   ; Do nothing if input empty
    je .displayPrompt

    mov si, cmdDir                              ; Dir
    call strCmp
    jc doDir

    mov si, cmdRename                           ; Rename
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
    
    mov si, cmdType                             ; Type
    call strCmp
    jc doType

    mov si, cmdCopy                             ; Copy
    call strCmp
    jc doCopy

    mov si, cmdCls                              ; Cls
    call strCmp
    jc doCls

    mov si, cmdTime                             ; Time
    call strCmp
    jc doTime

    mov si, cmdDate                             ; Date
    call strCmp
    jc doDate
    
    mov si, cmdHelp                             ; Help
    call strCmp
    jc doHelp

    mov si, cmdCd                               ; Cd
    call strCmp
    jc doCd

    mov si, cmdChdir
    call strCmp
    jc doCd
    
    mov si, cmdMd                               ; Md
    call strCmp
    jc doMd

    mov si, cmdMkdir
    call strCmp
    jc doMd

    mov si, cmdRd                               ; Rd
    call strCmp
    jc doRd

    mov si, cmdRmdir
    call strCmp
    jc doRd

    mov si, cmdWarranty                         ; Warranty
    call strCmp
    jc doWarranty
    
    mov si, cmdRedistrib                        ; Redistrib
    call strCmp
    jc doRedistrib

    jmp doLoad
    mov si, badCommandErr                       ; Bad command
    call videoWriteStr
    jmp cliLoop

;---------------------------------------------------
doDir:
;
; Display the contents of the root directory.
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
    push es
    push ds
    
    call loadCwd                                 ; Allocate and load the dir into memory
    jc .loadDirError

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
    add dx, word [es:di+dirFat.filesize+2]
    
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
    xor ax, ax
    mov al, byte [es:di+dirFat.attributes]      ; Get the file atribute byte
    cmp al, 0x10                                ; Check to see if its a directory
    jne .fileEntry
    
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    mov al, '<'
    call videoWriteChar
    mov al, 'D'
    call videoWriteChar
    mov al, 'I'
    call videoWriteChar
    mov al, 'R'
    call videoWriteChar
    mov al, '>'
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    mov al, ' '
    call videoWriteChar
    jmp .printDate
    
  .fileEntry:
    mov ax, word [es:di+dirFat.filesize]        ; Lo word of filesize
    mov dx, word [es:di+dirFat.filesize+2]      ; Hi word of filesize
    mov bx, 10                                  ; Decimal int
    mov ch, 10                                  ; Pad length
    mov cl, ' '                                 ; Pad with spaces
    call videoWriteNumPadding32

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
    mov bx, 10                                  ; Decimal int
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

    mov bx, 10
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
    
    call unloadCwd
    
    pop si
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
  .loadDirError:
    mov si, readSectorErr
    call videoWriteStr
    
    pop si
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
  
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

    push si
    push di

    mov si, bx
    call fileExists                             ; Check to see if the filename exists
    jc .fileNotFound
    
    mov si, cx
    call fileExists                             ; Check to see if the filename allready exists
    jnc .fileAllreadyExists
    
    mov si, bx                                  ; File to rename
    mov di, cx                                  ; New filename
    call renameFile                             ; Now call the rename file function
    jc .renameFailure

    pop di
    pop si
    
    jmp cliLoop
    
  .fileNotFound:
    mov si, fileNotFoundErr
    call videoWriteStr
    jmp .error
    
  .fileAllreadyExists:
    mov si, fileNotFoundOrErr
    call videoWriteStr
    jmp .error
    
  .renameFailure:
    mov si, writeSectorErr
    call videoWriteStr

  .error:
    pop di
    pop si
    
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

    push si

    mov si, bx                                  ; File to delete
    call deleteFile                             ; Call the function to delete the file
    jc .deleteFailure

    pop si
    jmp cliLoop
    
  .deleteFailure:    
    mov si, fileNotFoundOrErr                   ; Write out an error message
    call videoWriteStr
    
    pop si
    jmp cliLoop

;---------------------------------------------------
doType:
;
; Command to show the contents of a file.
;
; Expects: BX    = File to show
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError
    
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov si, bx                                  ; Start by getting the filesize,
    call fileSize                               ; also tests to see if the file exists
    jc .fileNotFound
    
    call memAllocBytes
    jc .memError
    
    call readFile                               ; Now we read the file into memory
    jc .readFailure                             ; Size goes into ax:dx

    call memFreeBytes                           ; Free up the memory

    xchg ax, dx
    
    cmp ax, 0
    jne .start                                  ; If ax:dx are is empty, we are done
    cmp dx, 0
    je .done

  .start:
    mov cx, ax
    
  .readBytes:
    mov al, byte [es:di]                        ; Grab the next byte from the file
    call videoWriteChar

    clc
    add di, 1                                   ; Increaese the pointer, buffer fix
    jnc .nextByte 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when bx overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000
    pop dx
    
  .nextByte:
    dec cx
    
    cmp cx, 0                                   ; Decrease counter and see if we are at the end
    jne .readBytes
    cmp dx, 0
    je .done

    sub dx, 1
    mov cx, 0xffff
    jmp .nextByte
    
  .done:
    mov al, 0x0a
    call videoWriteChar                         ; Line feed
    mov al, 0x0d
    call videoWriteChar                         ; Newline
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
  .readFailure:  
    call memFreeBytes                           ; Free up the memory used on error
    mov si, readSectorErr
    call videoWriteStr
    jmp .error
    
  .fileNotFound:
    mov si, fileNotFoundErr
    call videoWriteStr
    jmp .error
    
  .memError:
    mov si, memoryErr
    call videoWriteStr
    
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

;---------------------------------------------------   
doCopy:
;
; Command to copy one file to another.
;
; Expects: BX    = File to copy
;          CX    = File to create and write
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
    push dx
    push si
    push di
    push es
    push ds

    mov si, cx
    call fileExists                             ; See if the file to create allready exists
    jnc .fileAllreadyExists
    
    mov si, bx                                  ; Start by getting the filesize,
    call fileSize                               ; also tests to see if the file exists
    jc .fileNotFound

    call memAllocBytes                          ; Allocate space for the file 
    jc .memError
    
    call readFile                               ; Now we read the file into memory
    jc .readFailure                             ; Size goes into ax:dx

    mov si, cx
    call writeFile                              ; Finally write data into a new file
    jc .writeFailure
    
    call memFreeBytes                           ; Free up the memory
       
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
  .writeFailure:
    call memFreeBytes                           ; Free up the memory used on error
    mov si, writeSectorErr
    call videoWriteStr
    jmp .error

  .readFailure:  
    call memFreeBytes                           ; Free up the memory used on error
    mov si, readSectorErr
    call videoWriteStr
    jmp .error
    
  .fileAllreadyExists: 
    mov si, fileNotFoundOrErr
    call videoWriteStr
    jmp .error
    
  .fileNotFound:
    mov si, fileNotFoundErr
    call videoWriteStr
    jmp .error
    
  .memError:
    mov si, memoryErr
    call videoWriteStr
    
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

;---------------------------------------------------   
doCls:
;
; Command to clear the screen.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------   
    call videoClearScreen

    jmp cliLoop

;---------------------------------------------------   
doTime:
;
; Command to display the system time.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    mov si, currentTime
    call videoWriteStr
    
    mov ah, 0x02                                ; Read RTC time
    int 0x1a                                    ; System and RTC BIOS services

    mov al, ch                                  ; Hours in BCD
    call bcd                                    ; Convert BCD
    mov ch, al
    
    mov al, cl                                  ; Minutes in BCD
    call bcd                                    ; Convert BCD
    mov cl, al
    
    mov al, dh                                  ; Seconds in BCD
    call bcd                                    ; Convert BCD
    mov dh, al

    mov bx, 10                                  ; Base 10 number
    xor ah, ah
    mov al, ch                                  ; Hours
    call videoWriteNum                          ; Write as a number

    mov al, ':'
    call videoWriteChar
    
    mov al, cl                                  ; Minutes
    call videoWriteNum                          ; Write as a number

    mov al, ':'
    call videoWriteChar
        
    mov al, dh                                  ; Seconds
    call videoWriteNum                          ; Write as a number
    
    jmp cliLoop
    
;---------------------------------------------------   
doDate:
;
; Command to display the system date.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
    mov si, currentDate
    call videoWriteStr

    mov ah, 0x04                                ; Read RTC date
    int 0x1a                                    ; System and RTC BIOS services

    mov al, ch                                  ; Century in BCD
    call bcd                                    ; Convert BCD
    mov ch, al
    
    mov al, cl                                  ; Year in BCD
    call bcd                                    ; Convert BCD
    mov cl, al
    
    mov al, dh                                  ; Month in BCD
    call bcd                                    ; Convert BCD
    mov dh, al

    mov al, dl                                  ; Day in BCD
    call bcd                                    ; Convert BCD
    mov dl, al

    mov bx, 10                                  ; Base 10 number
    xor ah, ah
    mov al, dh                                  ; Month
    call videoWriteNum                          ; Write as a number

    mov al, '-'
    call videoWriteChar

    mov al, dl                                  ; Day
    call videoWriteNum                          ; Write as a number

    mov al, '-'
    call videoWriteChar

    mov al, ch                                  ; Century
    call videoWriteNum                          ; Write as a number

    mov al, cl                                  ; Year
    mov cl, '0'                                 ; Write as a padded number
    mov ch, 2
    call videoWriteNumPadding

    jmp cliLoop

    
paramError:
    push si                                     ; Save registers
    
    mov si, requiredParamErr                    ; Write out an error message
    call videoWriteStr
    
    pop si                                      ; Restore registers
    
    jmp cliLoop


    
;---------------------------------------------------
doHelp:
;
; Command to display system cli commands.
;
; Expects: Nothing
;
; Returns: Nothing
;
;---------------------------------------------------
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

    mov si, cmdCopy
    call videoWriteStr                          ; Print the command name
    mov si, cmdCopyDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdCls
    call videoWriteStr                          ; Print the command name
    mov si, cmdClsDesc
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

    mov si, cmdCd
    call videoWriteStr                          ; Print the command name
    mov si, cmdCdDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdMd
    call videoWriteStr                          ; Print the command name
    mov si, cmdMdDesc
    call videoWriteStr                          ; Print the command description

    mov si, cmdRd
    call videoWriteStr                          ; Print the command name
    mov si, cmdRdDesc
    call videoWriteStr                          ; Print the command description
    
    pop si
    jmp cliLoop
    
;---------------------------------------------------
doCd:
;
; Command to change the current directory.
;
; Expects: BX    = Directory
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError
    
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov si, bx
    call changeDir                              ; Attempt to change current dir
    jc .changeDirError
    
    mov ax, word [ds:bx]
    cmp ax, '..'
    jne .1dot

  .2dots:
    mov si, pathBuf
    call strLen
    add si, cx                                  ; Start at the end of the current path string
    dec si                                      ; Move back one to get the last char (should be a '\') 
    mov al, 0                                   ; Then fill it with a zero /null byte
    mov byte [ds:si], al
    dec si
    
  .search1:
    mov al, byte [ds:si]                        ; Grab the next byte from the current path string
    cmp al, 0x5c                                ; Check for a slash '\'
    je .done
    mov al, 0                                   ; If no slash, fill the character with a null byte
    mov byte [ds:si], al
    dec si
    loop .search1
    jmp .done
    
  .1dot:
    cmp al, '.'
    je .done

  .dirName:
    mov di, bx
    mov si, pathBuf
    call strLen
    add si, cx

  .search2:   
    mov al, byte [es:di]                        ; Grab the next byte from dir string
    call charToUpper                            ; Convert the char to uppercase
    mov byte [ds:si], al                        ; And shove it into the current path string
    inc si
    inc di
    cmp al, 0                                   ; Check for the end of the string
    je .end
    jmp .search2

  .end:
    dec si
    mov byte [ds:si], 0x5c                      ; End it with a slash

  .done:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

  .changeDirError: 
    mov si, badDirErr                           ; Write out an error message
    call videoWriteStr

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
;---------------------------------------------------
doMd:
;
; Command to make a new directory.
;
; Expects: BX    = Directory
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError
    
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov si, bx
    call createDir                              ; Attempt to create a new dir
    jc .createDirError
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

  .createDirError: 
    mov si, badMkDirErr                         ; Write out an error message
    call videoWriteStr

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
;---------------------------------------------------
doRd:
;
; Command to remove a directory.
;
; Expects: BX    = Directory
;
; Returns: Nothing
;
;---------------------------------------------------
    or bx, bx                                   ; Ensure that the user inputted a filename
    jz paramError
    
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov si, bx
    call removeDir                              ; Attempt to remove a dir
    jc .removeDirError
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

  .removeDirError: 
    mov si, badRmDirErr                         ; Write out an error message
    call videoWriteStr

    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop


;---------------------------------------------------
doLoad:
;
; Command to load a file into memory.
;
; Expects: AX    = File to load
;
; Returns: Nothing
;
;---------------------------------------------------
    or ax, ax                                   ; Ensure that the user inputted a filename
    jz paramError
    
    push ax                                     ; Save registers
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov si, di                                  ; Start by getting the filesize,
    call fileSize                               ; also tests to see if the file exists
    jc .fileNotFound
    
    call memAllocBytes
    jc .memError
    
    call readFile                               ; Now we read the file into memory
    jc .readFailure                             ; Size goes into ax:dx

    push es
    push ds
    push ax
    push dx
    
    mov dx, es
    mov cx, di                                  ; Here i dont want to work with offsets, 
    shr cx, 1                                   ; so shift it out right 4 times then
    shr cx, 1                                   ; add that to the segment
    shr cx, 1
    shr cx, 1
    add dx, cx
    xor cx, cx

    push cs                                     ; Setup a far jump so we can return to the kernel 
    call .getIp
  .getIp:                                       ; Use a call to get the current IP
    pop ax
    add ax, 8                                   ; We can add 8 to the IP so we return below retf
    push ax
    
    push dx                                     ; The new Code Segment
    push cx                                     ; The new Instruction Pointer
    
    retf                                        ; Far jump to the loaded program!

    pop dx
    pop ax
    pop ds
    pop es
    call memFreeBytes                           ; Free up the memory
    
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop
    
  .readFailure:  
    call memFreeBytes                           ; Free up the memory used on error
    mov si, readSectorErr
    call videoWriteStr
    jmp .error
    
  .fileNotFound:
    mov si, badCommandErr
    call videoWriteStr
    jmp .error
    
  .memError:
    mov si, memoryErr
    call videoWriteStr
    
  .error:
    pop ds                                      ; Restore registers
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    jmp cliLoop

    
doDump:
    push si
    
i
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
