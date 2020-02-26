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


    ; system detection
    ; System type:
    ; System BIOS:
    ; Diskette a:
    ; Diskette b:
    ; Hard Drive 0:
    ; Hard Drive 0:
    ; Video Adapter:
    ; Video Vendor:
