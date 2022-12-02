;  test.asm
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

%macro deleteFileMacro 1
    jmp %%endstr1
  %%str1:
    db %1, 0
  %%endstr1:
    mov ax, %%endstr1
    call deleteFile
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

;---------------------------------------------------
fileTesting:
;
; Testing for file i/o operations.
;
; Expects: None
;
; Returns: None
;
;---------------------------------------------------
%ifdef DEBUG
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es

    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10
    deleteFileMacro "BAR.TXT"
    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10

    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10
    deleteFileMacro "FOO.TXT"
    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10

    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10
    deleteFileMacro "BAZ.TXT"
    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10

    createDirMacro "FOO"
    changeDirMacro "FOO"

    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10
    deleteFileMacro "BAR.TXT"
    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10

    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10
    deleteFileMacro "FOO.TXT"
    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10

    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10
    deleteFileMacro "BAZ.TXT"
    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10

    createDirMacro "BAR"
    changeDirMacro "BAR"

    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10
    deleteFileMacro "BAR.TXT"
    writeFileMacro "BAR.TXT", "Urna sem ipsum ligula eu libero est bibendum.", 13, 10

    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10
    deleteFileMacro "FOO.TXT"
    writeFileMacro "FOO.TXT", "Ornare eget quisque hendrerit sapien fames adipiscing neque venenatis.", 13, 10

    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10
    deleteFileMacro "BAZ.TXT"
    writeFileMacro "BAZ.TXT", "Quisque amet enim elit viverra eleifend sociosqu morbi libero.", 13, 10

    changeDirMacro ".."

    writeFileMacro "FOOBAR.TXT", "Suscipit netus odio sem rhoncus auctor ullamcorper semper pretium malesuada orci.", 13, 10
    deleteFileMacro "FOOBAR.TXT"
    writeFileMacro "FOOBAR.TXT", "Suscipit netus odio sem rhoncus auctor ullamcorper semper pretium malesuada orci.", 13, 10

    writeFileMacro "BARFOO.TXT", "Ut mollis euismod libero netus egestas curae ipsum sagittis habitasse.", 13, 10
    deleteFileMacro "BARFOO.TXT"
    writeFileMacro "BARFOO.TXT", "Ut mollis euismod libero netus egestas curae ipsum sagittis habitasse.", 13, 10

    writeFileMacro "FOOBAZ.TXT", "Nisl vitae vulputate enim tempus non arcu proin.", 13, 10
    deleteFileMacro "FOOBAZ.TXT"
    writeFileMacro "FOOBAZ.TXT", "Nisl vitae vulputate enim tempus non arcu proin.", 13, 10

    changeDirMacro ".."

    writeFileMacro "FOOBAR.TXT", "Suscipit netus odio sem rhoncus auctor ullamcorper semper pretium malesuada orci.", 13, 10
    deleteFileMacro "FOOBAR.TXT"
    writeFileMacro "FOOBAR.TXT", "Suscipit netus odio sem rhoncus auctor ullamcorper semper pretium malesuada orci.", 13, 10

    writeFileMacro "BARFOO.TXT", "Ut mollis euismod libero netus egestas curae ipsum sagittis habitasse.", 13, 10
    deleteFileMacro "BARFOO.TXT"
    writeFileMacro "BARFOO.TXT", "Ut mollis euismod libero netus egestas curae ipsum sagittis habitasse.", 13, 10

    writeFileMacro "FOOBAZ.TXT", "Nisl vitae vulputate enim tempus non arcu proin.", 13, 10
    deleteFileMacro "FOOBAZ.TXT"
    writeFileMacro "FOOBAZ.TXT", "Nisl vitae vulputate enim tempus non arcu proin.", 13, 10

    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
%endif
    ret
