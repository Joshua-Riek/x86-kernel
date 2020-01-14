


;--------------------------------------------------
fatReadRootDir:
;
; Load the root directory to the appointed buffer
; from the the drive param block buffer passed.
;
; Expects: ES:DI = Disk buffer
;
; Returns: CF    = Carry Flag set on error
;
;--------------------------------------------------
    push ax                                     ; Save registers
    push cx

    
    mov cx, word [rootDirSize]                  ; Read the size in sectors of the directory
    mov ax, word [rootDirSector]                ; Starting sector of the directory
    call readSectors                            ; Read the sectors 
    jc .error

    pop cx                                      ; Restore registers
    pop ax
    
    clc                                         ; Clear carry, for no error
    ret

  .memError:
  .error:
    pop cx                                      ; Restore registers
    pop ax

    stc                                         ; Set carry, error occured
    ret
