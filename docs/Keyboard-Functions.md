### kbdFlushBuffer
Flush the most recent key in the BIOS keyboard buffer
* IN: `Nothing`
* OUT: `Nothing`

### kbdCheckBuffer
Check the BIOS keyboard buffer for a key
* IN: `Nothing`
* OUT: `AX` = Zero when empty

### kbdGetChar
Get a character and scan code from the BIOS keyboard buffer
* IN: `Nothing`
* OUT: `AH, AL` = Scan code, Ascii code

### kbdStoreKey
Shove a character and scan code into the BIOS keyboard buffer
* IN: `CH, CL` = Scan code, Ascii code
* OUT: `AX` = 0 Buffer full, 1 successfully stored

### kbdWaitUntillKey
Wait until a new character and scan code are in the BIOS keyboard buffer
* IN: `Nothing`
* OUT: `AH, AL` = Scan code, Ascii code

### kbdCaptureInput
Get input from the keyboard and fill into the buffer
* IN: `DS:SI` = Location of input buffer
* OUT: `DS:SI` = Buffer updated with keys pressed
