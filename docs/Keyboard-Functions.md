### setupKbdCtrl
Replace the current keyboard related interrupt with my own

* IN: `Nothing`
* OUT: `CF` = Carry flag set on error

### kbdCtrlRead
Read a byte from the keyboard controller

* IN: `Nothing`
* OUT: `AL` = Byte from read

### kbdCtrlWrite
Send a byte to the keyboard controller

* IN: `AL` = Byte to write
* OUT: `Nothing`

### kbdCtrlSendCmd
Send a command to the keyboard controller

* IN: `BL` = Command byte
* OUT: `Nothing`

### kbdCtrlSelfTest
Preform a self test of the keyboard controller

* IN: `Nothing`
* OUT: `CF` = Carry flag set on error

### kbdCtrlSetLeds
Set the keyboard leds

* IN: `Nothing`
* OUT: `Nothing`

### kbdCtrlWaitUntillKey
Wait untill a key is pressed

* IN: `Nothing`
* OUT: `AH, AL` = Scan code, Ascii code

### kbdCtrlCaptureInput
Get input from the keyboard and fill into the buffer

* IN: `DS:SI` = Location of input buffer
* OUT: `DS:SI` = Buffer updated with keys pressed

### kbdCtrlHandler
Custom interrupt service/ handler for the keyboard controller

* IN: `Nothing`
* OUT: `Nothing`

### kbdEncSendCmd
Send a command to the keyboard encoder

* IN: `BL` = Command byte
* OUT: `Nothing`

### scanToAcsii
Convert a scan code to ascii and set status flags

* IN: `AL` = Scan code
* OUT: `AH, AL` = Scan code, Ascii code 

### kbdBiosFlushBuffer
Flush the most recent key in the BIOS keyboard buffer

* IN: `Nothing`
* OUT: `Nothing`

### kbdBiosCheckBuffer
Check the BIOS keyboard buffer for a key

* IN: `Nothing`
* OUT: `AX` = Zero when empty

### kbdBiosGetChar
Get a character and scan code from the BIOS keyboard buffer

* IN: `Nothing`
* OUT: `AH, AL` = Scan code, Ascii code

### kbdBiosStoreKey
Shove a character and scan code into the BIOS keyboard buffer

* IN: `CH, CL` = Scan code, Ascii code
* OUT: `AX` = 0 Buffer full, 1 successfully stored

### kbdBiosWaitUntillKey
Wait until a new character and scan code are in the BIOS keyboard buffer

* IN: `Nothing`
* OUT: `AH, AL` = Scan code, Ascii code

### kbdBiosCaptureInput
Get input from the BIOS keyboard and fill into the buffer

* IN: `DS:SI` = Location of input buffer
* OUT: `DS:SI` = Buffer updated with keys pressed
