## setupVideo 
Get the cursor position from the BIOS, then save for use
* IN: `Nothing`
* OUT: `Nothing`

## videoWriteChar 
Write a character into video memory
* IN: `AL` = Ascii char
* OUT: `Nothing`

## videoWriteStr
Write a string into video memory
* IN: `DS:SI` = Pointer to string
* OUT: `Nothing`

## videoWriteNumPadding32 
Write a 32-bit number into video memory with padding
* IN: `AX:DX, BX, CH, CL` = Number, Base of number, Padding length, Char to pad with
* OUT: `Nothing`

## videoWriteNumPadding
Write a 16-bit number into video memory with padding
* IN: `AX, BX, CH, CL` = Number, Base of number, Padding length, Char to pad with
* OUT: `Nothing`

## videoWriteNum32 
Write a 32-bit number into video memory
* IN: `AX:DX, BX` = Number, Base of number
* OUT: `Nothing`

## videoWriteNum 
Write a 16-bit number into video memory
* IN: `AX, BX` = Number, Base of number
* OUT: `Nothing`

## videoScroll
Scroll the screen up one line, if cursor is on the last available line.
* IN: `Nothing`
* OUT: `Nothing`

## videoClearScreen
Clear the screen of text
* IN: `Nothing`
* OUT: `Nothing`

## videoSaveScreen
Save the contents of the screen into memory
* IN: `Nothing`
* OUT: `Nothing`

## videoRestoreScreen
Restore the contents of the screen from memory
* IN: `Nothing`
* OUT: `Nothing`

## videoUpdateBiosCur 
Update the BIOS cursor
* IN: `Nothing`
* OUT: `Nothing`

## videoUpdateCur
Update the hardware cursor
* IN: `Nothing`
* OUT: `Nothing`