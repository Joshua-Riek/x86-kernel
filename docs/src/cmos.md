# Cmos functions

### cmosRead
Read the contents from a chosen CMOS register
* IN: `AL` = CMOS address to read
* OUT: `AH` = Contents from read

### cmosWrite
Write to the chosen CMOS register
* IN: `AL, AH` = CMOS address to write, Value to write
* OUT: `Nothing`

### cmosReadDate
Get the system date from the CMOS
* IN: `Nothing`
* OUT: `AL, CX, DH, DL` = Week, Year, Month, Day

### cmosReadTime
Get the system time from the CMOS
* IN: `Nothing`
* OUT: `CH, CL, DH` = Hour, Minute, Second

### cmosDelay
Use the CMOS to wait a period of time in seconds
* IN: `CX` = Seconds to wait for
* OUT: `Nothing`

### bcd
Convert a value into a binary-coded decimal (BCD)
* IN: `AL` = Number to convert
* OUT: `AL` = Converted number
