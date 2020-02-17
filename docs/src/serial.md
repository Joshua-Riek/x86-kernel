# Serial Functions

### initSerial
Setup the serial (com1) port with 9600 baud, 8 bits, no parity, and one stop bit
* IN: `Nothing`
* OUT: `Nothing`

### writeSerial
Write some data to the serial (com1) port
* IN: `AL` = Data
* OUT: `Nothing`

### readSerial
Read some data from the serial (com1) port
* IN: `Nothing`
* OUT: `AL` = Data

### writeSerialStr
Write a string to the serial port (usefull for debugging)
* IN: `DS:SI` = Pointer to string
* OUT: `Nothing`

### writeSerialNumPadding32 
Write a 32-bit number to the serial port with padding (usefull for debugging)
* IN: `AX:DX, BX, CH, CL` = Number, Base of number, Padding length, Char to pad with
* OUT: `Nothing`
