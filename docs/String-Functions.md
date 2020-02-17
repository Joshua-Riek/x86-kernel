### itoa
Converts a 32-bit number and base to a string
* IN: `DS:SI, AX:DX, BX` = Pointer to string buffer, 32-bit number, Base of number
* OUT: `DS:SI` = Pointer now contains a string

### atoi
Converts a string and base to a 16-bit number
* IN: `DS:SI, BX` = Pointer to string buffer, Base of number
* OUT: `AX` = Number from string

### convertFilename83
Convert the filename into a fat formatted filename (8.3 format)
* IN: `DS:SI, ES:DI` = Pointer to filename, Pointer for converted filename
* OUT: `ES:DI` = Pointer now contains the converted filename

### padStr 
Pad a string with Ascii characters
* IN: `DS:SI, ES:DI, AL, CX` = Pointer to string for padding, Pointer to string buffer, Char to pad with, Length of padding
* OUT: `ES:DI` = Pointer now contains the padded string

### parseStr
Split the string into tokens
* IN: `DS:SI` = Pointer to string for parsing 
* OUT: `AX, BX, CX, DX` = Pointer to token 1, Pointer to token 2, Pointer to token 3, pointer to token 4

### strCmp
Checks to see if the two passed strings are equal to each other or not
* IN: `DS:SI, ES:DI` = Pointer to string, Pointer to string
* OUT: `CF` = Carry flag set if strings are equal

### strLen
Get the length of a string
* IN: `DS:SI` = Pointer to string
* OUT: `CX` = Length of string

### charToLower
Convert an Acsii letter to a lowercase character
* IN: `AL` = Ascii character
* OUT: `AL` = Lowercase Ascii character

### charToUpper
Convert an Acsii letter to a uppercase character
* IN: `AL` = Ascii character
* OUT: `AL` = Uppercase Ascii character
