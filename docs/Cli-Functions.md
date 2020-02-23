

### cliLoop
The main loop for the command line interface
* IN: `Nothing`
* OUT: `Nothing`

### doDir
Display the contents of the current directory
* IN: `Nothing`
* OUT: `Nothing`

### doRename
Command to rename a file
* IN: `BX, CX` = File to rename, New file name
* OUT: `Nothing`

### doDel
Command to delete a file
* IN: `BX` = File to delete
* OUT: `Nothing`

### doType
Command to show the contents of a file
* IN: `BX` = File to see the contents of
* OUT: `Nothing`

### doCopy
Command to copy one file to another
* IN: `BX, CX` = File to copy, File to create 
* OUT: `Nothing`

### doCls
Command to clear the screen
* IN: `Nothing`
* OUT: `Nothing`

### doTime
Command to display the system time
* IN: `Nothing`
* OUT: `Nothing`

### doDate
Command to display the system date
* IN: `Nothing`
* OUT: `Nothing` 

### doHelp
Command to show the system cli commands
* IN: `Nothing`
* OUT: `Nothing`

### doCd
Command to change the current directory
* IN: `BX` = Name of directory
* OUT: `Nothing` 

### doMd
Command to make a new directory
* IN: `BX` = Name of directory 
* OUT: `Nothing`

### doRd
Command to remove a directory
* IN: `BX` = Name of directory 
* OUT: `Nothing`

