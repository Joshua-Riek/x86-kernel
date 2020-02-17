### readClustersFAT12
Follow the chain of FAT12 clusters and load their data into memory
* IN: `ES:DI, AX` = Where to load the contents of the clusters in memory, Starting cluster
* OUT: `CF` = Carry flag set on error

### readClustersFAT16
Follow the chain of FAT16 clusters and load their data into memory
* IN: `ES:DI, AX` = Where to load the contents of the clusters in memory, Starting cluster
* OUT: `CF` = Carry flag set on error

### readClusters
Determine if to use FAT12 or FAT16, then follow the corresponding cluster chain and load it's data into memory
* IN: `ES:DI, AX` = Where to load the contents of the clusters in memory, Starting cluster
* OUT: `CF` = Carry flag set on error

### removeClustersFAT12
Follow a chain of FAT12 clusters and remove them from the FAT table
* IN: `AX` = Starting cluster
* OUT: `CF` = Carry flag set on error

### removeClustersFAT16
Follow a chain of FAT16 clusters and remove them from the FAT table
* IN: `AX` = Starting cluster
* OUT: `CF` = Carry flag set on error

### removeClusters
Determine if to use FAT12 or FAT16, then follow the corresponding cluster chain and remove them from the FAT table
* IN: `AX` = Starting cluster
* OUT: `CF` = Carry flag set on error

### writeClustersFAT12
Calculate and write a new set of clusters into FAT12, then follow the created chain of clusters and write their corresponding data to the disk
* IN: `ES:DI, AX` = Location of the data to write, 32-bit file size
* OUT: `CX, CF` = Start of cluster, Carry flag set on error

### writeClustersFAT16
Calculate and write a new set of clusters into FAT16, then follow the created chain of clusters and write their corresponding data to the disk
* IN: `ES:DI, AX` = Location of the data to write, 32-bit file size
* OUT: `CX, CF` = Start of cluster, Carry flag set on error

## writeClusters
Determine if to use FAT12 or FAT16, then calculate and write a new set of clusters into FAT, following the created chain of clusters and writing their corresponding data to the disk
* IN: `ES:DI, AX` = Location of the data to write, 32-bit file size
* OUT: `CX, CF` = Start of cluster, Carry flag set on error
