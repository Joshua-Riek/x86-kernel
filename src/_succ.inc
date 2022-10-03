;
;  System definitions and macros.
;  Copyright (c) 2017-2022 Joshua Riek
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

%ifndef __SUCC_INC_
%define __SUCC_INC_ 1

%define __SUCC_MAJOR_VERSION 1
%define __SUCC_MINOR_VERSION 4
%define __SUCC_PATCHLEVEL    1

%defstr __SUCC_VERSION       %[__SUCC_MAJOR_VERSION].%[__SUCC_MINOR_VERSION].%[__SUCC_PATCHLEVEL]

struc bpb                                       ; BPB - BIOS Parameter Block
  .bootJump:          resb 3                    ; Jump instruction over the OEM / BIOS param block
  .OEMName:           resb 8                    ; Disk label
  .bytesPerSector:    resw 1                    ; Bytes per sector
  .sectorsPerCluster: resb 1                    ; Sectors per cluster
  .reservedSectors:   resw 1                    ; Reserved sectors
  .fats:              resb 1                    ; Number of fats
  .rootDirEntries:    resw 1                    ; Number of entries in root dir
  .sectors:           resw 1                    ; Logical sectors
  .mediaType:         resb 1                    ; Media descriptor byte
  .fatSectors:        resw 1                    ; Sectors per FAT
  .sectorsPerTrack:   resw 1                    ; Sectors per track
  .heads:             resw 1                    ; Number of sides/heads
  .hiddenSectors:     resd 1                    ; Hidden sectors
  .hugeSectors:       resd 1                    ; LBA sectors
endstruc

struc ebpb                                      ; EBPB - Extended BIOS Parameter Block
  .driveNum:          resb 1                    ; Drive number
  .reserved:          resb 1                    ; This is not used
  .bootSignature:     resb 1                    ; Drive signature
  .volumeId:          resd 1                    ; Volume ID
  .volumeLabel:       resb 11                   ; Volume Label
  .fatTypeLabel:      resb 8                    ; File system type byte
  .bootCode:          resb 448                  ; Code used during the booting process
  .bootSig:           resw 1                    ; Bootable partition signature (0xaa55)
endstruc
   
struc dirFat
  .filename:          resb 8                    ; Filename 
  .extension:         resb 3                    ; Extension
  .attributes:        resb 1                    ; File attribtutes
  .reserved:          resb 1                    ; Reserved for Windows NT usage
  .idk:               resb 1                    ; 10-millisecond units past creation time below
  .creationTime:      resw 1                    ; Time of the file created
  .creationDate:      resw 1                    ; Date of the file created
  .accessedDate:      resw 1                    ; Date of last access to the file
  .clusterHi:         resw 1                    ; High starting cluster for file (alwase zero for fat16 and fat12)
  .modifiedTime:      resw 1                    ; Time of the file last modified
  .modifiedDate:      resw 1                    ; Date of the file last modified
  .clusterLo:         resw 1                    ; Low starting cluster
  .filesize:          resd 1                    ; Size of the file
endstruc

%endif
