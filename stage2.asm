BITS 16
	org 0x1500
	segment .text
_start:
	mov ax,cs
	mov ds,ax
	mov es,ax
	mov fs,ax
	mov gs,ax
	mov ss,ax
	mov sp,0x1500
	push dx
	call print_newline
	pop dx
	push dx
	call installation_check
	jc exit_error
	pop bx
	push bx
	mov di,0x7C00
	call load_partition_table
	jc exit_error
	mov di,0x01BE
	mov cx,4
	pop dx
	sub sp,[cs:bytes_per_sect]
	mov si,sp
	push dx
	push ss
	pop ds
find_part:
	test BYTE [cs:0x7c00+di],0x80
	jz .continue
	mov eax,[cs:0x7c00+di+0x08]
	xor edx,edx
	; FAT16 ?
	cmp BYTE [cs:0x7c00+di+4],0x04
	je f16
	cmp BYTE [cs:0x7c00+di+4],0x06
	je f16
	cmp BYTE [cs:0x7c00+di+4],0x0E
	je f16
	
	; EXT2 ?
	cmp BYTE [cs:0x7c00+di+4],0x83
	je ext2
	
	;
.continue:
	add di,0x10
	dec cx
	jnz find_part

exit_error:
	mov bp,error_msg
	mov ecx,error_msg_len
	call print
	int 0x18
.reboot	jmp 0xFFFF:0	; reboot
.hlt	hlt		; if that fails just halt
	jmp .hlt	; halt again if NMI

%include "print.asm"
%include "string.asm"
%include "disk_read.asm"
%include "fs.asm"

f16:
	; ds:si has a buffer that is a sector long at the bottom of the stack (ss:0x1500-[bytes_per_sect])
	; ds is already set to ss
	set_fs FAT16
	jc exit_error
	push WORD FAT16
	jmp load

ext2:
	jmp exit_error	; not yet implemented

; LBA of partition to load in eax
load:
	pop cx
	pop bx
	push eax
	push cx
	push bx
	call init
	jc exit_error
	mov si,filepath
	mov cx,filepath_len
	call get_file_size	; returns file size in eax
	jnc .chk_sz
	test bp,bp
	jnz .exit_error
	mov bp,file_not_found_msg
	mov ecx,file_not_found_msg_len
	call print
	jmp exit_error
.chk_sz	cmp eax,481*1024	; least amount of space at 0x7C00
	jbe .sz_ok		; could be more if EBDA is smaller
	mov bp,file_too_long_msg
	mov ecx,file_too_long_msg_len
	call print
	jmp exit_error
.sz_ok	mov si,filepath
	mov cx,filepath_len
	mov di,0x7C00
	call load_file
	jc exit_error
	pop dx
	pop cx
	pop eax
	add sp,[cs:bytes_per_sect]
finally:	; drive number in dl
		; starting LBA of partition in eax
		; filesystem type in cx (FAT16 (1) for fat16, ...)
	jmp 0x00:0x7C00

%defstr BOOTLOADER_STRING BOOTLOADER
string file_too_long_msg,"The file '",BOOTLOADER_STRING,"' is too long.",0xa,0xd
string file_not_found_msg,"Could not find the file: '",BOOTLOADER_STRING,"'",0xa,0xd
string error_msg,"ERROR",0xa,0xd
string filepath,BOOTLOADER_STRING
end:
