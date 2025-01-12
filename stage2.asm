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
	mov si,0x01BE
	mov cx,4
find_part:
	test BYTE [cs:0x7c00+si],0x80
	jz .continue
	mov eax,[cs:0x7c00+si+0x08]
	xor edx,edx
	; FAT16 ?
	cmp BYTE [cs:0x7c00+si+4],0x04
	je fat16
	cmp BYTE [cs:0x7c00+si+4],0x06
	je fat16
	cmp BYTE [cs:0x7c00+si+4],0x0E
	je fat16
	
	; EXT2 ?
	cmp BYTE [cs:0x7c00+si+4],0x83
	je ext2
	
	;
.continue:
	add si,0x10
	dec cx
	jnz find_part

exit_error:
	mov bp,error_msg
	mov cx,error_msg_len
	call print
	int 0x18
.reboot	jmp 0xFFFF:0	; reboot
.hlt	hlt		; if that fails just halt
	jmp .hlt	; halt again if NMI

; LBA of partition to load in eax
fat16:
	pop bx
	push eax
	push bx
	call fat16_init
	jc exit_error
	cmp BYTE [FAT.initialized],0
	je exit_error
	pop bx
	push bx
	mov si,filepath
	mov cx,filepath_len
	mov di,end
	call fat16_find_dir_entry_absolute
	jnc .ok
	test bp,bp
	jnz .err
	mov bp,file_not_found_msg
	mov cx,file_not_found_msg_len
	call print
.err	jmp exit_error
.ok	mov eax,[cs:end+28]	; file size in bytes
	cmp eax,481*1024	; least amount of space at 0x7C00
	jbe .sz_ok		; could be more if EBDA is smaller
	mov bp,file_too_long_msg
	mov cx,file_too_long_msg_len
	call print
	jmp exit_error
.sz_ok	pop bx
	push bx
	mov si,end
	mov di,0x7C00
	call fat16_load_file
	jc exit_error
	pop dx
	pop eax
	jmp finally

; LBA of partition to load in eax
ext2:
	jmp exit_error	; not yet implemented

finally:	; drive number in dl
		; starting LBA of partition in eax
	jmp 0x00:0x7C00

%include "print.asm"
%include "string.asm"
%defstr BOOTLOADER_STRING BOOTLOADER
string file_too_long_msg,"The file '",BOOTLOADER_STRING,"' is too long.",0xa,0xd
string file_not_found_msg,"Could not find the file: '",BOOTLOADER_STRING,"'",0xa,0xd
string error_msg,"ERROR",0xa,0xd
string press_key,"[Press any key to restart]"
string filepath,BOOTLOADER_STRING
%include "disk_read.asm"
%include "fs.asm"
end:
