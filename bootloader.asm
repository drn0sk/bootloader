BITS 16
	org 0x7C00
	segment .text
_start	jmp 0:.cs_0
.cs_0	mov bx,cs
	mov ds,bx
	mov es,bx
	mov fs,bx
	mov gs,bx
	mov ss,bx
	mov sp,0x7C00
	push eax
	push dx
	call installation_check
	jc exit_error
	cmp cx,1
	je fat16
	; other types

fat16:
	pop bx
	pop eax
	push eax
	push bx
	call fat16_init
	jc exit_error
	cmp BYTE [cs:FAT.initialized],0
	je exit_error
	pop bx
	push bx
	mov si,config_path
	mov cx,config_path_len
	mov di,end
	call fat16_find_dir_entry_absolute
	jc exit_error
	mov edx,[cs:end+28]
	mov [cs:config_len],edx
	pop bx
	push bx
	mov si,end
	mov di,end
	call fat16_load_file
	jc exit_error
	mov ecx,[cs:config_len]
	mov bp,end
	call print
	jmp exit

exit_error:
	mov bp,error_msg
	mov cx,error_msg_len
	call print
exit	int 0x18
	jmp 0xFFFF:0
.hlt	hlt
	jmp .hlt


config_len	dd	0

%include "print.asm"
%include "string.asm"
%defstr CONFIG_NAME CONFIG
string config_path,CONFIG_NAME
string error_msg,"ERROR",0xa,0xd
%include "disk_read.asm"
%include "fs.asm"
end:
