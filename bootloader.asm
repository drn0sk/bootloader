BITS 16
	org 0x7C00
	segment .text

	jmp 0:_start

%include "print.asm"
%include "string.asm"
%include "disk_read.asm"
%include "filesystems.asm"
%include "fs.asm"

_start	mov bx,cs
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
	set_fs cx
	jc exit_error
	pop bx
	pop eax
	push eax
	push bx
	call init
	jc exit_error
	pop bx
	push bx
	mov si,config_path
	mov cx,config_path_len
	call get_file_size
	jc exit_error
	mov [cs:config_len],eax
	pop bx
	push bx
	mov si,config_path
	mov cx,config_path_len
	mov di,end
	call load_file
	jc exit_error
	mov ecx,[cs:config_len]
	mov bp,end
	call print
	jmp exit

exit_error:
	mov bp,error_msg
	mov cx,error_msg_len
	call print
exit:	int 0x18
	jmp 0xFFFF:0
.hlt	hlt
	jmp .hlt

config_len	dd	0

%defstr CONFIG_NAME CONFIG
string config_path,CONFIG_NAME
string error_msg,"ERROR",0xa,0xd
end:
