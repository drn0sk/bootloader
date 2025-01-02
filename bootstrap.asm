BITS 16
	org 0x7C00
	segment .text
_start:
	jmp 0x00:.cs0
.cs0	mov ax,cs
	mov ds,ax
	mov es,ax
	mov ss,0x7000
	xor sp,sp
	push dx
	call installation_check
	jc exit_error
	pop bx
	push bx
	mov cx,51
	xor edx,edx
	mov eax,1
	mov es,0
	mov di,0x1500
	call read
	jc exit_error
	pop dx
	jmp 0x00:0x1500
exit_error:
	mov bp,error_msg
	mov cx,error_msg_len
	call print
	int 0x18
.reboot	jmp 0xFFFF:0	; reboot
.hlt	hlt		; if that fails just halt
	jmp .hlt	; halt again if NMI

%include "print.asm"
%include "string.asm"
%include "disk_read.asm"
; data
string error_msg,"ERROR",0xa,0xd
