BITS 16
	org 0x7C00
	segment .text
_start:
	; make sure cs is zero
	jmp 0x00:cs_zero
%include "print.asm"
%include "string.asm"
%include "disk_read.asm"
cs_zero:		; cs = 0
	mov ax,cs
	mov ds,ax
	mov es,ax
	mov ax,0x7000
	mov ss,ax
	xor sp,sp	; stack points to the end of conventional memory
	push dx
	call installation_check
	jc exit_error
	pop bx
	push bx
	xor dx,dx
	mov ax,0x7C00-0x1500 ; this is the max amount of bytes we can load
	div WORD [cs:bytes_per_sect]
	mov cx,ax	; we want to load ax (the quotient) sectors
	xor edx,edx
	mov eax,1	; at LBA 1
	mov di,0x1500	; to 0:0x1500 (es was set to cs which is 0)
	call read
	jc exit_error
	pop dx		; drive number in dl
	jmp 0x00:0x1500
exit_error:
	mov bp,error_msg
	mov ecx,error_msg_len
	call print
	int 0x18
.reboot	jmp 0xFFFF:0	; reboot
.hlt	hlt		; if that fails just halt
	jmp .hlt	; halt again if NMI

; data
string error_msg,"ERROR",0xa,0xd
