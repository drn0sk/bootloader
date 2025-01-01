BITS 16
	org 0x0500
	segment .text
_start:
	xor ax,ax
	mov ds,ax
	mov ss,ax
	mov sp,0x7C00
	mov es,ax
	mov si,0x7C00
	mov di,0x0500
	mov cx,0x200
	cld
	rep movsb
	jmp 0x00:relocated
relocated:
	mov ax,cs
	mov ds,ax
	mov es,ax
	mov ss,ax
	mov sp,0x7C00
	push dx
	mov ah,0x41
	mov bx,0x55AA
	int 0x13
	jc CHS
	cmp bx,0xAA55
	jne CHS
	and cx,0x01
	jz CHS
LBA:
%ifdef DEBUG
	mov bp,LBA_msg
	mov cx,LBA_msg_len
	call print
%endif
	mov DWORD [ds:DAP.lower_LBA],1
	mov WORD [ds:DAP.num_sectors],51
	mov WORD [ds:DAP.segment],0x00
	mov WORD [ds:DAP.offset],0x7C00
	mov si,DAP
	pop dx
	push dx
	mov ah,0x42
	int 0x13
	jc exit_error
next:
	pop dx
	jmp 0x00:0x7C00
CHS:
%ifdef DEBUG
	mov bp,CHS_msg
	mov cx,CHS_msg_len
	call print
%endif
exit_error:
	mov bp,error_msg
	mov cx,error_msg_len
	call print
	int 0x18
	mov bp,press_key
	mov cx,press_key_len
	call print
	xor ah,ah
	int 0x16	; wait for any keypress
.reboot	jmp 0xFFFF:0	; reboot
.hlt	hlt		; if that fails just halt
	jmp .hlt	; halt again if NMI


%include "print.asm"

; data
error_msg:	db	"ERROR",0xa,0xd
error_msg_len:	equ	$-error_msg
%ifdef DEBUG
LBA_msg:	db	"LBA",0xa,0xd
LBA_msg_len:	equ	$-LBA_msg
CHS_msg:	db	"CHS",0xa,0xd
CHS_msg_len:	equ	$-CHS_msg
%endif
press_key	db	"[Press any key to restart]"
press_key_len	equ	$-press_key
DAP:
.size		db	16
		db	0
.num_sectors	dw	0
.offset		dw	0
.segment	dw	0
.lower_LBA	dd	0
.upper_LBA	dd	0
