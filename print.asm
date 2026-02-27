%ifndef PRINT
	%define PRINT

_print_small:		; es:bp -> string
			; length of string in cx
	push ax
	push bx
	push cx
	test cx,cx
	jz .exit
	mov ah,0x0E
	xor bh,bh
.loop	mov al,[es:bp]
	int 0x10
	inc bp
	loop .loop
.exit	pop cx
	pop bx
	pop ax
	ret

print:			; es:bp -> string
			; length of string in ecx
	push ecx
	push edx
	mov edx,ecx
.loop	cmp edx,0xFFFF
	jbe .rem
	mov cx,0xFFFF
	push edx
	call _print_small
	pop edx
	jmp .loop
.rem	mov cx,dx
	call _print_small
.exit	pop edx
	pop ecx
	ret

print_newline:
	mov bp,.newline
	mov cx,.newline_len
	jmp _print_small
.newline	db	0xa,0xd
.newline_len	equ	$-.newline

; PRINT
%endif
