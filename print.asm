%ifndef PRINT
	%define PRINT

print:			; es:bp -> string
			; length of string in ecx
	push ax
	push bx
	push ecx
	test ecx,ecx
	jz .exit
	mov ah,0x0E
	xor bh,bh
.loop	mov al,[es:bp]
	int 0x10
	inc bp
	loop .loop,ecx
.exit	pop ecx
	pop bx
	pop ax
	ret

print_newline:
	mov bp,.newline
	mov cx,.newline_len
	jmp print
.newline	db	0xa,0xd
.newline_len	equ	$-.newline

; PRINT
%endif
