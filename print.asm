%ifndef PRINT
	%define PRINT

print:			; es:bp -> string
			; length of string in cx
	test cx,cx
	jz .exit
	mov ah,0x0E
	xor bh,bh
.loop	mov al,[es:bp]
	int 0x10
	inc bp
	loop .loop
.exit	ret

print_newline:
	mov bp,newline
	mov cx,newline_len
	jmp print

newline		db	0xa,0xd
newline_len	equ	$-newline

; PRINT
%endif
