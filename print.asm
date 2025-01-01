%ifndef PRINT
	%define PRINT

print:			; es:bp -> string
			; length of string in cx
	mov ah,0x0E
	xor bh,bh
.loop	mov al,[es:bp]
	int 0x10
	inc bp
	dec cx
	jnz .loop
	ret

print_newline:
	mov bp,newline
	mov cx,newline_len
	call print
	ret

newline		db	0xa,0xd
newline_len	equ	$-newline

; PRINT
%endif
