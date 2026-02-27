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
	dec ecx
	jnz .loop
.exit	pop ecx
	pop bx
	pop ax
	ret

print_newline:
	push bp
	push ecx
	push `\r\n`
	mov bp,sp
	mov ecx,2
	call print
	add sp,2
	pop ecx
	pop bp
	ret

; PRINT
%endif
