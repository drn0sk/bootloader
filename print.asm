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

print_num:		; number to print in edx:eax
	push eax
	push ebx
	push ecx
	push edx
	push ebp
	push es
	xor cx,cx
	push `\r\n`
	add cx,2
.loop	movzx bp,al
	and bp,0x0F
	mov bh,[cs:.dgs+bp]
	movzx bp,al
	shr bp,4
	mov bl,[cs:.dgs+bp]
	push bx
	add cx,2
	shrd eax,edx,8
	shr edx,8
	call .zero
	jnz .loop
	push "0x"
	add cx,2
	mov ax,ss
	mov es,ax
	mov bp,sp
	movzx ecx,cx
	call print
	add sp,cx
.exit	pop es
	pop ebp
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret
.dgs	db	"0123456789ABCDEF"
.zero:			; checks if edx:eax is zero
	test eax,eax
	jnz .done
	test edx,edx
.done	ret

; PRINT
%endif
