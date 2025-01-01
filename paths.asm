%ifndef PATHS
	%define PATHS
	segment .text
path_next_part:
	; ds:si -> path
	; cx is the length of the path
	; returns rest of path in ds:si
	; length of rest in cx
	; part of path in es:di
	; length of part in ax
	push ds
	pop es
	mov di,si
	mov ax,cx
	dec si
.loop	inc si
	cmp BYTE [ds:si],'/'
	loopne .loop
	inc si
	sub ax,cx
	test cx,cx
	jz .exit
	dec ax
.exit	ret
; PATHS
%endif
