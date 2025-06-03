%ifndef EXT2_INCLUDED
	%define EXT2_INCLUDED
	segment .text

ext2:
.initialized		db	0
.drive_num		db	0
.start_LBA		dd	0
.block_size		dd	0
.sect_per_block		dd	0
.num_inodes		dd	0
.num_blocks		dd	0
.blocks_per_group	dd	0
.inodes_per_group	dd	0
.num_groups		dd	0
.bgdt			db	1
.inode_size		dw	0

load_byte_from_LBA:	; loads a byte at LBA (edx:eax) + byte offset (si) to al
			; carry flag set on error
	push es
	push di
	push cx
	push bx
	push edx
	push eax
	xor dx,dx
	mov ax,si
	div WORD [cs:bytes_per_sect]	; quotient in ax, remainder in dx
	mov cx,dx	; move remainder to cx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	add eax,bx
	adc edx,0
	push cx
	mov cx,1
	mov bl,[cs:ext2.drive_num]
	push [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call read
	pop bx
	jc .exit
	mov al,[es:di+bx]
	clc
.exit	pop bx
	pop cx
	pop di
	pop es
	ret

load_word_from_LBA:	; loads a word at LBA (edx:eax) + byte offset (si) to ax
			; sets carry flag on error
	push cx
	push edx
	push eax
	push si
	call load_byte_from_LBA
	pop si
	mov cl,al
	pop eax
	pop edx
	jc .exit
	add si,1
	jnc .nc
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc	call load_byte_from_LBA
	jc .exit
	mov ah,al
	mov al,cl
	clc
.exit	pop cx
	ret

load_dword_from_LBA:	; loads a dword at LBA (edx:eax) + byte offset (si) to eax
			; sets carry flag on error
	push ecx
	push edx
	push eax
	push si
	call load_byte_from_LBA
	pop si
	mov cl,al
	pop eax
	pop edx
	jc .exit
	add si,1
	jnc .nc0
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc0	push edx
	push eax
	push si
	call load_byte_from_LBA
	pop si
	mov ch,al
	pop eax
	pop edx
	jc .exit
	shl ecx,16
	add si,1
	jnc .nc1
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc1	push edx
	push eax
	push si
	call load_byte_from_LBA
	pop si
	mov cl,al
	pop eax
	pop edx
	jc .exit
	add si,1
	jnc .nc2
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc2	call load_byte_from_LBA
	jc .exit
	mov ah,al
	mov al,cl
	shl eax,16
	shr ecx,16
	mov ah,ch
	mov al,cl
	clc
.exit	pop ecx
	ret

; ext2_init
;LBA of partition in eax
;drive number in bl
;carry flag set on error
ext2_init:
	mov [ext2.drive_num],bl
	mov [ext2.start_LBA],eax
	push eax
	xor edx,edx
	mov si,1024+24
	call load_dword_from_LBA
	jc .exit
	mov [cs:ext2.block_size],eax
	test eax,eax
	jnz .l1
	inc BYTE [cs:ext2.bgdt]
.l1	movzx ecx,al
	cmp ecx,eax
	stc
	jne .exit
	mov eax,1024
	shl eax,cl
	mov [cs:ext2.sect_per_block],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024
	call load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_inodes],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+4
	call load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_blocks],eax
	mov ecx,eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+32
	call load_dword_from_LBA
	jc .exit
	xchg ecx,eax
	mov [cs:ext2.blocks_per_group],ecx
	xor edx,edx
	div ecx
	test edx,edx
	jz .nr2
	inc eax
.nr2	mov [cs:ext2.num_groups],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+40
	call load_dword_from_LBA
	jc .exit
	mov [cs:ext2.inodes_per_group],eax
	mov WORD [cs:ext2.inode_size],128
	pop eax
	push eax
	xor edx,edx
	mov si,1024+76
	call load_dword_from_LBA
	jc .exit
	cmp eax,1
	jb .v0
	pop eax
	push eax
	xor edx,edx
	mov si,1024+88
	call load_word_from_LBA
	jc .exit
	mov [cs.ext2.inode_size],ax
.v0	clc
.exit	pop eax
	ret


; ext2_load_file
;ds:si -> path
;cx is the length of the path
;es:di -> buffer big enough to hold file
;Carry Flag (CF) set on error

; ext2_load_file_relative
;ds:si -> path
;cx is the length of the path
;es:di -> any necessary info about parent directory
;fs:dx -> buffer big enough to hold file
;Carry Flag (CF) set on error

; ext2_get_file_size
;ds:si -> path
;cx is the length of the path
;returns file size in eax
;Carry Flag (CF) set on error

; EXT2_INCLUDED
%endif
