%ifndef EXT2_INCLUDED
	%define EXT2_INCLUDED
	segment .text

_ext2_misc_buff times 1024 db 0

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

; ext2_init
;LBA of partition in eax
;drive number in bl
;carry flag set on error
ext2_init:
	mov [ext2.drive_num],bl
	mov [ext2.start_LBA],eax
	push eax
	xor dx,dx
	mov eax,1024
	div WORD [cs:bytes_per_sect]
	mov cx,ax
	test dx,dx
	stc
	jnz .exit
	pop edx
	add eax,edx
	mov edx,0
	adc edx,0
	push cs
	pop es
	mov di,_ext2_misc_buff
	call read
	jc .exit
	mov eax,[cs:_ext2_misc_buff+24]
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
	mov eax,[cs:_ext2_misc_buff]
	mov [cs:ext2.num_inodes],eax
	mov eax,[cs:_ext2_misc_buff+4]
	mov [cs:ext2.num_blocks],eax
	mov ecx,[cs:_ext2_misc_buff+32]
	mov [cs:ext2.blocks_per_group],ecx
	xor edx,edx
	div ecx
	test edx,edx
	jz .nr2
	inc eax
.nr2	mov [cs:ext2.num_groups],eax
	mov eax,[cs:_ext2_misc_buff+40]
	mov [cs:ext2.inodes_per_group],eax
	mov WORD [cs:ext2.inode_size],128
	cmp DWORD [cs:_ext2_misc_buff+76],1
	jb .v0
	mov ax,[cs:_ext2_misc_buff+88]
	mov [cs.ext2.inode_size],ax
.v0	clc
.exit	ret


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
