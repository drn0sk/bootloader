%ifndef EXT2_INCLUDED
	%define EXT2_INCLUDED
	segment .text

ext2:
.initialized		db	0
.drive_num		db	0
.start_LBA		dd	0
.bytes_per_block	dq	0
.sect_per_block		dd	0
.block_bytes_rem	dd	0
.num_inodes		dd	0
.num_blocks		dd	0
.blocks_per_group	dd	0
.inodes_per_group	dd	0
.num_groups		dd	0
.bgdt			db	1
.inode_size		dw	128
.inode_start		dd	11
.dir_type_feature	db	0

_load_byte_from_LBA:	; loads a byte at LBA (edx:eax) + byte offset (si) to al
			; carry flag set on error
	push es
	push di
	push cx
	push ebx
	push edx
	push eax
	xor dx,dx
	mov ax,si
	div WORD [cs:bytes_per_sect]	; quotient in ax, remainder in dx
	mov cx,dx	; move remainder to cx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	movzx ebx,bx
	add eax,ebx
	adc edx,0
	push cx
	mov cx,1
	mov bl,[cs:ext2.drive_num]
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call read
	pop bx
	jc .exit
	mov al,[es:di+bx]
	clc
.exit	pop ebx
	pop cx
	pop di
	pop es
	ret

_load_word_from_LBA:	; loads a word at LBA (edx:eax) + byte offset (si) to ax
			; sets carry flag on error
	push cx
	push edx
	push eax
	push si
	call _load_byte_from_LBA
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
.nc	call _load_byte_from_LBA
	jc .exit
	mov ah,al
	mov al,cl
	clc
.exit	pop cx
	ret

_load_dword_from_LBA:	; loads a dword at LBA (edx:eax) + byte offset (si) to eax
			; sets carry flag on error
	push ecx
	push edx
	push eax
	push si
	call _load_byte_from_LBA
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
	call _load_byte_from_LBA
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
	call _load_byte_from_LBA
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
.nc2	call _load_byte_from_LBA
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
	mov [cs:ext2.drive_num],bl
	mov [cs:ext2.start_LBA],eax
	push eax
	xor edx,edx
	mov si,1024+24
	call _load_dword_from_LBA
	jc .exit
	test eax,eax
	jnz .l1
	inc BYTE [cs:ext2.bgdt]
.l1	mov ecx,eax
	cmp ecx,32	; we can't shift by 32 or more
	stc
	jnb .exit
	xor edx,edx
	mov eax,1024
	shld edx,eax,cl
	shl eax,cl
	mov [cs:ext2.bytes_per_block],eax
	mov [cs:ext2.bytes_per_block+4],edx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov [cs:ext2.sect_per_block],eax
	mov [cs:ext2.block_bytes_rem],edx
	pop eax
	push eax
	xor edx,edx
	mov si,1024
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_inodes],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+4
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_blocks],eax
	mov ecx,eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+32
	call _load_dword_from_LBA
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
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.inodes_per_group],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+76
	call _load_dword_from_LBA
	jc .exit
	cmp eax,1
	jb .v0
	pop eax
	push eax
	xor edx,edx
	mov si,1024+88
	call _load_word_from_LBA
	jc .exit
	mov [cs:ext2.inode_size],ax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+84
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.inode_start],eax
	pop eax
	push eax
	xor edx,edx
	mov si,1024+96
	call _load_dword_from_LBA
	jc .exit
	test eax,2
	jz .no_typ
	mov BYTE [cs:ext2.dir_type_feature],1
.no_typ	and eax,~2
	stc
	jnz .exit
.v0	clc
	mov BYTE [cs:ext2.initialized],0x01
.exit	pop eax
	ret

%include "paths.asm"

_ext2_find_path:
	mov eax,2
_ext2_find_path_relative:	; ds:si -> path
				; cx is the length of the path
				; eax is the inode of the directory to look in (2 to look in the root dir)
				; returns inode in eax
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	;
	test cx,cx
	mov bp,1
	stc
	jz .exit
	cmp BYTE [ds:si],'/'
	jne .loop
	add si,1
	push ds
	mov bp,sp
	adc WORD [ss:bp],0
	pop ds
	dec cx
.loop	push eax
	call path_next_part
	test ax,ax
	mov bp,1
	stc
	jz .exit
	;;;
.exit	;
	ret

_ext2_find_inode:	; ds:si -> name
			; cx has the name length
			; eax is the parent inode (2 to look in the root directory)
			; returns inode associated with name in eax
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	;
.exit	;
	ret

_ext2_find_inode_in_block:
			; block address in eax
			; ds:si -> name
			; cx has the name length
			; inode returned in eax
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push ecx
	mov bp,cx
	shl ebp,16
	push eax
	mov ecx,[cs:ext2.sect_per_block]
	mul ecx
	pop ecx
	push edx
	push eax
	mov eax,[cs:ext2.block_bytes_rem]
	mul ecx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	cmp edx,0xFFFF	; remainder should be less than bytes_per_sect
			; which is a WORD
	jna .l0
	; this should never happen
	stc
	mov bp,2
	pop eax
	pop edx
	jmp .exit
	;pop ecx
.l0	mov bp,dx	; remainder <= a WORD
	mov ecx,eax
	pop eax
	pop edx
	add eax,ecx
	adc edx,0
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	mov ecx,[cs:ext2.sect_per_block]
.s_loop	push ecx
	mov cx,1
	mov bl,[cs:ext2.drive_num]
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call read
	jnc .rd_ok
	mov bp,1
	pop ecx
	jmp .exit
.rd_ok	cmp WORD [cs:.partial_len],0
	je .dir_lp
.chk_pt	push es
	push di
	push ds
	push si
	push cx
	push bx
	push es
	pop ds
	push cs
	pop es
	mov si,di
	add si,bp
	mov bx,[cs:.partial_len]
	lea di,[cs:.partial_entry+bx]
	cmp bx,8
	mov cx,8
	jb .rd_pt
	xor cx,cx
	cmp BYTE [cs:ext2.dir_type_feature],0
	jne .tp0
	mov cx,[cs:.partial_entry+6]
	jmp .rd_pt
.tp0	mov cl,[cs:.partial_entry+6]
.rd_pt	test ch,ch ; filenames longer than 255 bytes not supported
	jz .len_ok
	mov bp,3
	pop ecx
	jmp .exit
.len_ok	sub cx,bx
	pop bx
	cld
	push dx
	mov dx,[cs:bytes_per_sect]
	sub dx,bp
	cmp cx,dx
	pop dx
	ja .part
	cmp cx,8
	ja .full
	add [cs:.partial_len],cx
	rep movsb
	pop cx
	pop si
	pop ds
	pop di
	pop es
	jmp .chk_pt
.part	mov cx,[cs:bytes_per_sect]
	sub cx,bp
	add [cs:.partial_len],cx
	rep movsb
	pop cx
	pop si
	pop ds
	pop di
	pop es
	xor bp,bp
	jmp .next_s
.full	add bp,cx
	add [cs:.partial_len],cx
	rep movsb
	pop cx
	mov di,si
	pop si
	pop ds
	push cs
	pop es
	mov ecx,ebp
	shr ecx,16
	push bp
	xor bp,bp
	call .check_entry
	pop di
	jnc .done
	mov bp,di
	pop di
	pop es
	mov WORD [cs:.partial_len],0
.dir_lp	mov cx,[cs:bytes_per_sect]
	sub cx,bp
	cmp cx,8
	jb .mk_pt
	push dx
	xor dx,dx
	cmp BYTE [cs:ext2.dir_type_feature],0
	jne .tp
	mov dx,WORD [es:di+bp+6]
	jmp .cm
.tp	mov dl,BYTE [es:di+bp+6]
.cm	cmp cx,dx
	pop dx
	jb .mk_pt
	cmp DWORD [es:di+bp],0
	je .nxt_dr
	mov ecx,ebp
	shr ecx,16
	call .check_entry
	;shl edi,16
	;mov di,bp
	jnc .done
	;mov bp,di
	;shr edi,16
.nxt_dr	mov cx,bp
	add cx,[es:di+4]
	jc .nxt_sb
	cmp cx,[cs:bytes_per_sect]
	jae .nxt_sb
	mov bp,cx
	jmp .dir_lp
.mk_pt	test cx,cx
	jnz .ne
	xor bp,bp
	jmp .next_s
.ne	test ch,ch ; filenames longer than 255 bytes not supported
	jz .ln_ok
	mov bp,3
	pop ecx
	jmp .exit
.ln_ok	push es
	push di
	push ds
	push si
	push cx
	push es
	pop ds
	mov si,di
	add si,bp
	push cs
	pop es
	lea di,[cs:.partial_entry]
	mov [cs:.partial_len],cx
	rep movsb
	pop cx
	pop si
	pop ds
	pop di
	pop es
	xor bp,bp
	jmp .next_s
.nxt_sb	push edx
	push eax
	xor dx,dx
	mov ax,[es:di+4]
	div [cs:bytes_per_sect]
	add bp,dx
	movzx ecx,ax
	pop eax
	pop edx
	dec ecx
	add eax,ecx
	adc edx,0
.next_s	add eax,1
	adc edx,0
	pop ecx
	dec ecx
	jnz .s_loop
	stc
	jmp .exit
.done	pop ecx
	clc
.exit	pop ecx
	ret
.partial_entry	times 263 db 0
.partial_len	dw	0
.check_entry:	; ds:si -> name
		; cx = length of name
		; es:di+bp -> directory entry
		; returns inode in eax
		; Carry Flag (CF) is set if there is no match
		; CF clear if there is a match
	push ds
	push si
	push cx
	push es
	push di
	push dx
	add di,bp
	xor dx,dx
	cmp BYTE [cs:ext2.dir_type_feature],0
	jne .type
	mov dx,[es:di+6]
	jmp .cmp
.type	mov dl,[es:di+6]
.cmp	cmp cx,dx
	stc
	jne .ret	; different length names, so no match
	cld
	push es
	push di
	add di,8
	repe cmpsb
	pop di
	pop es
	stc
	jnz .ret	; no match
	mov eax,[es:di]
	clc
.ret	pop dx
	pop di
	pop es
	pop cx
	pop si
	pop ds
	ret

ext2_load_file:
ext2_load_file_absolute:	; ds:si -> path
				; cx is the length of the path
				; es:di -> buffer big enough to hold file
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	;
.exit	;
	ret

ext2_load_file_relative:	; ds:si -> path
				; cx is the length of the path
				; es:di -> any necessary info about parent directory
				; fs:dx -> buffer big enough to hold file
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	;
.exit	;
	ret

ext2_get_file_size:		; ds:si -> path
				; cx is the length of the path
				; returns file size in eax
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	;
.exit	;
	ret

; EXT2_INCLUDED
%endif
