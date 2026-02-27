%ifndef EXT2_INCLUDED
	%define EXT2_INCLUDED
	segment .text

_load_byte_from_LBA:	; loads a byte at LBA (edx:eax) + byte offset (si)
			; to lower byte of bp
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
	xor ebx,ebx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
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
	mov ax,bp
	mov al,[es:di+bx]
	mov bp,ax
	clc
.exit	pop eax
	pop edx
	pop ebx
	pop cx
	pop di
	pop es
	ret

_load_word_from_LBA:	; loads a word at LBA (edx:eax) + byte offset (si) to bp
			; sets carry flag on error
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
	xor ebx,ebx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
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
	mov bp,[es:di+bx]
	clc
.exit	pop eax
	pop edx
	pop ebx
	pop cx
	pop di
	pop es
	ret

_load_dword_from_LBA:	; loads a dword at LBA (edx:eax) + byte offset (si) to ebp
			; sets carry flag on error
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
	xor ebx,ebx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
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
	mov ebp,[es:di+bx]
	clc
.exit	pop eax
	pop edx
	pop ebx
	pop cx
	pop di
	pop es
	ret

ext2:
.initialized		db	0
.drive_num		db	0
.start_LBA		dd	0
.log_block_size		db	0
.bytes_per_block	dq	0
.sect_per_block		dd	0
.block_bytes_rem	dw	0
.num_inodes		dd	0
.num_blocks		dd	0
.blocks_per_group	dd	0
.inodes_per_group	dd	0
.num_groups		dd	0
.bgdt			db	1
.inode_size		dw	128
.inode_start		dd	11
.dir_type_feature	db	0
.size64			db	0

; ext2_init
;LBA of partition in eax
;drive number in bl
;carry flag set on error
ext2_init:
	push si
	push ebp
	push ebx
	push ecx
	push edx
	mov [cs:ext2.drive_num],bl
	mov [cs:ext2.start_LBA],eax
	push eax
	xor edx,edx
	mov si,1024+24
	call _load_dword_from_LBA
	jc .exit
	test ebp,ebp
	jnz .l1
	inc BYTE [cs:ext2.bgdt]
.l1	mov ecx,ebp
	stc
	cmp ecx,54	; we can't shift by 54 or more
	jnb .exit
	mov [cs:ext2.log_block_size],cl
	mov eax,1024
	shld edx,eax,cl
	shl eax,cl
	mov [cs:ext2.bytes_per_block],eax
	mov [cs:ext2.bytes_per_block+4],edx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx		; dividing by sector_size which is a word
	mov [cs:ext2.sect_per_block],eax
	mov [cs:ext2.block_bytes_rem],dx ; so the remainder is also a word
	pop eax
	push eax
	xor edx,edx
	mov si,1024
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_inodes],ebp
	mov si,1024+4
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.num_blocks],ebp
	mov ecx,ebp
	mov si,1024+32
	call _load_dword_from_LBA
	jc .exit
	mov eax,ecx
	mov ecx,ebp
	mov [cs:ext2.blocks_per_group],ecx
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
	mov [cs:ext2.inodes_per_group],ebp
	mov si,1024+76
	call _load_dword_from_LBA
	jc .exit
	cmp ebp,1
	jb .v0
	mov si,1024+88
	call _load_word_from_LBA
	jc .exit
	mov [cs:ext2.inode_size],bp
	mov si,1024+84
	call _load_dword_from_LBA
	jc .exit
	mov [cs:ext2.inode_start],ebp
	mov si,1024+96
	call _load_dword_from_LBA
	jc .exit
	test ebp,2
	jz .no_typ
	mov BYTE [cs:ext2.dir_type_feature],1
.no_typ	and ebp,~2
	stc
	jnz .exit
	mov si,1024+100
	call _load_dword_from_LBA
	jc .exit
	test ebp,2
	jz .v0
	mov BYTE [cs:ext2.size64],1
.v0	clc
	mov BYTE [cs:ext2.initialized],0x01
.exit	pop eax
	pop edx
	pop ecx
	pop ebx
	pop ebp
	pop si
	ret

%include "paths.asm"

_ext2_find_path:
_ext2_find_path_absolute:	; ds:si -> path
				; cx is the length of the path
				; returns inode in eax
				; Carry Flag (CF) set on error
				;  if bp is zero, the file was not found
				;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push ds
	push si
	push cx
	push es
	push di
	push ebx
	test cx,cx
	mov bp,1
	stc
	jz _ext2_find_path_relative.exit
	cmp BYTE [ds:si],'/'
	je .ok
	mov bp,1
	stc
	jmp _ext2_find_path_relative.exit ; absolute path must start with a '/'
.ok	add si,1
	dec cx
	mov eax,2
	jmp _ext2_find_path_relative.loop
_ext2_find_path_relative:	; ds:si -> path
				; cx is the length of the path
				; eax is the inode of the directory to look in (2 to look in the root dir)
				; returns inode in eax
				; Carry Flag (CF) set on error
				;  if bp is zero, the file was not found
				;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push ds
	push si
	push cx
	push es
	push di
	push ebx
	test cx,cx
	mov bp,1
	stc
	jz .exit
	cmp BYTE [ds:si],'/'
	je _ext2_find_path_absolute.ok
.loop	push eax
	call path_next_part
	test ax,ax
	pop ebx
	mov bp,1
	stc
	jz .exit
	push ds
	push si
	push cx
	mov cx,ax
	mov ax,es
	mov ds,ax
	mov si,di
	mov eax,ebx
	call _ext2_find_inode
	pop cx
	pop si
	pop ds
	jc .exit
	test cx,cx
	jnz .loop
	clc
.exit	pop ebx
	pop di
	pop es
	pop cx
	pop si
	pop ds
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
.start	push ds
	push si
	push ecx
	push es
	push edi
	push edx
	push ebx
	mov di,cx
	shl edi,16
	mov di,si
	xor edx,edx
	dec eax
	div DWORD [cs:ext2.inodes_per_group]
	push edx ; save remainder for later
	mov edx,32
	mul edx
	add eax,8
	adc edx,0
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov ebx,eax
	mov cx,dx	; divided by a WORD so the remainder is also a WORD
	mov ax,[cs:ext2.block_bytes_rem]
	movzx bp,BYTE [cs:ext2.bgdt]
	mul bp
	mov bp,WORD [cs:bytes_per_sect]
	div bp
	push dx
	push ax
	mov eax,[cs:ext2.sect_per_block]
	movzx ebp,BYTE [cs:ext2.bgdt]
	mul ebp
	xor ebp,ebp
	pop bp
	add eax,ebp
	adc edx,0
	pop si
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	add eax,ebx
	adc edx,0
	add si,cx
	jnc .nc0
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc0	call _load_dword_from_LBA
	jnc .rd_bgt
	pop edx
	mov bp,1
	jmp .exit
.rd_bgt	push ebp
	mov eax,ebp
	movzx ecx,WORD [cs:ext2.block_bytes_rem]
	mul ecx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov si,dx
	mov ebx,eax
	pop eax
	mov ecx,[cs:ext2.sect_per_block]
	mul ecx
	add eax,ebx
	adc edx,0
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	mov ecx,eax
	mov ebx,edx	; inode table start = ebx:ecx (sectors) + si (bytes)
	pop eax
	movzx ebp,WORD [cs:ext2.inode_size]
	mul ebp
	movzx ebp,WORD [cs:bytes_per_sect]
	div ebp
	add si,dx
	jnc .nc1
	sub si,[cs:bytes_per_sect]
	add ecx,1
	adc ebx,0
.nc1	add ecx,eax
	adc ebx,0
	mov eax,ecx
	mov edx,ebx
	call _load_word_from_LBA
	jnc .type
	mov bp,1
	jmp .exit
.type	test bp,0x4000	; error if parent directory is not an actual directory
	stc
	mov bp,0	; treated as not found
	jz .exit
	add si,4
	jnc .nc2
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0	; inode entry = edx:eax (sectors) + si (bytes)
.nc2	call _load_dword_from_LBA
	jnc .got_sz
	mov bp,1
	jmp .exit
.got_sz	push eax
	mov cl,[cs:ext2.log_block_size]
	add cl,10
	xor eax,eax
	shrd eax,ebp,cl
	shr ebp,cl
	xor ebx,ebx
	test eax,eax
	jz .noceil
	add ebp,1
	adc ebx,0
.noceil	pop eax
	mov ecx,ebp
	xchg di,si	; inode entry = edx:eax (sectors) + di (bytes)
	add di,36
	jnc .nc3
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc3	mov bp,12
.loop	xchg si,di
	push bp
	call _load_dword_from_LBA
	xchg si,di
	jnc .cont
	pop bp
	mov bp,1
	jmp .exit
.cont	push eax
	mov eax,ebp
	push di
	shr edi,16
	xchg cx,di
	shl edi,16
	pop di
	call _ext2_find_inode_in_block
	jc .nof0
	pop ebp
	pop bp
	clc
	jmp .exit	; file found
.nof0	test bp,bp
	pop eax
	pop bp
	mov bp,1
	stc
	jnz .exit	; some other error
	; file not found
	add di,4
	jnc .nc4
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc4	push di
	shr edi,16
	xchg cx,di
	shl edi,16
	pop di
	sub ecx,1
	sbb ebx,0
	jns .ns
	stc
	xor bp,bp
	jmp .exit
.ns	test ecx,ecx
	jnz .nz0
	test ebx,ebx
	jnz .nz0
	stc
	xor bp,bp
	jmp .exit
.nz0	dec bp
	jnz .loop
	; edx:eax (s) + di (b) -> singly indirect pointer
.lp_dn	push di
	shr edi,16
	xchg cx,di
	shl edi,16
	pop di
	push cx
	push si
	push ds
	sub esp,4
	mov si,sp
	mov cx,di
	shr edi,16
	xchg cx,di
	push edx
	push eax
	push di
	xchg si,di
	call _load_dword_from_LBA
	jnc .gotp
	add sp,20
	mov bp,1
	jmp .exit
.gotp	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_find_inode_in_block_wrapper
	push ss
	pop es
	; eax = block pointer
	; ebx:ecx = size in blocks
	call _ext2_foreach_block	; singly indirect
	pop si
	pop eax
	pop edx
	jc .err0
	test bp,bp
	jz .nof1
	pop eax
	pop ds
	pop si
	pop cx
	clc
	jmp .exit	; file found
.err0	pop ebp
	pop ds
	pop si
	pop cx
	mov bp,1
	jmp .exit
.nof1	test ecx,ecx
	jnz .nz1
	test ebx,ebx
	jnz .nz1
	stc
	xor bp,bp
	jmp .exit
.nz1	add si,4
	jnc .nc5
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc5	call _load_dword_from_LBA
	jnc .ld_ok0
	add sp,10
	mov bp,1
	jmp .exit
.ld_ok0	push sp
	push ss
	push _ext2_find_inode_in_block_wrapper
	push cs
	mov di,sp
	push ss
	pop es
	push edx
	push eax
	push si
	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_foreach_block_wrapper
	call _ext2_foreach_block	; doubly indirect
	pop si
	pop eax
	pop edx
	jc .err1
	test bp,bp
	jz .nof2
	add sp,8
	pop eax
	pop ds
	pop si
	pop cx
	clc
	jmp .exit	; file found
.err1	add sp,8
	pop ebp
	pop ds
	pop si
	pop cx
	mov bp,1
	jmp .exit
.nof2	test ecx,ecx
	jnz .nz2
	test ebx,ebx
	jnz .nz2
	stc
	xor bp,bp
	jmp .exit
.nz2	add si,4
	jnc .nc6
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc6	call _load_dword_from_LBA
	jnc .ld_ok1
	add sp,18
	mov bp,1
	jmp .exit
.ld_ok1	push sp
	push ss
	push _ext2_find_inode_in_block_wrapper
	push cs
	mov di,sp
	push ss
	pop es
	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_foreach_block_wrapper
	call _ext2_foreach_block	; trebly indirect
	pushf
	pop edx
	add sp,16
	pop eax
	add sp,6
	push edx
	popf
	mov dx,bp
	mov bp,1
	jc .exit
	xor bp,bp
	stc
	test dx,dx
	jz .exit
	clc
.exit	pop ebx
	pop edx
	pop edi
	pop es
	pop ecx
	pop si
	pop ds
	ret

_ext2_foreach_block_wrapper:
			; wrapper to use _ext2_foreach_block in _ext2_foreach_block
			; es:di ->	args:
			;		ds (function segment)
			;		si (function offset)
			;		es (function args segment)
			;		di (function args offset)
			; CF set and bp == 0 -> exit loop on success
			; CF set and bp != 0 -> exit loop on error
			; CF unset -> continue
	push ds
	push si
	push es
	push di
	mov ds,[es:di+8]
	mov si,[es:di+10]
	mov es,[es:di+12]
	mov di,[es:di+14]
	call _ext2_foreach_block
	pop di
	pop es
	pop si
	pop ds
	mov bp,1
	jc .exit
	test bp,bp
	jz .exit
	xor bp,bp
	stc
.exit	retf
_ext2_foreach_block:	;  indirect block pointer in eax
			;  maximum number of blocks to read in ebx:ecx
			;  (does not read more than the number of blocks in a block)
			;  returns ebx:ecx - <# of blocks read> in ebx:ecx
			;  ds:si -> function to call for each block
			;   Called with block pointer in eax
			;   and number of blocks read in ebx:ecx
			;   for each block in the indirect block
			;   es:di -> any other args to provide to the function
			;   If this function sets carry (CF) the loop is terminated:
			;    successfully if bp = 0
			;    on error if bp != 0
			;    continues the loop if carry flag (CF) is unset
			;  CF is set on error
			;  CF clear on success
			;    with bp nonzero if we exited early
			;         bp zero if we reached the end of the loop
	push esi
	push eax
	push edx
	push ebx
	push ecx
	call .is_zero
	stc
	jz .exit
	push eax
	push ecx
	mov cl,[cs:ext2.log_block_size]
	add cl,8
	xor edx,edx
	mov eax,1
	shld edx,eax,cl
	shl eax,cl
	pop ecx
	cmp ebx,edx
	jb .start
	ja .gt
	cmp ecx,eax
	jna .start
.gt	mov ecx,eax
	mov ebx,edx
.start	pop eax
	sub [ss:esp],ecx
	sbb [ss:esp+4],ebx
	push eax
	movzx edx,WORD [cs:ext2.block_bytes_rem]
	mul edx
	movzx ebp,WORD [cs:bytes_per_sect]
	div ebp
	mov ebp,eax
	pop eax
	push dx
	mov edx,[cs:ext2.sect_per_block]
	mul edx
	add eax,ebp
	adc edx,0
	pop bp
	jc .err
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	jc .err
.loop	shl esi,16
	mov si,bp
	call _load_dword_from_LBA
	jc .err
	push eax
	mov eax,ebp
	mov bp,si
	shr esi,16
	push bp
	test eax,eax
	jnz .b_ok
	shl esi,16
	pop si
	pop eax
	jmp .cont ; skip any block pointers that are zero
.b_ok	mov [cs:.fn_ptr],si
	mov [cs:.fn_ptr+2],ds
	shl esi,16
	pop si
	call far [cs:.fn_ptr]
	pop eax
	jnc .cont
	test bp,bp
	jnz .err
	mov bp,1
	clc
	jmp .exit
.cont	mov bp,si
	shr esi,16
	add bp,4
	jnc .nc
	sub bp,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
	jc .err
.nc	sub ecx,1
	sbb ebx,0
	call .is_zero
	jnz .loop
	xor bp,bp
	clc
	jmp .exit
.err	stc
.exit	pop eax
	pop edx
	add ecx,eax
	adc ebx,edx
	pop edx
	pop eax
	pop esi
	ret
.is_zero:		; sets ZF if ebx:ecx is zero, clears ZF otherwise
	test ecx,ecx
	jnz .nonz
	test ebx,ebx
.nonz	ret
.fn_ptr	dd	0

_ext2_find_inode_in_block_wrapper:
			; wrapper to use _ext2_find_inode_in_block in
			; _ext2_foreach_block
			; es:di -> args:
			;          eax (inode returned),
			;          ds (name segment),
			;          si (name offset),
			;          cx (name len)
			; CF set and bp == 0 -> file found
			; CF set and bp != 0 -> some error
			; CF unset -> file not found
	push ds
	push si
	push cx
	push eax
	mov ds,[es:di+4]
	mov si,[es:di+6]
	mov cx,[es:di+8]
	call _ext2_find_inode_in_block
	mov [es:di],eax
	pop eax
	pop cx
	pop si
	pop ds
	jc .nof
	xor bp,bp
	stc
	retf	; CF set and bp=0 -> file found
.nof	test bp,bp
	jnz .err
	clc
.err	retf
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
.start	push ebx
	push es
	push di
	push edx
	push ecx
	mov bp,cx
	shl ebp,16
	push eax
	mov ecx,[cs:ext2.sect_per_block]
	mul ecx
	pop ecx
	push edx
	push eax
	movzx eax,WORD [cs:ext2.block_bytes_rem]
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
	pop bx
	pop cx
	pop si
	pop ds
	pop di
	pop es
	pop ecx
	jmp .exit
.len_ok	add cx,8
	sub cx,bx
	cld
	push dx
	mov dx,[cs:bytes_per_sect]
	sub dx,bp
	cmp cx,dx
	pop dx
	jna .npart
	pop bx
	jmp .part
.npart	cmp bx,8
	pop bx
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
.full	rep movsb
	pop cx
	mov di,.partial_entry
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
	jc .cnt0
	pop di
	pop es
	jmp .done
.cnt0	mov bp,di
	pop di
	pop es
	mov cx,[cs:.partial_entry+4]
	sub cx,[cs:.partial_len]
	mov WORD [cs:.partial_len],0
	add cx,bp
	jc .nxt_sb
	cmp cx,[cs:bytes_per_sect]
	jae .nxt_sb
	mov bp,cx
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
.cm	add dx,8
	cmp cx,dx
	pop dx
	jb .mk_pt
	cmp DWORD [es:di+bp],0
	je .nxt_dr
	mov ecx,ebp
	shr ecx,16
	call .check_entry
	jnc .done
.nxt_dr	mov cx,bp
	add cx,[es:di+bp+4]
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
	mov ax,[es:di+bp+4]
	div [cs:bytes_per_sect]
	movzx ecx,ax
	add bp,dx
	jnc .nc
	inc ecx
	sub bp,[cs:bytes_per_sect]
	jmp .cnt
.nc	cmp bp,[cs:bytes_per_sect]
	jb .cnt
	inc ecx
	sub bp,[cs:bytes_per_sect]
.cnt	pop eax
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
	pop edx
	pop di
	pop es
	pop ebx
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

; loads ebx:ecx bytes of file with inode eax into es:di
; sets CF on error
_ext2_load_inode:	; inode of file to load in eax
			; es:di -> buffer to load file to
			; size of buffer in ebx:ecx
			; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	stc
	ret
.start	push eax
	push es
	push di
	push ds
	push si
	push ebx
	push ecx
	push edx
	push ebp
	push ebx
	push ecx
	xor edx,edx
	dec eax
	div DWORD [cs:ext2.inodes_per_group]
	push edx ; save remainder for later
	mov edx,32
	mul edx
	add eax,8
	adc edx,0
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov ebx,eax
	mov cx,dx	; divided by a WORD so the remainder is also a WORD
	mov ax,[cs:ext2.block_bytes_rem]
	movzx bp,BYTE [cs:ext2.bgdt]
	mul bp
	mov bp,WORD [cs:bytes_per_sect]
	div bp
	push dx
	push ax
	mov eax,[cs:ext2.sect_per_block]
	movzx ebp,BYTE [cs:ext2.bgdt]
	mul ebp
	xor ebp,ebp
	pop bp
	add eax,ebp
	adc edx,0
	pop si
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	add eax,ebx
	adc edx,0
	add si,cx
	jnc .nc0
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc0	call _load_dword_from_LBA
	jnc .rd_bgt
	pop edx
	pop ecx
	pop ebx
	jmp .exit
.rd_bgt	push ebp
	mov eax,ebp
	movzx ecx,WORD [cs:ext2.block_bytes_rem]
	mul ecx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov si,dx
	mov ebx,eax
	pop eax
	mov ecx,[cs:ext2.sect_per_block]
	mul ecx
	add eax,ebx
	adc edx,0
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	mov ecx,eax
	mov ebx,edx	; inode table start = ebx:ecx (sectors) + si (bytes)
	pop eax
	movzx ebp,WORD [cs:ext2.inode_size]
	mul ebp
	movzx ebp,WORD [cs:bytes_per_sect]
	div ebp
	add si,dx
	jnc .nc1
	sub si,[cs:bytes_per_sect]
	add ecx,1
	adc ebx,0
.nc1	add ecx,eax
	adc ebx,0
	mov eax,ecx
	mov edx,ebx
	add si,4
	jnc .nc2
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0	; inode entry = edx:eax (sectors) + si (bytes)
.nc2	call _load_dword_from_LBA
	jnc .got_sz
	pop ecx
	pop ebx
	jmp .exit
.got_sz	cmp BYTE [cs:ext2.size64],0
	jnz .fs64
	xor ebx,ebx
	mov ecx,ebp
.fs64	mov ecx,ebp
	sub si,4
	jnc .nc3
	add si,[cs:bytes_per_sect]
	sub eax,1
	sbb edx,0
.nc3	call _load_word_from_LBA
	jnc .got_tp
	pop ecx
	pop ebx
	jmp .exit
.got_tp	test bp,0x4000
	jz .file
	xor ebx,ebx
.file	add si,108
	jnc .nc4
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc4	call _load_dword_from_LBA
	jnc .sz64
	pop ecx
	pop ebx
	jmp .exit
.sz64	mov ebx,ebp
	cmp ebx,[ss:esp+4]
	jne .h
	cmp ecx,[ss:esp]
.h	jbe .load
	mov ebx,[ss:esp+4]
	mov ecx,[ss:esp]
.load	add sp,8
	sub si,68	; inode entry = edx:eax (sectors) + si (bytes)
	jnc .nc5
	add si,[cs:bytes_per_sect]
	sub eax,1
	sbb edx,0
.nc5	mov bp,12
.loop	push eax
	push edx
	mov eax,ecx
	mov edx,ebx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov ecx,eax
	mov bx,dx
	pop edx
	pop eax
	cmp ecx,0xFFFF
	jbe .sectok
	stc
	jmp .exit
.sectok	push bp
	call _load_dword_from_LBA
	jnc .cont
	pop bp
	jmp .exit
.cont	push eax
	mov eax,ebp
	call _ext2_load_block_of_file
	jnc .rdok
	pop ebp
	pop bp
	jmp .exit	; error
.rdok	test ax,ax
	pop eax
	pop bp
	jnz .exit	; file loaded
	; did not finish loading file
	add si,4
	jnc .nc6
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc6	push eax
	push dx
	xor dx,dx
	mov ax,cx
	mul WORD [cs:bytes_per_sect]
	mov cx,dx
	shl ecx,16
	mov cx,ax
	movzx eax,bx
	xor ebx,ebx
	add ecx,eax
	adc ebx,0
	pop dx
	pop eax
	test ecx,ecx
	jnz .nz0
	test ebx,ebx
	clc
	jz .exit
.nz0	dec bp
	jnz .loop
	; edx:eax (s) + di (b) -> singly indirect pointer
.lp_dn	push di
	push es
	push eax
	push edx
	mov edx,ebx
	mov eax,ecx
	movzx ebp,WORD [cs:bytes_per_sect]
	div ebp
	mov di,dx
	mov ebp,eax
	pop edx
	pop eax
	cmp ebp,0xFFFF
	jbe .qok
	pop es
	pop di
	stc
	jmp .exit
.qok	push di
	push bp
	sub sp,2
	mov di,sp
	push edx
	push eax
	mov ebp,ecx
	mov cl,[cs:ext2.log_block_size]
	add cl,10
	xor eax,eax
	shrd eax,ebp,cl
	shrd ebp,ebx,cl
	shr ebx,cl
	test eax,eax
	jz .noceil
	add ebp,1
	adc ebx,0
.noceil	mov ecx,ebp
	pop eax
	pop edx
	call _load_dword_from_LBA
	jnc .gotp
	add sp,10
	jmp .exit
.gotp	push edx
	push eax
	push si
	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_load_block_of_file_wrapper
	push ss
	pop es
	; eax = block pointer
	; ebx:ecx = size in blocks
	call _ext2_foreach_block	; singly indirect
	pop si
	pop eax
	pop edx
	jc .err0
	cmp WORD [ss:esp],0
	je .more0
	pop ax
	pop cx
	pop bx
	pop es
	pop di
	clc
	jmp .exit	; file loaded
.err0	pop ax
	pop cx
	pop bx
	pop es
	pop di
	jmp .exit
.more0	test ecx,ecx
	jnz .nz1
	test ebx,ebx
	jnz .nz1
	add sp,10
	clc
	jmp .exit
.nz1	add si,4
	jnc .nc7
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc7	call _load_dword_from_LBA
	jnc .ld_ok
	add sp,10
	jmp .exit
.ld_ok	push sp
	push ss
	push _ext2_load_block_of_file_wrapper
	push cs
	mov di,sp
	push ss
	pop es
	push edx
	push eax
	push si
	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_foreach_block_wrapper
	call _ext2_foreach_block	; doubly indirect
	pop si
	pop eax
	pop edx
	jc .err1
	cmp WORD [ss:esp+8],0
	je .more1
	add sp,8
	pop ax
	pop cx
	pop bx
	pop es
	pop di
	clc
	jmp .exit	; file loaded
.err1	add sp,8
	pop ax
	pop cx
	pop bx
	pop es
	pop di
	jmp .exit
.more1	test ecx,ecx
	jnz .nz2
	test ebx,ebx
	jnz .nz2
	add sp,18
	clc
	jmp .exit
.nz2	add si,4
	jnc .nc8
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc8	call _load_dword_from_LBA
	jnc .ld_ok1
	add sp,18
	jmp .exit
.ld_ok1	push sp
	push ss
	push _ext2_load_block_of_file_wrapper
	push cs
	mov di,sp
	push ss
	pop es
	mov eax,ebp
	push cs
	pop ds
	mov si,_ext2_foreach_block_wrapper
	call _ext2_foreach_block	; trebly indirect
	pushf
	pop edx
	add sp,16
	pop ax
	pop cx
	pop bx
	pop es
	pop di
	push edx
	popf
	jc .exit
	stc
	test ax,ax
	jz .exit
	clc
.exit	pop ebp
	pop edx
	pop ecx
	pop ebx
	pop si
	pop ds
	pop di
	pop es
	pop eax
	ret

_ext2_load_block_of_file_wrapper:
			; wrapper to use _ext2_load_block_of_file in
			; _ext2_foreach_block
			; block pointer in eax
			; es:di -> args:
			;          ax (1 if done reading else 0) (output),
			;          cx (number of sectors) (input & output),
			;          bx (number of bytes) (input & output),
			;          es (destination segment) (input & output),
			;          di (destination offset) (input & output)
			; CF set and bp == 0 -> file loaded
			; CF set and bp != 0 -> some error
			; CF unset -> not done loading file
	push ds
	push si
	push eax
	push ebx
	push ecx
	push es
	push di
	mov cx,[es:di+2]
	mov bx,[es:di+4]
	push dx
	mov dx,[es:di+6]
	mov di,[es:di+8]
	mov es,dx
	pop dx
	call _ext2_load_block_of_file
	pop si
	pop ds
	mov [ds:si],ax
	mov [ds:si+2],cx
	mov [ds:si+4],bx
	mov bx,es
	mov [ds:si+6],bx
	mov [ds:si+8],di
	push ds
	pop es
	mov di,si
	mov bp,1
	jc .exit
	test ax,ax
	jz .exit
	xor bp,bp
	stc
.exit	pop ecx
	pop ebx
	pop eax
	pop si
	pop ds
	retf
_ext2_load_block_of_file:	; block pointer in eax
				; filesize: cx (sectors) + bx (bytes)
				; es:di -> destination
				; returns size left to load in cx,bx
				; returns ax == 1 if done reading file
				; returns ax == 0 otherwise
				; es:di points to end of buffer on return
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	stc
	ret
.start	push ds
	push si
	push edx
	push ebp
	test cx,cx
	jnz .siz_ok
	test bx,bx
	jnz .siz_ok
	clc
	mov ax,1
	jmp .exit
.siz_ok	push DWORD [cs:ext2.sect_per_block]
	push WORD [cs:ext2.block_bytes_rem]
	push ecx
	push ebx
	push es
	push di
	push eax
	movzx ecx,WORD [cs:ext2.block_bytes_rem]
	mul ecx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov ecx,eax
	mov bx,dx	; divided by a WORD so remainder fits in a WORD
	pop eax
	mul DWORD [cs:ext2.sect_per_block]
	add eax,ecx
	adc edx,0
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	test bx,bx
	jz .nox
	push bx
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	mov bl,[cs:ext2.drive_num]
	mov cx,1
	call read
	pop bx
	jnc .rd_ok
	pop di
	pop es
	pop ebx
	pop ecx
	add sp,6
	jmp .exit
.rd_ok	add di,bx
	mov cx,[cs:bytes_per_sect]
	sub cx,bx
	push es
	pop ds
	mov si,di
	pop di
	pop es
	pop ebp
	pop ebx
	push ebx
	push ebp
	test bx,bx
	jnz .bigger
	cmp bp,cx
	jae .bigger
	mov cx,bp
.bigger	cmp DWORD [ss:esp+10],0
	jne .big
	cmp [ss:esp+8],cx
	jae .big
	mov cx,[ss:esp+8]
.big	push cx
	push es
	push di
	cld
	rep movsb
	pop di
	pop es
	pop cx
	movzx ecx,cx
	call add_to_seg_off
	jnc .add_ok
	pop ebx
	pop ecx
	add sp,6
	jmp .exit
.add_ok	mov bp,cx
	pop ebx
	pop ecx
	sub bx,bp
	jg .more
	dec cx
	jns .bor
	clc
	mov ax,1
	add sp,6
	jmp .exit
.bor	add bx,[cs:bytes_per_sect]
.more	add eax,1
	adc edx,0
	sub [ss:esp+8],bp
	jg .noborr
	dec DWORD [ss:esp+10]
	jns .borr
	clc
	pop eax
	pop eax
	xor ax,ax
	add sp,6
	jmp .exit
.borr	push ax
	mov ax,[cs:bytes_per_sect]
	add [ss:esp+8],ax
	pop ax
.noborr	push ecx
	push ebx
	push es
	push di
.nox	pop di
	pop es
	pop ebx
	pop ecx
	test cx,cx
	jz .rem
	push ecx
	push ebx
	movzx ecx,cx
	cmp ecx,[ss:esp+10]
	jbe .less
	; cx > [ss:esp+10]
	; so [ss:esp+10] fits in a WORD
	mov cx,[ss:esp+10]
.less	mov bl,[cs:ext2.drive_num]
	call read
	jnc .rd_ok2
	pop ebx
	pop ecx
	add sp,6
	jmp .exit
.rd_ok2	push eax
	push edx
	push cx
	mov ax,[cs:bytes_per_sect]
	mul cx
	mov cx,dx
	shl ecx,16
	mov cx,ax
	call add_to_seg_off
	pop cx
	pop edx
	pop eax
	jnc .ok2
	pop ebx
	pop ecx
	add sp,6
	jmp .exit
.ok2	movzx ecx,cx
	add eax,ecx
	adc edx,0
	movzx ebp,cx
	pop ebx
	pop ecx
	sub cx,bp
	jnz .ck_bk
	test bx,bx
	jnz .ck_bk
	mov ax,1
	add sp,6
	clc
	jmp .exit
.ck_bk	sub [ss:esp+2],ebp
	jnz .rem
	cmp WORD [ss:esp],0
	jne .rem
	xor ax,ax
	add sp,6
	clc
	jmp .exit
.rem	push ecx
	push ebx
	movzx ecx,cx
	cmp ecx,[ss:esp+10]
	jne .neq
	cmp bx,[ss:esp+8]
.neq	ja .blkend
	test cx,cx
	jz .rd_rm
	pop ebx
	pop ecx
	add sp,6
	stc
	jmp .exit	; this shouldn't happen
.blkend	cmp DWORD [ss:esp+10],0
	jne .sectok
	pop ebx
	pop ecx
	add sp,6
	stc
	jmp .exit	; this shouldn't happen
.sectok	mov bx,[ss:esp+8]
.rd_rm	push es
	push di
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	mov bp,bx
	mov cx,1
	mov bl,[cs:ext2.drive_num]
	call read
	push es
	pop ds
	mov si,di
	pop di
	pop es
	pop ebx
	pop ecx
	pop dx
	pop eax
	jc .exit
	push es
	push di
	push cx
	mov cx,bp
	cld
	rep movsb
	pop cx
	pop di
	pop es
	push ecx
	movzx ecx,bp
	call add_to_seg_off
	pop ecx
	jc .exit
	sub bx,bp
	jns .nos
	add bx,[cs:bytes_per_sect]
	stc
	dec cx
	js .exit
.nos	sub dx,bp
	jns .nos2
	add dx,[cs:bytes_per_sect]
	stc
	dec eax
	js .exit
.nos2	test cx,cx
	jnz .notdon
	test bx,bx
	jnz .notdon
	clc
	mov ax,1
	jmp .exit
.notdon	test eax,eax
	stc
	jnz .exit
	test dx,dx
	stc
	jnz .exit
	xor ax,ax
	clc
.exit	pop ebp
	pop edx
	pop si
	pop ds
	ret

ext2_load_file:
ext2_load_file_absolute:	; ds:si -> path
				; cx is the length of the path
				; es:di -> buffer big enough to hold file
				; Carry Flag (CF) set on error
				;  if bp is zero, the file was not found
				;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start:	push edx
	push ecx
	push ebx
	push eax
	call _ext2_find_path_absolute
	jc .exit
	push eax
	call _ext2_get_inode_size
	mov bp,1
	mov ebx,edx
	mov ecx,eax
	pop eax
	jc .exit
	call _ext2_load_inode
	mov bp,1
.exit:	pop eax
	pop ebx
	pop ecx
	pop edx
	ret

ext2_load_file_relative:	; ds:si -> path
				; cx is the length of the path
				; eax is the inode of the directory to look in (2 to look in the root dir) (only needed if the path is relative)
				; es:di -> buffer big enough to hold file
				; Carry Flag (CF) set on error
				;  if bp is zero, the file was not found
				;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start:	push edx
	push ecx
	push ebx
	push eax
	call _ext2_find_path_relative
	jc .exit
	push eax
	call _ext2_get_inode_size
	mov bp,1
	mov ebx,edx
	mov ecx,eax
	pop eax
	jc .exit
	call _ext2_load_inode
	mov bp,1
.exit:	pop eax
	pop ebx
	pop ecx
	pop edx
	ret

_ext2_get_inode_size:		; eax is the inode of the file to get the size of
				; returns the filesize in edx:eax
				; Carry Flag (CF) set on error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	stc
	ret
.start	push ebx
	push ecx
	push ebp
	push si
	xor edx,edx
	dec eax
	div DWORD [cs:ext2.inodes_per_group]
	push edx ; save remainder for later
	mov edx,32
	mul edx
	add eax,8
	adc edx,0
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov ebx,eax
	mov cx,dx	; divided by a WORD so the remainder is also a WORD
	mov ax,[cs:ext2.block_bytes_rem]
	movzx bp,BYTE [cs:ext2.bgdt]
	mul bp
	mov bp,WORD [cs:bytes_per_sect]
	div bp
	push dx
	push ax
	mov eax,[cs:ext2.sect_per_block]
	movzx ebp,BYTE [cs:ext2.bgdt]
	mul ebp
	xor ebp,ebp
	pop bp
	add eax,ebp
	adc edx,0
	pop si
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	add eax,ebx
	adc edx,0
	add si,cx
	jnc .noc
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.noc	call _load_dword_from_LBA
	jnc .rd_bgt
	pop edx
	jmp .exit
.rd_bgt	push ebp
	mov eax,ebp
	movzx ecx,WORD [cs:ext2.block_bytes_rem]
	mul ecx
	movzx ecx,WORD [cs:bytes_per_sect]
	div ecx
	mov si,dx
	mov ebx,eax
	pop eax
	mov ecx,[cs:ext2.sect_per_block]
	mul ecx
	add eax,ebx
	adc edx,0
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	mov ecx,eax
	mov ebx,edx	; inode table start = ebx:ecx (sectors) + si (bytes)
	pop eax
	movzx ebp,WORD [cs:ext2.inode_size]
	mul ebp
	movzx ebp,WORD [cs:bytes_per_sect]
	div ebp
	add si,dx
	jnc .nc0
	sub si,[cs:bytes_per_sect]
	add ecx,1
	adc ebx,0
.nc0	add ecx,eax
	adc ebx,0
	mov eax,ecx
	mov edx,ebx
	add si,4
	jnc .nc1
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0	; inode entry = edx:eax (sectors) + si (bytes)
.nc1	call _load_dword_from_LBA
	jc .exit
	cmp BYTE [cs:ext2.size64],0
	jnz .fs64
	xor edx,edx
	mov eax,ebp
	clc
	jmp .exit
.fs64	mov ecx,ebp
	sub si,4
	jnc .nc2
	add si,[cs:bytes_per_sect]
	sub eax,1
	sbb edx,0
.nc2	call _load_word_from_LBA
	jc .exit
	test bp,0x4000
	jz .file
	xor edx,edx
	mov eax,ecx
	clc
	jmp .exit
.file	add si,108
	jnc .nc3
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc3	call _load_dword_from_LBA
	jc .exit
	mov eax,ecx
	mov edx,ebp
	clc
.exit	pop si
	pop ebp
	pop ecx
	pop ebx
	ret

ext2_get_file_size:		; ds:si -> path
				; cx is the length of the path
				; returns file size in edx:eax
				; Carry Flag (CF) set on error
				;  if bp is zero, the file was not found
				;  if bp is nonzero, there was some other error
	cmp BYTE [cs:ext2.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start:	call _ext2_find_path_absolute
	jc .exit
	call _ext2_get_inode_size
	mov bp,1
.exit:	ret

; EXT2_INCLUDED
%endif
