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
	xor bx,bx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
	;movzx ebx,bx
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
	xor bx,bx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
	;movzx ebx,bx
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
	xor bx,bx
	mov bx,ax	; move quotient to bx
	pop eax
	pop edx
	push edx
	push eax
	;movzx ebx,bx
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
	cmp ecx,54	; we can't shift by 54 or more
	stc
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

_ext2_find_path_absolute:
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
_ext2_find_path:
_ext2_find_path_relative:	; ds:si -> path
				; cx is the length of the path
				; eax is the inode of the directory to look in (2 to look in the root dir) (only needed if path is relative)
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
.start	mov di,cx
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
	jnc .noc
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.noc	call _load_dword_from_LBA
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
	jnc .nc
	sub si,[cs:bytes_per_sect]
	add ecx,1
	adc ebx,0
.nc	add ecx,eax
	adc ebx,0
	mov eax,ecx
	mov edx,ebx
	add si,4
	jnc .nc1
	sub si,[cs:bytes_per_sect]
	add eax,1
	adc edx,0	; inode entry = edx:eax (sectors) + si (bytes)
.nc1	call _load_dword_from_LBA
	jnc .got_sz
	mov bp,1
	jmp .exit
.got_sz	push eax
	mov cl,[cs:ext2.log_block_size]
	add cl,10
	xor eax,eax
	shrd eax,ebp,cl
	shr ebp,cl
	test eax,eax
	jnz .noceil
	inc ebp
.noceil	pop eax
	xor ebx,ebx
	mov ecx,ebp
	xchg di,si	; inode entry = edx:eax (sectors) + di (bytes)
	add di,36
	jnc .nc2
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc2	mov bp,12
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
	jc .nof
	pop ebp
	pop bp
	clc
	jmp .exit	; file found
.nof	test bp,bp
	pop eax
	pop bp
	mov bp,1
	stc
	jnz .exit	; some other error
	; file not found
	add di,4
	jnc .nc3
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc3	push di
	shr edi,16
	xchg cx,di
	shl edi,16
	pop di
	sub ecx,1
	sbb ebx,0
	js .lp_dn
	test ecx,ecx
	jnz .noz
	test ebx,ebx
	jz .lp_dn
.noz	dec bp
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
	pop di
	pop eax
	pop edx
	jc .err
	test bp,bp
	jz .nof2
	pop eax
	pop ds
	pop si
	pop cx
	clc
	jmp .exit	; file found
.err	pop ebp
	pop ds
	pop si
	pop cx
	mov bp,1
	jmp .exit
.nof2	add di,4
	jnc .nc4
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc4	mov si,di
	call _load_dword_from_LBA
	jnc .ld_ok
	add sp,10
	mov bp,1
	jmp .exit
.ld_ok	push sp
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
	pop di
	pop eax
	pop edx
	jc .err2
	test bp,bp
	jz .nof3
	add sp,8
	pop eax
	pop ds
	pop si
	pop cx
	clc
	jmp .exit	; file found
.err2	add sp,8
	pop ebp
	pop ds
	pop si
	pop cx
	mov bp,1
	jmp .exit
.nof3	add di,4
	jnc .nc5
	sub di,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
.nc5	mov si,di
	call _load_dword_from_LBA
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
.exit	ret

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
	push edx
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
	ja .sub
	cmp ecx,eax
	jna .start
.sub	sub ecx,eax
	sbb ebx,edx
.start	pop eax
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
	jc .err
	add eax,[cs:ext2.start_LBA]
	adc edx,0
	jc .err
	pop bp
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
	call far [cs:.fn_ptr]
	shl esi,16
	pop si
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
	jnc .noc
	sub bp,[cs:bytes_per_sect]
	add eax,1
	adc edx,0
	jmp .err
.noc	sub ecx,1
	sbb ebx,0
	call .is_zero
	jnz .loop
	xor bp,bp
	clc
	jmp .exit
.err	stc
.exit	pop edx
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
			; CF set and bp = 0 -> file found (inode in eax)
			; CF set and bp != 0 -> some error
			; CF unset -> file not found
	push ds
	push si
	push ecx
	push eax
	mov ds,[es:di+4]
	mov si,[es:di+6]
	mov cx,[es:di+8]
	call _ext2_find_inode_in_block
	mov [es:di],eax
	pop eax
	pop ecx
	pop si
	pop ds
	jc .nof
	xor bp,bp
	stc
	ret	; CF set and bp=0 -> file found
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
.start:	;
.exit:	;
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
.start:	;
.exit:	;
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
	jnc .nc
	sub si,[cs:bytes_per_sect]
	add ecx,1
	adc ebx,0
.nc	add ecx,eax
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
	and ebp,0x4000
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
