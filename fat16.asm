%ifndef FAT16_INCLUDED
	%define FAT16_INCLUDED
	segment .text
fat16:
.initialized		db	0
.drive_num		db	0
.file_size		dd	0
.first_data_sect	dd	0
.data_sect_high		dd	0
.sect_per_clust		dw	0
.first_fat_sect		dd	0
.fat_sect_high		dd	0
.fat_size_sects		dw	0
.fat_size_bytes		dd	0
.bytes_per_clust	dd	0
.fat_off		dw	0
.current_clust		dw	0
.buffer_off		dw	0
.buffer_seg		dw	0
.first_root_sect	dd	0
.root_sect_high		dd	0
.root_size		dw	0

struc fat_BS
.bootjmp	resb 3
.oem_name	resq 1
.bytes_per_sect	resw 1
.sect_per_clust	resb 1
.reserved_sects	resw 1
.table_count	resb 1
.root_entries	resw 1
.total_sects_16	resw 1
.media_type	resb 1
.table_size	resw 1
.sect_per_track	resw 1
.head_side_cnt	resw 1
.hidden_sectors	resd 1
.total_sects_32	resd 1
.bios_drive_num	resb 1
.reserved1	resb 1
.boot_sig	resb 1
.volume_id	resd 1
.volume_label	resb 11
.fat_type_label	resq 1
.boot_code	resb 448
.part_sig	resb 2
endstruc



fat16_init:	; LBA of partition in eax
		; drive number in bl
		; carry flag set on error
		; on success fat16.initialized is set to non-zero value
	push es
	push bx
	mov [cs:fat16.drive_num],bl
	push eax
	mov cx,1
	xor edx,edx
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call read
	jc .exit
	xor eax,eax
	push WORD [cs:_buff.seg]
	pop es
	mov bx,[cs:_buff.off]
	xor ah,ah
	mov al,[es:bx+fat_BS.sect_per_clust]
	xor edx,edx
	mul WORD [es:bx+fat_BS.bytes_per_sect]
	mov WORD [cs:fat16.bytes_per_clust],ax
	mov WORD [cs:fat16.bytes_per_clust+2],dx
	div WORD [cs:bytes_per_sect]
	mov [cs:fat16.sect_per_clust],ax
	xor ax,ax
	mov cx,1
	test dx,dx
	cmovnz ax,cx
	add [cs:fat16.sect_per_clust],ax
	mov dx,[cs:fat16.sect_per_clust]
	mov ax,0xFFFF
	cmovc dx,ax
	mov [cs:fat16.sect_per_clust],dx
	xor eax,eax
	mov ax,[es:bx+fat_BS.reserved_sects]
	xor edx,edx
	mul WORD [es:bx+fat_BS.bytes_per_sect]
	div WORD [cs:bytes_per_sect]
	mov DWORD [cs:fat16.first_fat_sect],eax
	mov WORD [cs:fat16.fat_off],dx
	mov ax,[es:bx+fat_BS.table_size]
	xor edx,edx
	mul WORD [es:bx+fat_BS.bytes_per_sect]
	mov WORD [cs:fat16.fat_size_bytes],ax
	mov WORD [cs:fat16.fat_size_bytes+2],dx
	div WORD [cs:bytes_per_sect]
	mov WORD [cs:fat16.fat_size_sects],ax
	test dx,dx
	jz .done_rounding
	add WORD [cs:fat16.fat_size_sects],1
	jnc .done_rounding
	mov WORD [cs:fat16.fat_size_sects],0xFFFF
.done_rounding:
	xor ecx,ecx
	mov cx,[es:bx+fat_BS.root_entries]
	shl ecx,5
	movzx edx,WORD [cs:bytes_per_sect]
	dec edx
	add edx,ecx
	mov ax,dx
	shr edx,16
	div WORD [cs:bytes_per_sect]
	mov cx,ax
	mov WORD [cs:fat16.root_size],cx
	xor edx,edx
	mov ax,[es:bx+fat_BS.table_count]
	mul WORD [cs:fat16.fat_size_sects]
	shl edx,16
	mov dx,ax
	pop eax
	push eax
	add eax,edx
	mov edx,0
	adc edx,0
	mov edi,[cs:fat16.first_fat_sect]
	add eax,edi
	adc edx,0
	mov DWORD [cs:fat16.first_root_sect],eax
	mov DWORD [cs:fat16.root_sect_high],edx
	mov DWORD [cs:fat16.first_data_sect],eax
	mov DWORD [cs:fat16.data_sect_high],edx
	movzx edi,cx
	add DWORD [cs:fat16.first_data_sect],edi
	adc DWORD [cs:fat16.data_sect_high],0
	pop ebx
	push ebx
	mov edi,ebx
	add [cs:fat16.first_fat_sect],ebx
	adc DWORD [cs:fat16.fat_sect_high],0
	clc
	mov BYTE [cs:fat16.initialized],0x01
.exit:
	pop eax
	pop bx
	pop es
	ret

%include "paths.asm"

fat16_load_file:
fat16_load_file_absolute:	; ds:si -> path
				; cx is the length of the path
				; es:di -> buffer big enough to hold file
				; Carry Flag (CF) set on error
	push es
	push di
	call _fat16_find_dir_entry_absolute
	jnc .load
	ret
.load	push es
	pop ds
	mov si,di
	pop di
	pop es
	jmp _fat16_load_file_from_dir_entry

fat16_load_file_relative:	; ds:si -> path
				; cx is the length of the path
				; es:di -> directory entry of parent
				; fs:dx -> buffer big enough to hold file
				; Carry Flag (CF) set on error
	call _fat16_find_dir_entry_relative
	jnc .load
	ret
.load	push es
	pop ds
	mov si,di
	push fs
	pop es
	mov di,dx
	jmp _fat16_load_file_from_dir_entry


fat16_get_file_size:
			; ds:si -> path
			; cx is the length of the path
			; returns file size in eax
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	push es
	push di
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call _fat16_find_dir_entry_absolute
	jc .exit
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	mov eax,[es:di+28]
	clc
.exit	pop di
	pop es
	ret

_fat16_get_info:
_fat16_get_info_absolute:
_fat16_find_dir_entry_absolute:
			; ds:si -> path
			; cx is the length of the path
			; es:di -> 32 byte buffer to return the
			;  directory entry
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push ax
	push edx
	cmp BYTE [ds:si],'/'
	jne .no_s
	inc si
	dec cx
.no_s	push es
	push di
	call path_next_part
	test ax,ax
	mov bp,1
	stc
	jz .exit
	xchg ax,cx
	mov dx,ds
	shl edx,16
	mov dx,si
	push es
	pop ds
	mov si,di
	pop di
	pop es
	call _fat16_find_dir_entry_in_root_dir
	jc .exit
	mov cx,ax
	mov si,dx
	shr edx,16
	mov ds,dx
	clc
	test cx,cx
	jz .exit
	pop edx
	pop ax
	jmp _fat16_find_dir_entry_relative
.exit	pop edx
	pop ax
	ret

_fat16_get_info_relative:
_fat16_find_dir_entry_relative:
			; ds:si -> path
			; cx is the length of the path
			; es:di -> 32 byte buffer
			;   on input: contains the directory entry of the
			;  parent directory
			;   on output: returns the directory entry
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push ax
	push edx
	test cx,cx
	mov bp,1
	stc
	jz .exit
	cmp BYTE [ds:si],'/'
	je _fat16_find_dir_entry_absolute
.loop	push es
	push di
	call path_next_part
	test ax,ax
	mov bp,1
	stc
	jz .exit
	xchg ax,cx
	mov dx,ds
	shl edx,16
	mov dx,si
	push es
	pop ds
	mov si,di
	pop di
	pop es
	call _fat16_find_dir_entry_in_subdir
	jc .exit
	mov cx,ax
	mov si,dx
	shr edx,16
	mov ds,dx
	test cx,cx
	jnz .loop
	clc
.exit	pop edx
	pop ax
	ret

; gets the directory entry for a file or directory in the root directory
_fat16_find_dir_entry_in_root_dir:	
			; ds:si -> file or directory name
			; cx has the length of the name
			; es:di -> 32 byte buffer to return the
			;  directory entry
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push eax
	push edx
	push cx
	mov eax,[cs:fat16.first_root_sect]
	mov edx,[cs:fat16.root_sect_high]
	mov cx,[cs:fat16.root_size]
.loop	pop bp
	push bp
	push cx
	mov cx,bp
	call _fat16_find_dir_entry_in_sector
	pop cx
	jnc .exit	; file found
	test bp,bp
	stc
	jnz .exit	; some error
	add eax,1
	adc edx,0
	loop .loop
	xor bp,bp
	stc
.exit	pop cx
	pop edx
	pop eax
	ret

struc _fat16_find_params
.filename_seg	resw	1
.filename_off	resw	1
.filename_len	resw	1
.dir_entry_seg	resw	1
.dir_entry_off	resw	1
.found		resb	1
endstruc

_fat16_find_dir_entry_in_subdir:
			; ds:si -> file or directory name
			; cx has the length of the name
			; es:di -> 32 byte buffer
			;   on input: contains the directory entry of the
			;  parent directory
			;   on output: returns the directory entry
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push cx
	push fs
	push si
	push es
	push di
	test BYTE [es:di+11],0x10	; error if parent directory is not an actual directory
	stc
	xor bp,bp			; treated as not found
	jz .exit
	mov WORD [cs:.params+_fat16_find_params.filename_len],cx
	mov cx,[es:di+26]	; starting cluster
	mov WORD [cs:.params+_fat16_find_params.filename_seg],ds
	mov WORD [cs:.params+_fat16_find_params.filename_off],si
	mov WORD [cs:.params+_fat16_find_params.dir_entry_seg],es
	mov WORD [cs:.params+_fat16_find_params.dir_entry_off],di
	mov BYTE [cs:.params+_fat16_find_params.found],0
	push cs
	pop fs
	mov si,_fat16_find_dir_entry_in_clust
	push cs
	pop es
	mov di,.params
	call _fat16_for_each_clust_in_chain
	mov bp,1
	jc .exit	; error
	cmp BYTE [cs:.params+_fat16_find_params.found],0
	clc
	jne .exit	; file found
	xor bp,bp
	stc
.exit	pop di
	pop es
	pop si
	pop fs
	pop cx
	ret
.params	times	_fat16_find_params_size db 0

_fat16_find_dir_entry_in_clust:	
			; cluster number in cx
			; es:di -> _fat16_find_params
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov ax,1
	stc
	retf
.start	push bp
	push cx
	push ds
	push si
	push es
	push di
	push WORD [es:di+_fat16_find_params.filename_len]
	mov BYTE [es:di+_fat16_find_params.found],0
	mov ax,[es:di+_fat16_find_params.filename_seg]
	mov ds,ax
	mov si,[es:di+_fat16_find_params.filename_off]
	mov ax,[es:di+_fat16_find_params.dir_entry_seg]
	mov di,[es:di+_fat16_find_params.dir_entry_off]
	mov es,ax
	mov ax,[cs:fat16.sect_per_clust]
	sub cx,2
	mul cx
	shl edx,16
	mov dx,ax
	mov eax,edx
	xor edx,edx
	add eax,[cs:fat16.first_data_sect]
	adc edx,[cs:fat16.data_sect_high]
	mov cx,[cs:fat16.sect_per_clust]
.loop	pop bp
	push bp
	push cx
	mov cx,bp
	call _fat16_find_dir_entry_in_sector
	pop cx
	jc .err
	pop ax
	pop di
	pop es
	inc BYTE [es:di+_fat16_find_params.found]
	push es
	push di
	push ax
	xor ax,ax
	stc
	jmp .exit	; file found
.err	test bp,bp
	jz .cont
	mov ax,1
	stc
	jmp .exit	; some error
.cont	add eax,1
	adc edx,0
	loop .loop
	; not found
	clc
.exit	pop bp
	pop di
	pop es
	pop si
	pop ds
	pop cx
	pop bp
	retf

_fat16_find_dir_entry_in_sector:
			; LBA in edx:eax
			; ds:si -> file or directory name
			; cx has the length of the name
			; es:di -> 32 byte buffer to return the
			;  directory entry
			; Carry Flag (CF) set on error
			;  if bp is zero, the file was not found
			;  if bp is nonzero, there was some other error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov bp,1
	stc
	ret
.start	push fs
	push cx
	push es
	push di
	mov bl,[cs:fat16.drive_num]
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	mov cx,1
	call read
	pop di
	pop es
	jnc .rd_ok
	mov bp,1
	jmp .exit
.rd_ok	mov BYTE [cs:.long],0
	mov WORD [cs:.len],0
	mov WORD [cs:.off],.lfn+255
	push WORD [cs:_buff.seg]
	pop fs
	xor bp,bp	
.loop	mov bx,[cs:_buff.off]
	add bx,bp
	cmp BYTE [fs:bx],0
	jne .next
	xor bp,bp
	stc
	jmp .exit	; not found
.next	mov bx,[cs:_buff.off]
	add bx,bp
	cmp BYTE [fs:bx],0xE5
	jne .used
	mov BYTE [cs:.long],0
	mov WORD [cs:.len],0
	mov WORD [cs:.off],.lfn+255
	jmp .cont	; skip unused entries
.used	mov bx,[cs:_buff.off]
	add bx,bp
	cmp BYTE [fs:bx+11],0x0F
	jne .normal
	push es
	push di
	push cs
	pop es
	mov di,[cs:.off]
	xor cl,cl
	push si
	mov si,[cs:_buff.off]
.chr_lp	movzx bx,cl
	mov bl,[cs:.lfn_chrs+bx]
	add bx,bp
	cmp WORD [fs:si+bx],0xFFFF
	je .nxtchr
	cmp BYTE [fs:si+bx],0x00
	je .nxtchr
	dec di
	mov bl,[fs:si+bx]
	mov BYTE [es:di],bl
.nxtchr	inc cl
	cmp cl,13
	jb .chr_lp
	pop si
	mov BYTE [cs:.long],1
	mov [cs:.off],di
	lea di,[cs:.lfn+255]
	sub di,[cs:.off]
	mov [cs:.len],di
	pop di
	pop es
	jmp .cont
.normal:		; if long use lfn
	pop cx		; filename length
	push cx
	cmp BYTE [cs:.long],0
	je .short
	mov BYTE [cs:.long],0
	cmp cx,[cs:.len]
	cmova cx,[cs:.len]
	jmp .cmp
.short	cmp cx,12
	jbe .sfn
	mov WORD [cs:.len],0
	mov WORD [cs:.off],.lfn+255
	jmp .cont
.sfn	push es
	push di
	push ds
	push si
	mov WORD [cs:.off],.lfn+255
	mov WORD [cs:.len],0
	push cs
	pop ds
	push cs
	pop es
	mov si,[cs:_buff.off]
	lea si,[fs:si+bp+11]
	mov di,[cs:.off]
	mov cx,3
	std
.ext_lp	dec si
	cmp BYTE [ds:si],' '
	je .ext_ct
	dec di
	mov bh,[ds:si]
	mov [es:di],bh
	inc WORD [cs:.len]
.ext_ct	loop .ext_lp
	dec di
	mov BYTE [es:di],'.'
	inc WORD [cs:.len]
	mov cx,8
.sht_lp	dec si
	cmp BYTE [ds:si],' '
	je .sht_ct
	dec di
	mov bh,[ds:si]
	mov [es:di],bh
	inc BYTE [cs:.len]
.sht_ct	loop .sht_lp
	mov [cs:.off],di
	pop si
	pop ds
	pop di
	pop es
	pop cx
	push cx
.cmp	push ds
	push si
	push es
	push di
	push cs
	pop es
	mov di,[cs:.off]
	cld
	repe cmpsb
	pop di
	pop es
	pop si
	pop ds
	mov WORD [cs:.len],0
	mov WORD [cs:.off],.lfn+255
	jz .lp_dn	; file found
.cont	add bp,32	; look at next dir entry (32 bytes later)
	cmp bp,[cs:bytes_per_sect]
	jb .loop
	xor bp,bp
	stc
	jmp .exit	; file not found
.lp_dn	push es
	push di
	push ds
	push si
	push cs
	pop ds
	mov si,[cs:_buff.off]
	lea si,[fs:si+bp]
	mov cx,32
	cld
	rep movsb
	pop si
	pop ds
	pop di
	pop es
	clc
.exit	pop cx
	pop fs
	ret
.lfn	times	255	db	0
.long		db	0
.off		dw	.lfn+255
.len		dw	0
.lfn_chrs	db	30,28,24,22,20,18,16,14,9,7,5,3,1

_fat16_for_each_clust_in_chain:	; starting cluster in cx
				; fs:si -> function
				;  Called with each cluster number in cx
				;  for every cluster in chain.
				;  Passed es:di, for any other inputs.
				;   this is passed unmodified, so if
				;   the called function changes the value
				;   the new value will be passed to it.
				;  If this function sets carry on return
				;  we terminate, successfully if ax=0
				;  on error if ax nonzero
				;  Must clear carry flag for success
				; Carry Flag set on error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	stc
	ret
.start	push eax
	push edx
	push es
	push di
	push cx
	push fs
	push si
.loop	cmp cx,0xFFF8
	jae .done
	cmp cx,1
	jbe .done
	mov [cs:.fn_ptr],si
	mov [cs:.fn_ptr+2],fs
	call far [cs:.fn_ptr]
	pop si
	pop fs
	push fs
	push si
	jnc .cont
	test ax,ax
	stc
	jnz .exit
	clc
	jmp .done
.cont	mov eax,2
	mul cx
	div WORD [cs:bytes_per_sect]
	add dx,[cs:fat16.fat_off]
	jnc .nocar
	inc eax
	sub dx,[cs:bytes_per_sect]
.nocar	cmp dx,[cs:bytes_per_sect]
	jb .remok
	inc eax
	sub dx,[cs:bytes_per_sect]
.remok	push es
	push di
	push dx
	xor edx,edx
	add eax,[cs:fat16.first_fat_sect]
	adc edx,[cs:fat16.fat_sect_high]
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	mov cx,1
	mov bl,[cs:fat16.drive_num]
	call read
	pop dx
	pop di
	pop es
	jc .exit
	mov bp,dx
	add bp,[cs:_buff.off]
	push fs
	push WORD [cs:_buff.seg]
	pop fs
	mov cx,[fs:bp]
	pop fs
	jmp .loop
.done	clc
.exit	pop si
	pop fs
	pop cx
	pop di
	pop es
	pop edx
	pop eax
	ret
.fn_ptr dw 0
	dw 0

struc _fat16_load_params
.buff_seg	resw	1
.buff_off	resw	1
.buff_len_sect	resw	1
.buff_len_rem	resw	1
endstruc

; loads one cluster of a file to es:di
_fat16_load_one_cluster_of_file:
			; cluster number in cx
			; es:di -> _fat16_load_params
			; carry flag set on error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	mov ax,1
	stc
	retf
.start	push ds
	push si
	push bp
	push cx
	push es
	push di
	mov bl,[cs:fat16.drive_num]
	mov bp,[es:di+_fat16_load_params.buff_len_sect]
	test bp,bp
	jnz .sec_ok
	cmp WORD [es:di+_fat16_load_params.buff_len_rem],0
	stc
	je .exit
.sec_ok	mov ax,[es:di+_fat16_load_params.buff_seg]
	mov di,[es:di+_fat16_load_params.buff_off]
	mov es,ax
	mov ax,[cs:fat16.sect_per_clust]
	sub cx,2
	mul cx
	shl edx,16
	mov dx,ax
	mov eax,edx
	xor edx,edx
	add eax,[cs:fat16.first_data_sect]
	adc edx,[cs:fat16.data_sect_high]
	mov cx,[cs:fat16.sect_per_clust]
	sub bp,cx
	jae .rd_cl
	add bp,cx
	mov cx,bp
	call read
	jnc .r_ok
	mov ax,1
	jmp .exit
.r_ok	movzx ebp,bp
	add eax,ebp
	adc edx,0
	pop si
	pop ds
	push ds
	push si
	push eax
	push edx
	mov ax,[cs:bytes_per_sect]
	mul bp
	push dx
	push ax
	mov ax,es
	shr ax,12
	pop dx
	add di,dx
	pop dx
	adc ax,dx
	shl ax,12
	mov es,ax
	mov [ds:si+_fat16_load_params.buff_seg],es
	mov [ds:si+_fat16_load_params.buff_off],di
	mov WORD [ds:si+_fat16_load_params.buff_len_sect],0
	pop edx
	pop eax
	cmp WORD [ds:si+_fat16_load_params.buff_len_rem],0
	clc
	je .exit
	mov cx,1
	push WORD [cs:_buff.seg]
	pop es
	mov di,[cs:_buff.off]
	call read
	mov ax,1
	jc .exit
	mov cx,[ds:si+_fat16_load_params.buff_len_rem]
	mov ax,[ds:si+_fat16_load_params.buff_seg]
	mov es,ax
	mov di,[ds:si+_fat16_load_params.buff_off]
	push WORD [cs:_buff.seg]
	pop ds
	mov si,[cs:_buff.off]
	cld
	rep movsb
	pop si
	pop ds
	mov [ds:si+_fat16_load_params.buff_seg],es
	mov [ds:si+_fat16_load_params.buff_off],di
	mov WORD [ds:si+_fat16_load_params.buff_len_rem],0
	push ds
	push si
	clc
	jmp .exit
.rd_cl	call read
	mov ax,1
	jc .exit
	mov ax,es
	shr ax,12
	add di,[cs:fat16.bytes_per_clust]
	adc ax,[cs:fat16.bytes_per_clust+2]
	shl ax,12
	mov es,ax
	pop si
	pop ds
	push ds
	push si
	mov [ds:si+_fat16_load_params.buff_seg],es
	mov [ds:si+_fat16_load_params.buff_off],di
	mov [ds:si+_fat16_load_params.buff_len_sect],bp
	clc
.exit	pop di
	pop es
	pop cx
	pop bp
	pop si
	pop ds
	retf

_fat16_load_file_from_dir_entry:	; ds:si -> directory entry of file to load
					; es:di -> buffer big enough to hold file
					; Carry Flag set on error
	cmp BYTE [cs:fat16.initialized],0
	jne .start
	stc
	ret
.start	push edx
	push ax
	push es
	push di
	push ds
	push si
	push fs
	push cx
	mov edx,[ds:si+28]
	mov ax,dx
	shr edx,16
	div WORD [cs:bytes_per_sect]
	mov [cs:.params+_fat16_load_params.buff_len_sect],ax
	mov [cs:.params+_fat16_load_params.buff_len_rem],dx
	mov cx,[ds:si+26]
	mov [cs:.params+_fat16_load_params.buff_seg],es
	mov [cs:.params+_fat16_load_params.buff_off],di
	push cs
	pop es
	lea di,[cs:.params]
	push cs
	pop fs
	mov si,_fat16_load_one_cluster_of_file
	call _fat16_for_each_clust_in_chain
.exit	pop cx
	pop fs
	pop si
	pop ds
	pop di
	pop es
	pop ax
	pop edx
	ret
.params	times	_fat16_load_params_size db 0

; FAT16_INCLUDED
%endif
