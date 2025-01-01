%ifndef DISK_READ
	%define DISK_READ
	segment .text
; reads cx sectors, on drive bl, from LBA edx:eax to es:di
; carry flag set on error with error code in ah
%define	read	WORD [cs:read_fnptr]
; default to using CHS which will always be supported
read_fnptr	dw	read_CHS
DAP:
.size           db      16
                db      0
.num_sectors    dw      0
.offset         dw      0
.segment        dw      0
.lower_LBA      dd      0
.upper_LBA      dd      0

read_LBA:		; reads using int 13h extensions
	test cx,cx
	clc
	jz .exit
	push ds
	pushad
	mov WORD [ds:DAP.num_sectors],cx
	mov WORD [ds:DAP.offset],di
	mov WORD [ds:DAP.segment],es
	mov DWORD [ds:DAP.lower_LBA],eax
	mov DWORD [ds:DAP.upper_LBA],edx
	push cs
	pop ds
	mov si,DAP
	mov dl,bl
	mov ah,0x42
	int 0x13
	popad
	pop ds
.exit	ret

read_CHS:		; converts LBA to CHS to read using int 13h, ah=02h
	stc	; not supported yet
	ret

bytes_per_sect	dw	512
drive_params	dw	0x001A
		times 0x1A-$+drive_params db 0
installation_check:	; sets read ([cs:read_fnptr]) to read_LBA if
			; bios int 13h extensions are supported
			; otherwise sets read to read_CHS
			; drive number in dl
			; carry flag set on error
	push ax
	push bx
	push cx
	push ds
	push si
	push dx
	mov ah,0x41
	mov bx,0x55AA
	int 0x13
	jc .CHS
	cmp bx,0xAA55
	jne .CHS
	test cx,0x01
	jz .CHS
.LBA:
	mov ah,0x48
	pop dx
	push cs
	pop ds
	mov si,drive_params
	int 0x13
	jc .exit
	mov WORD ax,[ds:drive_params+0x18]
	mov WORD [ds:bytes_per_sect],ax
	mov WORD [cs:read_fnptr],read_LBA
	jmp .success
.CHS:
	mov WORD [cs:read_fnptr],read_CHS
.success:
	clc
.exit:
	pop si
	pop ds
	pop cx
	pop bx
	pop ax
	ret
load_partition_table:	; drive in bl
			; location to load to in es:di
			; carry flag set on error
	mov cx,1
	xor edx,edx
	xor eax,eax
	call read
	ret

; DISK_READ
%endif
