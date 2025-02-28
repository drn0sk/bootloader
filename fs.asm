%define FAT16	1
%define EXT2	2
; other filesystems

%define init			WORD [cs:init_ptr]
init_ptr		dw	0
%define load_file		WORD [cs:load_file_ptr]
load_file_ptr		dw	0
%define load_file_relative	WORD [cs:load_file_rel_ptr]
load_file_rel_ptr	dw	0
%define get_file_size		WORD [cs:get_file_sz_ptr]
get_file_sz_ptr		dw	0

%macro _set_fs_ptrs 1
	mov WORD [cs:init_ptr],%1_init
	mov WORD [cs:load_file_ptr],%1_load_file
	mov WORD [cs:load_file_rel_ptr],%1_load_file_relative
	mov WORD [cs:get_file_sz_ptr],%1_get_file_size
%endmacro

%macro set_fs 1
	mov cx,%1
	call _fs_init
%endmacro

_fs_init:	; filesystem type in cx
	clc
	cmp cx,FAT16
	jne .notf16
	_set_fs_ptrs fat16
	ret
.notf16	cmp cx,EXT2
	jne .note2
	_set_fs_ptrs ext2
	ret
.note2:
	; add more filesystems
	stc
	ret
