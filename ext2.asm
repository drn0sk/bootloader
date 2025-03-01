%ifndef EXT2_INCLUDED
	%define EXT2_INCLUDED
	segment .text

; ext2_init
;LBA of partition in eax
;drive number in bl
;carry flag set on error

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
