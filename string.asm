%macro string 2+
	%1 db %2
	%tok(%strcat(%str(%1), "_len")) equ $-%1
%endmacro

%macro string 1
	string %1,%str(%1)
%endmacro
