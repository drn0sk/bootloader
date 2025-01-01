.DELETE_ON_ERROR:

.PHONY : all clean install_bootstrap install_stage2 install

all : bootstrap stage2

bootstrap bootstrap.lst &: bootstrap.asm
	nasm -f bin -l bootstrap.lst -o bootstrap bootstrap.asm

BOOTLOADER_PATH = /Bootloader.bin

stage2 stage2.lst &: stage2.asm disk_read.asm fat16.asm paths.asm print.asm
	nasm -f bin -l stage2.lst -o stage2 -d'BOOTLOADER_PATH=$(BOOTLOADER_PATH)' stage2.asm

clean :
	-rm bootstrap bootstrap.lst
	-rm stage2 stage2.lst

install_bootstrap :
	dd if=bootstrap of=drive bs=446 count=1 conv=notrunc

install_stage2 :
	dd if=stage2 of=drive bs=512 count=51 conv=notrunc seek=1

install : install_bootstrap install_stage2
