.DELETE_ON_ERROR:

.PHONY : all clean install_bootstrap install_stage2 install

BUILD_DIR = build

all : $(BUILD_DIR)/bootstrap $(BUILD_DIR)/stage2

$(BUILD_DIR)/bootstrap : bootstrap.asm
	mkdir -p $(BUILD_DIR)
	nasm -f bin -o $(BUILD_DIR)/bootstrap bootstrap.asm

BOOTLOADER_PATH = /boot.bin
$(BUILD_DIR)/stage2 : stage2.asm disk_read.asm fs.asm fat16.asm ext2.asm paths.asm print.asm string.asm
	mkdir -p $(BUILD_DIR)
	nasm -f bin -o $(BUILD_DIR)/stage2 -d'BOOTLOADER=$(BOOTLOADER_PATH)' stage2.asm

clean :
	-rm -r $(BUILD_DIR)

DRIVE = test/drive

install_bootstrap :
	dd if=$(BUILD_DIR)/bootstrap of=$(DRIVE) bs=446 count=1 conv=notrunc

install_stage2 :
	dd if=$(BUILD_DIR)/stage2 of=$(DRIVE) bs=512 count=51 conv=notrunc seek=1

install : install_bootstrap install_stage2
