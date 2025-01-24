.DELETE_ON_ERROR:

.PHONY : all clean install_bootstrap install_stage2 install

BUILD_DIR = build

all : $(BUILD_DIR)/bootstrap $(BUILD_DIR)/stage2 $(BUILD_DIR)/loader

$(BUILD_DIR)/bootstrap : bootstrap.asm
	mkdir -p $(BUILD_DIR)
	nasm -f bin -o $(BUILD_DIR)/bootstrap bootstrap.asm

BOOTLOADER_PATH = /boot.bin
$(BUILD_DIR)/stage2 : stage2.asm disk_read.asm fs.asm fat16.asm paths.asm print.asm string.asm
	mkdir -p $(BUILD_DIR)
	nasm -f bin -o $(BUILD_DIR)/stage2 -d'BOOTLOADER=$(BOOTLOADER_PATH)' stage2.asm

CONFIG_PATH = /boot.conf
$(BUILD_DIR)/loader : bootloader.asm disk_read.asm fs.asm fat16.asm paths.asm print.asm string.asm
	mkdir -p $(BUILD_DIR)
	nasm -f bin -o $(BUILD_DIR)/loader -d'CONFIG=$(CONFIG_PATH)' bootloader.asm

clean :
	-rm -r $(BUILD_DIR)

DRIVE = test/drive
USE_LOOP = yes
PART_OFFSET = 1048576

install_bootstrap :
	dd if=$(BUILD_DIR)/bootstrap of=$(DRIVE) bs=446 count=1 conv=notrunc

install_stage2 :
	dd if=$(BUILD_DIR)/stage2 of=$(DRIVE) bs=512 count=51 conv=notrunc seek=1

install_loader :
	$(eval TMP := $(shell mktemp -d))
ifdef USE_LOOP
	mount -o loop,offset=$(PART_OFFSET) $(DRIVE) $(TMP)
else
	mount -o offset=$(PART_OFFSET) $(DRIVE) $(TMP)
endif
	cp $(BUILD_DIR)/loader $(TMP)$(BOOTLOADER_PATH)
	umount -d $(TMP)
	rm -rf $(TMP)

install : install_bootstrap install_stage2 install_loader
