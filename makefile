all:
	nasm bootloader.asm -f elf32 -F dwarf -o bootloader.o
	ld -Ttext=0x7c00 -m elf_i386 bootloader.o -o bootloader.elf 
	objcopy -O binary bootloader.elf bootloader.img
	unset GTK_PATH
	qemu-system-i386 bootloader.img -s -S &
	gdb 
