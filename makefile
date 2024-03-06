all: build1
	nasm bootloader.asm -f elf32 -F dwarf -o bootloader.o
	ld -Ttext=0x7c00 -m elf_i386 bootloader.o -o bootloader.elf 
	nasm PruebaImagen.asm -f elf32 -F dwarf -o PruebaImagen.o
	ld -Ttext=0x8000 -m elf_i386 PruebaImagen.o -o PruebaImagen.elf 
	objcopy -O binary bootloader.elf bootloader.img
	objcopy -O binary PruebaImagen.elf PruebaImagen.img
	cat bootloader.img PruebaImagen.img > result.img
	unset GTK_PATH
	qemu-system-i386 result.img -s -S &
	gdb 

build1:
	echo "org 0x7c00"|cat - bootloader.asm >Build/bootloader.asm
	echo "org 0x8000"|cat - PruebaImagen.asm >Build/PruebaImagen.asm
	nasm Build/bootloader.asm -f bin -o Build/bootloader.o
	nasm Build/PruebaImagen.asm -f bin -o Build/PruebaImagen.o
	cat bootloader.img PruebaImagen.img > Build/result.img
