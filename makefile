all:
	nasm PruebaImagen.asm -f elf -o PruebaImagen.o
	nasm bootloader.asm -f bin  -o bootloader.bin
	ld -m elf_i386 -s PruebaImagen.o
	objcopy a.out
	qemu-system-i386 -fda bootloader.bin