all:
	rm -rf build
	mkdir build
	nasm -f bin PruebaTeclado.asm -o build/PruebaTeclado.bin
	nasm -f bin bootloader_v2.asm -o build/bootloader_v2.bin
	cat build/bootloader_v2.bin build/PruebaTeclado.bin > build/micromundos.bin
	qemu-system-i386 -fda build/micromundos.bin