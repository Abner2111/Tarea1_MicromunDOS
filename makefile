all:
	rm -rf build
	mkdir build
	nasm -f bin ../game.asm -o build/game.bin
	nasm -f bin ../only_bootloader.asm -o build/bootloader.bin
	cat build/bootloader.bin build/game.bin > build/micromundos.bin
	qemu-system-x86_64 -fda build/micromundos.bin
modular:
	rm -rf build
	mkdir build
	nasm -f bin game.asm -o build/game.bin
	nasm -f bin only_bootloader.asm -o build/bootloader.bin
	cat build/bootloader.bin build/game.bin > build/micromundos.bin
	qemu-system-x86_64 -fda build/micromundos.bin
