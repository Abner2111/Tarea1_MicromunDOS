all:
	rm -rf build
	mkdir build
	echo "org 0x7c00"|cat - bootloader.asm>build/bootloader.asm
	echo "org 0x8000"|cat - game.asm>build/game.asm
	nasm -f bin build/game.asm -o build/game.bin
	nasm -f bin build/bootloader.asm -o build/bootloader.bin
	cat build/bootloader.bin build/game.bin > build/micromundos.bin
	
q:
	rm -rf debug
	mkdir debug
	nasm bootloader.asm -f elf32 -F dwarf -o debug/bootloader.o
	ld -Ttext=0x7c00 -m elf_i386 debug/bootloader.o -o debug/bootloader.elf 
	nasm game.asm -f elf32 -F dwarf -o debug/game.o
	ld -Ttext=0x8000 -m elf_i386 debug/game.o -o debug/game.elf 
	objcopy -O binary debug/bootloader.elf debug/bootloader.img
	objcopy -O binary debug/game.elf debug/game.img
	cat debug/bootloader.img debug/game.img > debug/result.img
	unset GTK_PATH
	qemu-system-i386 debug/result.img -s -S &
	gdb 

q1:
	rm -rf build
	mkdir build
	echo "org 0x7c00"|cat - bootloader.asm>build/bootloader.asm
	echo "org 0x8000"|cat - game.asm>build/game.asm
	nasm -f bin build/game.asm -o build/game.bin
	nasm -f bin build/bootloader.asm -o build/bootloader.bin
	cat build/bootloader.bin build/game.bin > build/micromundos.bin
	qemu-system-x86_64 -fda build/micromundos.bin
	
mount: 
	sudo dd if=build/result.img of=/dev/sdb bs=512 count=18
