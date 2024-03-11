set architecture i8086
set disassembly-flavor intel
add-symbol-file debug/bootloader.elf
add-symbol-file debug/game.elf
target remote localhost:1234
layout split
layout src
layout regs
break *0x7c00
break *0x8000
break end 
break drawT
c
