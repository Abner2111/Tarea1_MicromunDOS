set architecture i8086
target remote localhost:1234
symbol-file bootloader.elf
layout split
layout src
layout regs

break *0x7c00
break end
