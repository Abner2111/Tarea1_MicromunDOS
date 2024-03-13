[bits 16]
%define SECTOR_AMOUNT 0X12

jmp short start

start:
cli ;desabilitar interrupciones
xor ax, ax
mov ds, ax
mov ss, ax
mov es, ax
mov fs, ax
mov gs, ax
mov sp, 0x6ef0

sti ;habilitar interrupcioes

mov ah, 0
int 0x13 ;interrupcion para utilizar el disco

mov bx, 0x8000 ;direccion de memoria del juego
mov al, SECTOR_AMOUNT
mov ch, 0 ;cilindro
mov dh, 0 ;cabeza
mov cl, 2 ;sector

mov ah, 2 ;lee desde el disco
int 0x13 ; interrupt para usar disco en BIOS
jmp 0x8000 ; ir al juego

;PADDING AND SIGNATURE

times 510-($-$$) db 0;
db 0x55
db 0xaa
