org 0x7c00

%define SECTOR_AMOUNT 0X4

jmp short start

start:
    cli;
    xor ax,ax
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov sp, 0x6ef0
    sti;

    mov ah, 0   ;resetea estado del disco
    int 0x13    ;interrupcion para utilzar el disco con el bios

    mov bx, 0x8000
    mov al, SECTOR_AMOUNT;
    mov ch, 0   ;cilindro = 0
    mov dh, 0   ;cabeza = 0
    mov cl, 2   ;sector = 2
    mov ah, 2
    int 0x13
    jmp 0x8000

;PADDING AND SIGNATURE
times 510-($-$$)db 0;
db 0x55
db 0xaa