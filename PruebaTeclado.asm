org 0x8000
%use masm
xor ax, ax
mov es, ax

readNextChar:
    waitForInputLoop:
        in al, 64h ;status byte 

        and al, 10b ;check input

        jz waitForInputLoop
    
    in al, 60h ; read input to ax

    push ax
    call printChar
    jmp readNextChar

printChar:
    push ax
    push bx

    mov bx, es:[-2]

    mov word ptr es:[bx], 0xa

    mov ax, [esp+6]

    mov word ptr es:[bx+1], ax
    
    add bx, 2

    mov es:[-2], bx

    pop bx
    pop ax

    ret 2