org 0x8000
[bits 16]

section .text
    global _start

_start:
    xor ax, ax ;setting ax to 0
    mov ah, 0   ;Set display mode
    mov al, 13h ;13h = 320x200, 256 colors
    int  0x10   ;Video BIOS Services

    
readNextChar:
    waitForInputLoop:
            
        mov ah, 1
        int 0x16
        jz waitForInputLoop

        mov ah, 0
        int 0x16

        
    in al, 60h
    jmp draw_with_keys

draw_with_keys:
    cmp ah, 0x48 ;up
    je handle_north

    cmp ah,0x50 ;down
    je handle_south

    cmp ah, 0x4b ;left
    je handle_west

    cmp ah,  0x4d ;right
    je handle_east

    jmp readNextChar
handle_north:
    push 11  ;COLOR
    push 80 ;X-POS
    push 2  ;Y-POS
    call drawturtle
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    jmp readNextChar

handle_south:
  
    push 12  ;COLOR
    push 80;X-POS
    push 30  ;Y-POS
    call drawturtle
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    jmp readNextChar
handle_east:

    push 13  ;COLOR
    push 90 ;X-POS
    push 18  ;Y-POS
    call drawturtle
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    jmp readNextChar
handle_west:
    push 14  ;COLOR
    push 70 ;X-POS
    push 18  ;Y-POS
    call drawturtle
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    jmp readNextChar

draw_test:
    ;Pruebas del dibujado
    push 11  ;COLOR
    push 1 ;X-POS
    push 0  ;Y-POS
    call drawBox
    call drawturtle

    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    ;este conjunto de instrucciones es equivalente a add sp, -3*2

    push 0x11; color
    push 1 ;X-POS
    push 3  ;Y-POS
    call drawBox

    push 0x16; color
    push 1 ;X-POS
    push 2  ;Y-POS
    call drawturtle
    call drawBox

    push 0x19; color
    push 1 ;X-POS
    push 1  ;Y-POS
    call drawturtle

    jmp readNextChar;

drawBox:
	pusha

	;mover constantes y stackpointer
    mov si, 6;
    mov di, 6;
	mov bx, sp;
	; recuperar x, y & color

	mov dx, [bx+9*2];
	mov cx, [bx+9*2+2];
	mov al, [bx+9*2+4];

    push ax; guardar el color
    ;Escalar Y
    mov ax, dx;
    mul si;
    mov dx,ax;

    push dx; guardar valor de Y

    ;Escalar X
    mov ax, cx;
    mul si; mul sobreescribe dx siempre
    mov cx,ax;
	
    pop dx; recuperar valor de Y

    pop ax


	push si               ;save x-length
	.for_x:
		push di           ;save y-length
		.for_y:
			pusha
			mov bh, 0     ;page number (0 is default)
			add cx, si    ;cx = x-coordinate
			add dx, di    ;dx = y-coordinate
			mov ah, 0xC   ;write pixel at coordinate
			int 0x10      ;draw pixel!
			popa
		sub di, 1         ;decrease di by one and set flags
		jnz .for_y        ;repeat for y-length times
		pop di            ;restore di to y-length
	sub si, 1             ;decrease si by one and set flags
	jnz .for_x            ;repeat for x-length times
	pop si                ;restore si to x-length  -> starting state restored
	popa
	ret

drawturtle:
	pusha
	;mover constantes y stackpointer
    mov si, 6;
    mov di, 6;
	mov bx, sp;
	; recuperar x, y del stack
	mov dx, [bx+9*2];
	mov cx, [bx+9*2+2];

	;Escalar Y
    mov ax, dx;
    mul si;
    mov dx,ax;

    push dx; guardar valor de Y

    ;Escalar X
    mov ax, cx;
    mul si; mul sobreescribe dx siempre
    mov cx,ax;
	pop dx;
			;Dibujamos de derecha a izquierda de abajo a arriba
			;dibujar Linea 6
			mov bh, 0     ;page number (0 is default)
			mov al, 40; Color de la tortuga

			add cx, 6    ;cx = x-coordinate
			add dx, 6    ;dx = y-coordinate
			mov ah, 0xC   ;write pixel at coordinate
			int 0x10      ;draw pixel!
			add cx, -5    ;cx = x-coordinate
			int 0x10      ;draw pixel!

			;Dibujar linea 5
			add cx, 4    ;cx = x-coordinate
			add dx, -1    ;dx = y-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!

			;Dibujar linea 4
			add cx, 4    ;cx = x-coordinate
			add dx, -1    ;dx = y-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -3    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!

			;Dibujar linea 3
			add cx, 5    ;cx = x-coordinate
			add dx, -1    ;dx = y-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -3    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!

			;Dibujar linea 2
			add cx, 4    ;cx = x-coordinate
			add dx, -1    ;dx = y-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!

			;dibujar linea 1
			add cx, 4    ;cx = x-coordinate
			add dx, -1    ;dx = y-coordinate
			int 0x10      ;draw pixel!
			add cx, -2    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -1    ;cx = x-coordinate
			int 0x10      ;draw pixel!
			add cx, -2    ;cx = x-coordinate
			int 0x10      ;draw pixel!
	popa
	ret

end:
	jmp $
	nop