[bits 16]
section .text
escalate_size:
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
    ret 
    
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
    
    call escalate_size

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
        mov si, 6;
        mov di, 6;
	mov bx, sp;
	; recuperar x, y del stack
	mov dx, [bx+9*2];
	mov cx, [bx+9*2+2];
  
    call escalate_size
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

drawScreen_Progressive:
	pusha

	;mover constantes y stackpointer
    mov si, 320;
    mov di, 200;
    mov bx, sp;
	; recuperar color

	mov al, [bx+9*2];

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
			push cx
			push dx
			mov cx, 0
			mov ah, 86h
			mov dx, 0
			int 15h;sleep
			pop dx
			pop cx
			popa
		sub di, 1         ;decrease di by one and set flags
		jnz .for_y        ;repeat for y-length times
		pop di            ;restore di to y-length
	sub si, 1             ;decrease si by one and set flags
	jnz .for_x            ;repeat for x-length times
	pop si                ;restore si to x-length  -> starting state restored
	popa
	ret

get_text_draw_parameters:
    ;mover constantes y stackpointer
    mov si, 4;
    mov di, 5;
    mov bx, sp;
    
    ; recuperar x, y del stack
    mov dx, [bx+9*2+2];
    mov cx, [bx+9*2+2+2];
    
    call escalate_size
    
    mov bh, 0     ;page number (0 is default)
    mov al, 30; Color de la tortuga
    mov ah, 0xC   ;write pixel at coordinate
    ret

drawT:
	pusha
	call get_text_draw_parameters
	;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3

		add cx, 1   ;cx = x-coordinate
		add dx, 2    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1    ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, 2    ;cx = x-coordinate
		int 0x10      ;draw pixel!

	popa
	ret
	
drawL:
	pusha
   	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3

		add cx, 2   ;cx = x-coordinate
		add dx, 2    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

	popa
	ret
	
drawW:
	pusha
	call get_text_draw_parameters
	
		;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3
		
		add cx, 2   ;cx = x-coordinate
		add dx, 2    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

	popa
	ret

draw0:
    cmp dx, 0;
    jne return
    
    pusha
    call get_text_draw_parameters
	;Dibujamos de derecha a izquierda de abajo a arriba
	;dibujar Linea 3

	add cx, 2   ;cx = x-coordinate
	add dx, 4    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 2
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, 2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, 2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

    popa
    ret
	
draw1:
    cmp dx, 1;
    jne return
	pusha
	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3

		add cx, 1   ;cx = x-coordinate
		add dx, 4    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

	popa
	ret
	
draw2:
    cmp dx, 2;
    jne return
	pusha
	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3
		
		add cx, 2   ;cx = x-coordinate
		add dx, 4    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		add cx, 1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		add cx, 1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

	popa
	ret	
draw3:
    cmp dx, 3;
    jne return
	pusha
	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;dibujar Linea 3

		
		add cx, 2   ;cx = x-coordinate
		add dx, 4    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		add cx, 2   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		add cx, 2   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

	popa
	ret	
	
draw4:
    cmp dx, 4;
    jne return
	pusha
	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;Dibujar linea 3
		
		add cx, 2   ;cx = x-coordinate
		add dx, 4    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, 2   ;cx = x-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -2   ;cx = x-coordinate
		int 0x10      ;draw pixel!

	popa
	ret
		
draw5:
    cmp dx, 5;
    jne return
	pusha
	call get_text_draw_parameters
		;Dibujamos de derecha a izquierda de abajo a arriba
		;Dibujar linea 3
		
		add cx, 2   ;cx = x-coordinate
		add dx, 4    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		add cx, 2   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, -1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		
		;Dibujar linea 1
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!

		;Dibujar linea 2
		add dx, -1    ;dx = y-coordinate
		int 0x10      ;draw pixel!
		add cx, 1   ;cx = x-coordinate
		int 0x10      ;draw pixel!
		add cx, 1   ;cx = x-coordinate
		int 0x10      ;draw pixel!

	popa
	ret

draw6:
    cmp dx, 6;
    jne return
    pusha
    call get_text_draw_parameters
        ; Draw number 6
        ;bottom
        add cx, 2   ; cx = x-coordinate
        add dx, 4   ; dx = y-coordinate
        int 0x10    ; draw pixel!
        add cx, -1  ; cx = x-coordinate
        int 0x10    ; draw pixel!
        add cx, -1  ; cx = x-coordinate
        int 0x10    ; draw pixel!

        ; Draw diagonal line of number 6
        add dx, -1   ; dx = y-coordinate
        int 0x10     ; draw pixel!
        add cx, 2    ; cx = x-coordinate
        int 0x10     ; draw pixel!

        ; Draw horizontal line of number 6
        add dx, -1   ; dx = y-coordinate
        int 0x10    ; draw pixel!
        add cx, -1  ; cx = x-coordinate
        int 0x10    ; draw pixel!
        add cx, -1  ; cx = x-coordinate
        int 0x10    ; draw pixel!
        
        add dx, -1   ; dx = y-coordinate
        int 0x10     ; draw pixel!
        
        ; Draw horizontal line of number 6
        add dx, -1   ; dx = y-coordinate
        int 0x10    ; draw pixel!

    popa
    ret

draw7:
    cmp dx, 7;
    jne return
    pusha
    call get_text_draw_parameters
	add cx, 2   ;cx = x-coordinate
	add dx, 4    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 2
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx,-1     ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx,-1     ;cx = x-coordinate
	int 0x10      ;draw pixel!

    popa
    ret

draw8:
    cmp dx, 8;
    jne return
    pusha
    call get_text_draw_parameters
        ; Draw number 8
	add cx, 2   ;cx = x-coordinate
	add dx, 4    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 2
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, 2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, 2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

    popa
    ret

draw9:
    cmp dx, 9;
    jne return
    pusha
    call get_text_draw_parameters
        ; Draw number 9
	add cx, 2   ;cx = x-coordinate
	add dx, 4    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 2
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, 2   ;cx = x-coordinate
	int 0x10      ;draw pixel!

	;Dibujar linea 1
	add dx, -1    ;dx = y-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!
	add cx, -1   ;cx = x-coordinate
	int 0x10      ;draw pixel!

    popa
    ret

return:
	ret

drawMiniBox:
	pusha

	;mover constantes y stackpointer
    call get_text_draw_parameters
    mov al, 0;

	push si               ;save x-length
	.for_x:
		push di           ;save y-length
		.for_y:
			pusha
			int 0x10      ;draw pixel!
			add cx, si    ;cx = x-coordinate
			add cx, -1    ;cx = x-coordinate
			add dx, di    ;dx = y-coordinate
			add dx, -1    ;dx = y-coordinate
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
	
	
;code by PUSTY https://github.com/Pusty/realmode-assembly/tree/master/part5 
draw_Images:
	pusha
	    ; recuperar x, y del stack

	xor ax, ax
	lodsb
	mov cx, ax                      ;x-length (first byte of binary)
	lodsb
	mov dx, ax                      ;y-length (2nd byte of binary)
	
	mov bx, sp;
    	mov bx, [bx+9*2];			
    	mov bh, 0               ;page number (0 is default)
	.for_x:
		push dx
		.for_y:
		
			add dx, bx	
			lodsb                   ;read byte to al (color) -> next byte
			mov ah, 0xC             ;write pixel at coordinate (cx, dx)
			int 0x10                ;draw!
			sub dx, bx
		sub dx, 1                   ;decrease dx by one and set flags
		jnz .for_y                  ;repeat for y-length
		pop dx                      ;restore dx (y-length)

	sub cx, 1                       ;decrease si by one and set flags
	jnz .for_x                      ;repeat for x-length
	popa                            ;restore everything
	ret
animation_loop:
	
	color1:
		mov word ax, [pixel_color]
		add word ax, [draw]
		push word ax
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di
		add word [x_coord], 1
	; Wait for a short delay
    mov cx, 1
    mov ah, 86h
    int 15h

	color2:
		mov word ax, [pixel_color]
		add word ax, [draw]
		push word ax
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di
		add word [y_coord], 1
	; Wait for a short delay
    mov cx, 1
    mov ah, 86h
    int 15h

	color3:
		mov word ax, [pixel_color]
		add word ax, [draw]
		push word ax
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di
		add word [x_coord], -1
	; Wait for a short delay
    mov cx, 1
    mov ah, 86h
    int 15h
	color4:
		mov word ax, [pixel_color]
		add word ax, [draw]
		push word ax
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di
		add word [y_coord], -1
		
	; Wait for a short delay
    mov cx, 1
    mov ah, 86h
    int 15h
	erase:
		push word [black_color]
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di

		add word [x_coord], 1
		
		push word [black_color]
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di

		add word [y_coord], 1

		push word [black_color]
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di

		add word [x_coord], -1

		push word [black_color]
		push word [x_coord]
		push word [y_coord]
		call drawBox
		pop di
		pop di
		pop di

		add word [y_coord], -1
	dec byte [draw]
	cmp byte [draw],0
    jne animation_loop
	ret
print_start:
	print_loop:
		; Load character from string
		lodsb

		; Check for null terminator
		cmp al, 0
		je print_done

		; Call BIOS teletype function to print the character
		mov ah, 0Eh     ; Function code for teletype
		mov bh, 1       ; Page number (0 for default)
		mov bl, 15      ; Color attribute (white on black)
		int 10h

		; Repeat for next character
		jmp print_loop
	print_done:
	ret												
LeftCommand: incbin "LeftCommand.bin"
DownCommand: incbin "DownCommand.bin"
RigthCommand: incbin "RigthCommand.bin"
UpCommand: incbin "UpCommand.bin"
MenuCommand: incbin "MenuCommand.bin"
