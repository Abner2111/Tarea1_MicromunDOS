[bits 16]
section .data
    x_coord     dw      0
    y_coord     dw      0
    trtl_color  dw      40
    n_color     dw      13
    s_color     dw      14
    e_color     dw      15
    w_color     dw      10
    last_dir    dw      0
    draw        db      0
    draw_color  dw      16

section .text
    global _start

_start:
    xor ax, ax      ;setting ax to 0
    mov ah, 0       ;Set display mode
    mov al, 13h     ;13h = 320x200, 256 colors
    int  0x10       ;Video BIOS Services
    
    push 11         ;COLOR
    push word [x_coord]  ;X-POS
    push word [y_coord]  ;Y-POS
    call drawturtle
    pop di          ;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    
readNextChar:
    waitForInputLoop:
            
        mov ah, 1
        int 0x16 ;needed for consistent behavior
        jz waitForInputLoop

        mov ah, 0
        int 0x16 ;saves scan code to al

        
    
    jmp handle_keys

handle_keys:
    cmp ah, 0x48 ;up
    je handle_north

    cmp ah,0x50 ;down
    je handle_south

    cmp ah, 0x4b ;left
    je handle_west

    cmp ah,  0x4d ;right
    je handle_east

    cmp ah, 0x39 ;space
    je handle_space

    jmp readNextChar

handle_space:
    not byte [draw]
    jmp readNextChar

handle_north:
    
    cmp byte [draw],0 ;check draw
    je not_draw_n
    jne draw_n
    not_draw_n:
    call make_color_black
    jmp continue_n


    draw_n:
    mov word ax, [n_color]
    mov word [draw_color], ax

    continue_n:
    push word [draw_color]  ;COLOR  ;COLOR
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawBox
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    
    change_turtle_pos_n:
        dec word  [y_coord] ;decrease ycoord

        jmp draw_turtle_caller

handle_south:
    cmp byte [draw],0 ;check draw
    je not_draw_s
    jne draw_s
    not_draw_s:
    call make_color_black
    jmp continue_s

    draw_s:
    mov word ax, [s_color]
    mov word [draw_color], ax

    continue_s:
    push word [draw_color] 
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawBox
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    change_turtle_pos_s:
        inc word  [y_coord] ;increase y-coord

        jmp draw_turtle_caller
handle_east:
 
    cmp byte [draw],0 ;check draw
    je not_draw_e
    jne draw_e
    not_draw_e:
    call make_color_black
    jmp continue_e

    draw_e:
    mov word ax, [e_color]
    mov word [draw_color], ax

    continue_e:
    push word [draw_color]
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawBox
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    change_turtle_pos_e:
        inc word  [x_coord]

        jmp draw_turtle_caller
handle_west:
    ;check draw variable
    
    cmp byte [draw],0 ;check draw
    je not_draw_w
    jne draw_w
    not_draw_w:
    call make_color_black
    jmp continue_w

    draw_w:
    mov word ax, [w_color]
    mov word [draw_color], ax

    continue_w:
    push word [draw_color]
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawBox
    pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    
    dec word  [x_coord]
    jmp draw_turtle_caller

draw_turtle_caller:
    
    push word [n_color]  ;COLOR  ;COLOR
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawturtle
    jmp readNextChar
make_color_black:
    mov word [draw_color], 16
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