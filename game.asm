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
    seconds     db      0
    totaltime   dw      0

section .text
    global _start

_start:
    xor ax, ax      ;setting ax to 0
    mov ah, 0       ;Set display mode
    mov al, 13h     ;13h = 320x200, 256 colors
    int  0x10       ;Video BIOS Services
    
    ;test
    mov bx, 255*7;
    mov [seconds], bx;
    mov [totaltime],bx;
    ;test
    
    push 11         ;COLOR
    push word [x_coord]  ;X-POS
    push word [y_coord]  ;Y-POS
    call drawturtle
    pop di          ;Recordar hacer pop, da igual el registro siempre que no este en uso
    pop di
    pop di
    
readNextChar:

    waitForInputLoop:
            
	call handler_Timer
            
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
;chat gpt code

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


;end chat gpt code



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

handler_Timer:
	pusha
	mov ah, 02h
	int 1ah
	mov bx,[seconds]
	cmp dh, [seconds]
	je handler_Timer_Not
	
	mov [seconds], dh
	
	mov ax, [totaltime]
	add ax, 1
	mov [totaltime],ax
	
	jmp handler_Timer_Print
	
	
handler_Timer_Not:
	popa
	ret

handler_Timer_Print:
	mov dx, 0; 
	mov bx,0 ;tamaño
	mov cx, 10; divisor
	
	count_time_size:
		div cx
		mov dx,0
		add bx,1
		cmp ax,0
		je print_numbers
		jmp count_time_size
	
	print_numbers:
		mov ax, [totaltime]
		
		print_numbers_loop:
			push bx;
			push 1;
			div cx;
			
			call drawMiniBox;

			call draw0;
			
			call draw1;
			
			call draw2;
			
			call draw3;
			
			call draw4;
			
			call draw5;
			
			call draw6;
			
			call draw7;
			
			call draw8;
			
			call draw9;
			
			pop bx;
			pop bx;
			cmp bx, 0;
			je end_print_loop;
			mov dx,0;
			add bx, -1;
			jmp print_numbers_loop;
	
	
	end_print_loop:
		popa
		ret

end:
	jmp $
	nop
times (3*512)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size
