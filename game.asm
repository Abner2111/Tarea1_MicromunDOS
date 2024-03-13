[bits 16]


section .data
	draw        db      0
    x_coord     dw      27
    y_coord     dw      17
    trtl_color  dw      40
	black_color dw      0
    last_dir    dw      0
    draw_color  dw      0
	pixel_color dw      0
    seconds     db      1
    totaltime   db      1
	paddingColor   dw		0
	n_color     dw      13
    s_color     dw      14
    e_color     dw      15
    w_color     dw      10

section .text
_start:
    xor ax, ax      ;setting ax to 0
    mov ah, 0       ;Set display mode
    mov al, 13h     ;13h = 320x200, 256 colors
    int  0x10       ;Video BIOS Services
    mov word [n_color], 13
	mov word [n_color], 13
	mov word [s_color], 14
	mov word [e_color], 15
	mov word [w_color], 10
    ;set timer
    mov ax, 50
    mov [totaltime],ax
    
	call setScreenToBlack
    ;draw menu
    push 25
    mov si, MenuCommand
    call draw_Images
    pop di
    
    
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
    
    push 10
    mov si, UpCommand
    call draw_Images
    pop di
    
    cmp byte [draw],0 ;check draw
    je not_draw_n
    jne draw_n
    not_draw_n:
	call make_color_black
	jmp continue_n

    
    draw_n:

        cmp byte [last_dir],0x50   ;check if last dir was south
        je back_to_north_color                   ;if last dir was south dont check color
        

        check_north_color:

            ;CHECKING PIXEL COLOR
            mov word ax, [y_coord] ;moving y_cord to ax
            add word ax, -1 ;the next y coord place where the turtle will be
            mov word cx, [x_coord] ;mov next to x_coord
            mov word dx, ax
            add word cx, 1 ;offset
            add word dx, 1 ;offset
            ;scaling to turtle size
            mov ax, 6
            imul cx, ax
            imul dx, ax

            call getPixelColor
            
            mov byte [pixel_color], al

            cmp byte [black_color], al     ;si el color es negro, continuar a dibujar en color norte
            je back_to_north_color
            cmp byte [n_color],al
            je back_to_north_color
            cmp byte [s_color],al
            je back_to_north_color
			

	push 0ah; animacion de ganar
	call drawScreen_Progressive
	pop di
        
        back_to_north_color:
        mov word ax, [n_color]
        mov word [draw_color], ax

        continue_n:
        push word [draw_color]  ;COLOR  ;COLOR
        push word [x_coord] ;X-POS
        push word [y_coord]  ;Y-POS
        call drawBox
        pop di
        pop di
        pop di
        
        change_turtle_pos_n:
            dec word  [y_coord] ;decrease ycoord
			mov byte [last_dir],0x48
            jmp draw_turtle_caller
handle_south:
    push 10;write command
    mov si, DownCommand
    call draw_Images
    pop di
    
    cmp byte [draw],0 ;check draw
        je not_draw_s
        jne draw_s
        not_draw_s:
        call make_color_black
        jmp continue_s

        draw_s:
        cmp byte [last_dir],0x48   ;check if last dir was south
        je back_to_south_color                   ;if last dir was south dont check color

        check_south_color:

            ;CHECKING PIXEL COLOR
            mov ax, [y_coord] ;moving y_cord to ax
            add word ax, 1 ;the next y coord place where the turtle will be
            mov word cx, [x_coord] ;mov next to x_coord
            mov word dx, ax
            add word cx, 1
            add word dx, 1
            ;scaling to turtle size
            mov ax, 6
            imul cx, ax
            imul dx, ax

            call getPixelColor
            
            mov byte [pixel_color], al

            cmp byte [black_color], al     ;si el color es negro, continuar a dibujar en color norte
            je back_to_south_color
            cmp byte [n_color],al
            je back_to_south_color
            cmp byte [s_color],al
            je back_to_south_color


	push 0ah; animacion de ganar
	call drawScreen_Progressive
	pop di
	
        back_to_south_color:
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
            mov byte [last_dir],0x50
            jmp draw_turtle_caller
handle_east:

    push 10
    mov si, RigthCommand
    call draw_Images
    pop di
 
    cmp byte [draw],0 ;check draw
        je not_draw_e
        jne draw_e
        not_draw_e:
        call make_color_black
        jmp continue_e

        draw_e:
        cmp byte [last_dir],0x4b   ;check if last dir was south
        je back_to_east_color                   ;if last dir was south dont check color

        check_east_color:

            ;CHECKING PIXEL COLOR
            mov ax, [y_coord] ;moving y_cord to ax
            
            mov word cx, [x_coord] ;mov next to x_coord
            add word cx, 1 ;the next y coord place where the turtle will be
            mov word dx, ax
            add word cx, 1 ;offset
            add word dx, 1 ;offset
            ;scaling to turtle size
            mov ax, 6
            imul cx, ax
            imul dx, ax

            call getPixelColor
            
            mov byte [pixel_color], al

            cmp byte [black_color], al     ;si el color es negro, continuar a dibujar en color norte
            je back_to_east_color
            cmp byte [e_color],al
            je back_to_east_color
            cmp byte [w_color],al
            je back_to_east_color


	push 0ah; animacion de ganar
	call drawScreen_Progressive
	pop di
	
        back_to_east_color:
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
            mov byte [last_dir],0x4d
            jmp draw_turtle_caller
handle_west:
	
    push 10; draw command
    mov si, LeftCommand
    call draw_Images
    pop di
    
    cmp byte [draw],0 ;check draw
        je not_draw_w
        jne draw_w
        not_draw_w:
        call make_color_black
        jmp continue_w

        draw_w:
        cmp byte [last_dir],0x4d   ;check if last dir was south
        je back_to_west_color                   ;if last dir was south dont check color

        check_west_color:

            ;CHECKING PIXEL COLOR
            mov word ax, [y_coord] ;moving y_cord to ax

            mov word cx, [x_coord] ;mov next to x_coord
            ;add word cx, -1 ;the next y coord place where the turtle will be
            mov word dx, ax
            add word dx, 1
            ;scaling to turtle size
            mov ax, 6
            imul cx, ax
            imul dx, ax
        
            call getPixelColor
            
            mov byte [pixel_color], al

            ;check valid colission
            cmp byte [black_color], al 
            je back_to_west_color
            cmp byte [e_color],al
            je back_to_west_color
            cmp byte [w_color],al
            je back_to_west_color


	push 0ah; animacion de ganar
	call drawScreen_Progressive
	pop di
	
        back_to_west_color:
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
        mov byte [last_dir], 0x4b
        jmp draw_turtle_caller
draw_turtle_caller:
    
    push word [n_color]  ;COLOR  ;COLOR
    push word [x_coord] ;X-POS
    push word [y_coord]  ;Y-POS
    call drawturtle
    jmp readNextChar
make_color_black:
    mov word [draw_color], 0
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
	add ax, -1
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
		mov ax, [totaltime]
		cmp ax, 0
		je print_loose
		popa
		ret
	print_loose:
		push 4
		call drawScreen_Progressive
		pop di
		popa
		ret
getPixelColor:
        ; Inputs:
        ;   CX = x-coordinate
        ;   DX = y-coordinate
        ; Calculate the offset in video memory
        mov ax, 320      ; Width of the screen in pixels
        mul dx           ; Multiply y-coordinate by screen width
        add ax, cx       ; Add x-coordinate to get the offset
        mov di, ax       ; DI register now holds the offset
        ; Video memory address in Mode 13h is 0xA0000
        mov ax, 0xA000
        mov es, ax  ; Set ES segment register to video memory segment
        ; Read the pixel color at (x, y)
        mov ax, [es:di]  ; AL register now holds the color value
        ret
setScreenToBlack:
        ; Video memory address in Mode 13h is 0xA0000
        mov ax, 0xA000
        mov es, ax  ; Set ES segment register to video memory segment
        ; Calculate the total number of pixels on the screen (320x200)
        mov cx, 320      ; Width of the screen in pixels
        mov dx, 200      ; Height of the screen in pixels
        mul cx           ; Multiply width by height to get total pixels
        ; Fill video memory with white color index (0xFF)
        mov di, 0        ; DI register is the offset in video memory
        mov al, [black_color]     ; Color index for white
    fillLoop:
        ; Write the white color index to video memory
        mov [es:di], al
        ; Move to the next pixel
        inc di
        ; Check if all pixels have been processed
        loop fillLoop
        ret
end:
	jmp $
	nop
%include "DrawFunctions.asm";
times (8*512)-($-$$) db 0 ;kernel must have size multiple of 512 so let's pad it to the correct size
