[bits 16]
section .data
    x_coord     dw      0
    y_coord     dw      0
    trtl_color  dw      40
    n_color     dw      13
    s_color     dw      14
    e_color     dw      15
    w_color     dw      10
    black_color dw      0
    last_dir    db      0
    draw        db      0
    draw_color  dw      16
    pixel_color dw      16


section .text


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
        call setScreenToBlack
        
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
        
        cmp byte [draw],0 ;check if draw flag is on
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


            push word [pixel_color]
            push word 0 ;X-POS
            push word 0  ;Y-POS
            call drawBox
            pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
            pop di
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

            jmp draw_turtle_caller

    handle_south:
        cmp byte [draw],0 ;check draw
        je not_draw_s
        jne draw_s
        not_draw_s:
        call make_color_black
        jmp continue_s

        draw_s:
        cmp byte [last_dir],0x48   ;check if last dir was south
        je back_to_north_color                   ;if last dir was south dont check color

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


            push word [pixel_color]
            push word 0 ;X-POS
            push word 0  ;Y-POS
            call drawBox
            pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
            pop di
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


            push word [pixel_color]
            push word 0 ;X-POS
            push word 0  ;Y-POS
            call drawBox
            pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
            pop di
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
        ;check draw variable
        
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


            push word [pixel_color]
            push word 0 ;X-POS
            push word 0  ;Y-POS
            call drawBox
            pop di;Recordar hacer pop, da igual el registro siempre que no este en uso
            pop di
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
        mov ax, [black_color]
        mov [draw_color], ax 
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