; do addition
org 0x7c00
bits 16
mov ax, 4
mov bx, 1
add bx, ax
mov al, bl; print the number
mov ah, 0x0e
add al, 48
int 0x10

xor ax, ax

mov ax, 0xb800
mov es, ax
mov byte  ds:[0], 20  ; row
mov byte  ds:[1], 4   ; col
mov byte  ds:[2], 39  ; direction
mov word  ds:[4], 450  ; apple address
mov word  ds:[6], 80h  ; start address
mov word  ds:[8], 80h  ; head address

call startupDrawRectangle

startupDrawRectangle:
    push cx
    push bx
    
    ; draw top line
    mov cx, 80
    mov bx, 0
    startupDrawRectangle_topLine:
        mov byte  es:[bx], 0x00
        mov byte  es:[bx+1], 255
        
        add bx, 2
        dec cx
        jnz startupDrawRectangle_topLine
        
    ; draw bottom line
    mov cx, 80
    mov bx, 3840
    startupDrawRectangle_bottomLine:
        mov byte  es:[bx], 0x00
        mov byte  es:[bx+1], 255
        
        add bx, 2
        dec cx
        jnz startupDrawRectangle_bottomLine
        
            
    ; draw left line
    mov cx, 25
    mov bx, 0
    startupDrawRectangle_leftLine:
        mov byte  es:[bx], 0x00
        mov byte  es:[bx+1], 255
        
        add bx, 160
        dec cx
        jnz startupDrawRectangle_leftLine
    
    ; draw right line
    mov cx, 25
    mov bx, 158
    startupDrawRectangle_rightLine:
        mov byte  es:[bx], 0x00
        mov byte  es:[bx+1], 255
        
        add bx, 160
        dec cx
        jnz startupDrawRectangle_rightLine
        
    pop bx
    pop cx
    
    ret 1

times 510 - ($-$$) db 0		
dw 0xAA55