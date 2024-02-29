BITS 16
section .text

global _start

;org 0x7c00
_start:

; this code is complex (and slow), please turn off animation in Settings

; CONTROLS:
; click run button, then click on keyboard and use arrow keys to change snake direction

; HOW TO PLAY
; You are snake, to grow, collect (red) apples.
; try not to crash to walls and yourself!

xor ax, ax

mov ax, 0xb800
mov es, ax

; variables
mov byte  ds:[0], 20  ; row
mov byte  ds:[1], 4   ; col
mov byte  ds:[2], 39  ; direction
mov word  ds:[4], 450  ; apple address
mov word  ds:[6], 80h  ; start address
mov word  ds:[8], 80h  ; head address

call startupDrawRectangle
call generateApple

call startupGrowSnake
jmp readNextChar

startupDrawRectangle:
    push cx
    push bx
    
    ; draw top line
    mov cx, 80
    mov bx, 0
    startupDrawRectangle_topLine:
        mov byte  es:[bx], 0x99
        mov byte  es:[bx+1], 's'
        
        add bx, 2
        dec cx
        jnz startupDrawRectangle_topLine
        
    ; draw bottom line
    mov cx, 80
    mov bx, 3840
    startupDrawRectangle_bottomLine:
        mov byte  es:[bx], 0x99
        mov byte  es:[bx+1], 's'
        
        add bx, 2
        dec cx
        jnz startupDrawRectangle_bottomLine
        
            
    ; draw left line
    mov cx, 25
    mov bx, 0
    startupDrawRectangle_leftLine:
        mov byte  es:[bx], 0x99
        mov byte  es:[bx+1], 's'
        
        add bx, 160
        dec cx
        jnz startupDrawRectangle_leftLine
    
    ; draw right line
    mov cx, 25
    mov bx, 158
    startupDrawRectangle_rightLine:
        mov byte  es:[bx], 0x99
        mov byte  es:[bx+1], 's'
        
        add bx, 160
        dec cx
        jnz startupDrawRectangle_rightLine
        
    pop bx
    pop cx
    
    ret 1
    
startupGrowSnake:
    mov cx, 3   ; grow 3 times

    startupgrowSnake_again:
        add byte  ds:[0], 1
        call saveCursorPosition

        dec cx
        jnz startupgrowSnake_again
    
    ret 1

readNextChar:
    waitForInputLoop:  ; Wait for IBF = 1
        nop     ; rerender screen
        ; Read Status byte
        in al, 64h
        
        ; Test IBF flag (Status<1>) = does keyboard have input ready for me?
        and al, 10b     
        
        jz noInput;
        
        
    xor ax, ax
    noInput:
        
        
    ; arrow up?
    cmp byte  ds:[2], 38
    jnz notArrowUp
        ; out of bounds?
        cmp byte  ds:[1], 0
        jz gameOver
        
        sub byte  ds:[1], 1
    notArrowUp:
    
    ; arrow down?
    cmp byte  ds:[2], 40
    jnz notArrowDown
        cmp byte  ds:[1], 24 ; 25 rows on screen
        jz gameOver
        
        add byte  ds:[1], 1
    notArrowDown:

    ; arrow left?
    cmp byte  ds:[2], 37
    jnz notArrowLeft
        cmp byte  ds:[0], 0
        jz gameOver
        
        sub byte  ds:[0], 1
    notArrowLeft:
    
    ; arrow right?
    cmp byte  ds:[2], 39
    jnz notArrowRight
        cmp byte  ds:[0], 79 ; 80 cols on screen
        jz gameOver
        
        add byte  ds:[0], 1
    notArrowRight:
    
    ; if colision, end the game
    call getCharToAl
    cmp al, 's'
    jz gameOver
    

    
    
    
    call getCharToAl
    cmp al, 'a' ; is apple at the next cursor position?
    jz dontCutTail
        call saveCursorPosition
        call cutTail
        jmp readNextChar
       
        
dontCutTail:
    call saveCursorPosition
    call generateApple
    jmp readNextChar
    

    
    
    
    
saveCursorPosition:
    push bx
    
    ; print to screen
    push 's'
    call printChar
    
    ; save position
    call getCursorPositionToBx
    mov ax, bx      ; ax = head address
    mov bx, ds:[8]  ; head
    mov [bx], ax
    add word  ds:[8], 2
    pop bx
    ret 1
    
cutTail:
    push ax
    push bx
    push cx
    push dx
    
    mov bx, ds:[6]  ; tail block to remove
    mov cx, ds:[bx]
    
    mov bx, ds:[6]  ; counter, start at start
    
    ; loop to move all addresses by one
    cutTail_moveNextAddress:
    cmp ds:[8], bx  ; are we at the end?
    jz cutTail_end
        mov ax, ds:[bx+2]
        mov ds:[bx], ax
        add bx, 2
        
        jmp cutTail_moveNextAddress
        
    
    
    cutTail_end:
    
    mov bx, cx

    ; delete
    mov byte  es:[bx], 0x0
    
    ; and write it on screen
    mov byte  es:[bx+1], 0
    
    sub word  ds:[8], 2
    pop dx
    pop cx
    pop bx
    pop ax
    ret 1
    
; read cursor position from memory to BX
getCursorPositionToBx:
    push ax
    xor bx, bx

    ; row
    mov ax,  ds:[0]
    mov bx, 2
    mul bx
    mov bx, ax

    ; col
    mov ax,  ds:[1]
    mov bx, 160
    mul bx
    add bx, ax
    
    pop ax
    ret 1


generateApple_fixOutOfBounds:
    sub bx, 2000
    jmp generateApple_fixedBounds

generateApple:
    push bx
    
    mov bx, ds:[4]
    
    generateApple_tryNextPlace:
        add bx, 558
        
        cmp bx, 2000    ;80 cols * 25 rows
        jns generateApple_fixOutOfBounds
        
        generateApple_fixedBounds:
    
        mov al, es:[bx+1]   ; character at position we want to generate apple
        cmp al, 0
        je generateApple_foundEmptyPlace
        jmp generateApple_tryNextPlace

    generateApple_foundEmptyPlace:
    ; color 
    mov byte  es:[bx], 0x44
    
    ; and write it on screen
    mov byte  es:[bx+1], 'a'
    
    mov ds:[4], bx  ; save generated apple address
    pop bx
    ret 1


; returns char at cursor position to AL
getCharToAl:
    push bx
    call getCursorPositionToBx
    mov al, es:[bx+1]
    pop bx
    ret 1
    


printChar:
    ; store ax and bx values in stack
    push ax
    push bx
    push cx
    push dx

    call getCursorPositionToBx
    
    ; set color to green text + black background
    mov byte  es:[bx], 0xbb
    
    push bx
    ; get pushed ascii value from stack
    mov bx ,sp
    mov dx, [bx+10]
    pop bx
    
    ; and write it on screen
    mov byte  es:[bx+1], dl
    
    ; restore ax and bx values from stack
    pop dx
    pop cx
    pop bx
    pop ax
    
    ; ret pops address from stack and jump to it
    ret 2
    
    
getRandomNumberToAx:
    
    
printString:
    push ax
    push bx
    push dx
    mov bx, sp

    mov dx, [bx+8]  ; read string offset from stack
    mov bx,0
    printString_readChar:
        add bx, dx
        mov al, ds:[bx]   ; char
        mov ah, ds:[bx+1] ; color
        sub bx, dx
        
        cmp al, 0
        jz printString_endOfString
        
        mov byte  es:[bx+70], ah
        mov byte  es:[bx+71], al
        
        add bx, 2
        
        jmp printString_readChar
        
    printString_endOfString:
    pop dx
    pop bx
    pop ax
    ret 1
        
        
    
    
gameOver:
    push 0x60
    call printString

    