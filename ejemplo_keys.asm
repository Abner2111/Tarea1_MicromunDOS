; this is the last part of this tutorial. Every computer needs a keyboard. This one is PS/2 one
; This simple example reads from keyboard and types it to screen

; ------------
; Important magic values:
; 0x64 = keyboard status port. (in other words - does keyboard have input for me. If yes, second lowest bit (IBF flag) is set)
; 0x60 = port to read input from keyboard

; ------------
; to use this example
; 1) press run
; 2) click on keyboard image
; 3) press keys
; 4) your pressed keys will appear on the screen
; ------------
; That's all. If you ever get stuck, read the manual by pressing 'Help' button.
; Now create something awesome!
; ------------

xor ax, ax

mov ax, b800h
mov es, ax

readNextChar:
    waitForInputLoop:  ; Wait for IBF = 1
        ; Read Status byte
        in al, 64h
        
        ; Test IBF flag (Status<1>) = does keyboard have input ready for me?
        and al, 10b     
        
        jz     waitForInputLoop    
        
    ; read input to ax
    in al, 60h

    ; print ax
    push ax
    call printChar
    jmp readNextChar
    

printChar:
    ; store ax and bx values in stack
    push ax
    push bx
    
    ; read cursor position from memory
    mov bx, es:[-2]
    
    ; set color to green text + black background
    mov byte ptr es:[bx], 0xa
    
    ; get pushed ascii value from stack
    mov ax, [sp+6]
    
    ; and write it on screen
    mov byte ptr es:[bx+1], ax
    
    ; increase cursor position
    add bx, 2           
    
    ; store updated cursor to memory
    mov es:[-2], bx    
    
    ; restore ax and bx values from stack
    pop bx
    pop ax
    
    ; ret pops address from stack and jump to it
    ret 2
    