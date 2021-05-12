CODE SEGMENT
	ASSUME CS:CODE, DS:NOTHING, SS:NOTHING

MAIN PROC FAR

	push ax
	push dx
	push ds
	push di
		
	mov ax, cs
	mov ds, ax
	mov di, offset overlay1_address
	add di, 22
	call WRD_TO_HEX
	mov dx, offset overlay1_address
	call WRITEWRD
		
	pop di
	pop ds
	pop dx
	pop ax
	
    retf

MAIN ENDP

overlay1_address db 'Overlay 1 address:      ', 0dh, 0ah, '$'
	
WRITEWRD  PROC  NEAR
    push ax
    mov ah, 9
    int 21h
    pop ax
    ret
WRITEWRD  ENDP


TETR_TO_HEX proc near
    and al, 0fh
    cmp al, 09
    jbe next
    add al, 07

next:
    add al, 30h
    ret

TETR_TO_HEX endp

BYTE_TO_HEX proc near

    push cx
    mov ah, al
    call TETR_TO_HEX
    xchg al, ah
    mov cl, 4
    shr al, cl
    call TETR_TO_HEX
    pop cx

    ret

BYTE_TO_HEX endp

WRD_TO_HEX proc near

    push bx
    mov bh, ah
    call BYTE_TO_HEX
    mov [di], ah
    dec di
    mov [di], al
    dec di
    mov al, bh
    call BYTE_TO_HEX
    mov [di], ah
    dec di
    mov [di], al
    pop bx

    ret
    
WRD_TO_HEX endp

CODE ENDS
END MAIN