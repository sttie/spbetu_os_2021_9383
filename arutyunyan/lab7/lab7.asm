ASTACK SEGMENT STACK
   DW 256 DUP(?)
ASTACK ENDS

DATA SEGMENT

NEWLINE db 0dh, 0ah, '$'

DTA db 43 DUP(0)

OVERLAY_ADDRESS dd 0
OVERLAY_NAME1 db 'overlay1.ovl', 0h
OVERLAY_NAME2 db 'overlay2.ovl', 0h
OVERLAY_PATH db 128 DUP(0)

KEEP_SS dw 0
KEEP_SP dw 0

ERROR_MEM_FREE db 0
MCB_ERROR_STRING db 'Memory Free Error: Memory Control Block has crashed', 0DH, 0AH, '$'
OUT_OF_MEM_ERROR_STR db 'Memory Free Error: Not Enough Memory', 0DH, 0AH, '$'
WRONG_ADDRESS_ERROR_STRING db 'Memory Free Error: Wrong Address', 0DH, 0AH, '$'
FREE_OK_MESSAGE db 'Freed successfully', 0DH, 0AH, '$'

FILE_NOT_FOUND_ERROR_STRING db 'Allocation error: File not found', 0DH, 0AH, '$'
ROUTE_NOT_FOUND_ERROR_STRING db 'Overlay Allocation Error: Route not found', 0DH, 0AH, '$'
ALLOCATED_MEM_FOR_OVERLAY_MESSAGE db 'Allocated memory for overlay successfully', 0DH, 0AH, '$'

OVERLAY_FUNCTION_NOT_EXIST_ERROR db 'Function does not exist', 0DH, 0AH, '$'
OVERLAY_FILE_NOT_FOUND_ERROR db 'File not found', 0DH, 0AH, '$'
OVERLAY_ROUTE_NOT_FOUND db 'Route not found', 0DH, 0AH, '$'
OVERLAY_TOO_MANY_FILES_OPENED_ERROR db 'Too many files opened', 0DH, 0AH, '$'
OVERLAY_NO_ACCESS_ERROR db 'No access', 0DH, 0AH, '$'
OVERLAY_NOT_ENOUGH_MEMORY_ERROR db 'Not enough memory', 0DH, 0AH, '$'
OVERLAY_WRONG_ENV_ERROR db 'Wrong environment', 0DH, 0AH, '$'
OVERLAY_LOAD_SUCCESS_ERROR db 'Overlay loaded successfully', 0DH, 0AH, '$'

DATA_END db 0

DATA ENDS

CODE SEGMENT
    ASSUME CS:CODE, DS:DATA, SS:ASTACK


PRINT_NEWLINE proc near

    push ax
    push dx

    mov dx, offset NEWLINE
    mov ah, 9h
    int 21h

    pop dx
    pop ax

    ret

PRINT_NEWLINE endp

WRITE_STRING proc near

    push ax

    mov ah, 9h
    int 21h

    pop ax
    ret

WRITE_STRING endp

FREE_UNUSED_MEMORY PROC FAR
    push ax
    push bx
    push cx
    push dx
    push es

    xor dx, dx

    mov ERROR_MEM_FREE, 0h

    mov ax, offset DATA_END
    mov bx, offset main_proc_end_byte
    add ax, bx
        
    mov bx, 10h
    div bx
    add ax, 100h
    mov bx, ax

    xor ax, ax

    mov ah, 4ah
    int 21h 

    jnc free_without_error

	mov ERROR_MEM_FREE, 1h

    cmp ax, 7
    jne not_enough_memory

    mov dx, offset MCB_ERROR_STRING
    call WRITE_STRING
    jmp free_unused_memory_end

not_enough_memory:
    cmp ax, 8
    jne wrong_address

    mov dx, offset OUT_OF_MEM_ERROR_STR
    call WRITE_STRING
    jmp free_unused_memory_end	

wrong_address:
    cmp ax, 9
    jne free_unused_memory_end

    mov dx, offset WRONG_ADDRESS_ERROR_STRING
    call WRITE_STRING
    jmp free_unused_memory_end

free_without_error:
    mov dx, offset FREE_OK_MESSAGE
    call WRITE_STRING
        
free_unused_memory_end:
    pop es
    pop dx
    pop cx
    pop bx
    pop ax

    ret

FREE_UNUSED_MEMORY ENDP
        

LOAD_OVERLAY PROC FAR
    push ax
    push bx
    push cx
    push dx
    push es

    push ds
    push es
    mov KEEP_SP, sp
    mov KEEP_SS, ss

    mov ax, data
    mov es, ax

    mov bx, offset OVERLAY_ADDRESS

    mov dx, offset OVERLAY_PATH
        
    mov ax, 4b03h 
    int 21h 
        
    mov ss, KEEP_SS
    mov sp, KEEP_SP
    pop es
    pop ds

    jnc loaded_successfully

    ;function does not exist error
    cmp ax, 1
	jne load_file_not_found
	mov dx, offset OVERLAY_FUNCTION_NOT_EXIST_ERROR
	call WRITE_STRING
	jmp load_module_end
    
load_file_not_found:
    cmp ax, 2
	jne load_route_error
	mov dx, offset OVERLAY_FILE_NOT_FOUND_ERROR
	call WRITE_STRING
	jmp load_module_end

load_route_error:
    cmp ax, 3
	jne load_too_many_files_opened
	mov dx, offset OVERLAY_ROUTE_NOT_FOUND
	call WRITE_STRING
	jmp load_module_end

load_too_many_files_opened:
    cmp ax, 4
	jne load_no_access_error
	mov dx, offset OVERLAY_TOO_MANY_FILES_OPENED_ERROR
	call WRITE_STRING
	jmp load_module_end

load_no_access_error:
    cmp ax, 5
	jne load_not_enough_memory
	mov dx, offset OVERLAY_NO_ACCESS_ERROR
	call WRITE_STRING
	jmp load_module_end

load_not_enough_memory:
    cmp ax, 8
	jne load_wrong_env
	mov dx, offset OVERLAY_NOT_ENOUGH_MEMORY_ERROR
	call WRITE_STRING
	jmp load_module_end

load_wrong_env:
    cmp ax, 10
	jne load_module_end
	mov dx, offset OVERLAY_WRONG_ENV_ERROR
	call WRITE_STRING
	jmp load_module_end

loaded_successfully:
    mov bx, offset OVERLAY_ADDRESS

    mov ax, [bx]
    mov cx, [bx + 2]
    mov [bx], cx
    mov [bx + 2], ax

    call OVERLAY_ADDRESS

    mov es, ax
    mov ah, 49h
    int 21h

load_module_end:
    pop es
    pop dx
    pop cx
    pop bx
    pop ax

    ret

LOAD_OVERLAY ENDP

GET_PATH PROC NEAR ;name in si

    push ax
    push dx
    push es
    push di

    xor di, di
    mov ax, es:[2ch]
    mov es, ax

content_loop:
    mov dl, es:[di]
    cmp dl, 0
    je end_string2

    inc di
    jmp content_loop

end_string2:
    inc di

    mov dl, es:[di]
    cmp dl, 0
    jne content_loop

    call PARSE_PATH

    pop di
    pop es
    pop dx
    pop ax

    ret

GET_PATH ENDP


PARSE_PATH PROC NEAR

    push ax
    push bx
    push bp
    push dx
    push es
    push di

    mov bx, offset OVERLAY_PATH

    add di, 3

boot_loop:
    mov dl, es:[di]
    mov [bx], dl
    cmp dl, '.'
    je parse_to_slash

    inc di
    inc bx

    jmp boot_loop

parse_to_slash:
    mov dl, [bx]
    cmp dl, '\'
    je get_overlay_name
    mov dl, 0h
    mov [bx], dl

    dec bx
    jmp parse_to_slash
    
get_overlay_name:
    mov di, si ; si - overlay_name
    inc bx

add_overlay_name:
    mov dl, [di]
    cmp dl, 0h
    je parse_path_end

    mov [bx], dl

    inc bx
    inc di

    jmp add_overlay_name

parse_path_end:
    mov [bx], dl

    pop di
    pop es
    pop dx
    pop bp
    pop bx
    pop ax

    ret

PARSE_PATH ENDP


ALLOCATE_FOR_OVERLAY PROC FAR

    push ax
    push bx
    push cx
    push dx
    push di

    mov dx, offset DTA
    mov ah, 1ah
    int 21h
        
    mov dx, offset OVERLAY_PATH
    mov cx, 0
    mov ah, 4eh
    int 21h

    jnc got_size_succesfully

    ;file not found error
    cmp ax, 12h
    jne route_error
    mov dx, offset FILE_NOT_FOUND_ERROR_STRING
    call WRITE_STRING
    jmp allocate_for_overlay_end

route_error:
    cmp ax, 3
    jne allocate_for_overlay_end
    mov dx, offset ROUTE_NOT_FOUND_ERROR_STRING
    call WRITE_STRING
    jmp allocate_for_overlay_end

got_size_succesfully:
    mov di, offset DTA
    mov dx, [di + 1ch]
    mov ax, [di + 1ah]

    mov bx, 10h
    div bx
    add ax, 1h

    mov bx, ax

    mov ah, 48h
    int 21h

    mov bx, offset OVERLAY_ADDRESS
    mov cx, 0000h
    mov [bx], ax
    mov [bx + 2], cx

allocate_for_overlay_end:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ALLOCATE_FOR_OVERLAY ENDP


MAIN PROC FAR
    mov ax, data
    mov ds, ax

    call FREE_UNUSED_MEMORY

    cmp ERROR_MEM_FREE, 0h
    jne main_end

    call PRINT_NEWLINE

    mov si, offset OVERLAY_NAME1
    call GET_PATH
    call ALLOCATE_FOR_OVERLAY

    call LOAD_OVERLAY

    call PRINT_NEWLINE

    mov si, offset OVERLAY_NAME2
    call GET_PATH
    call ALLOCATE_FOR_OVERLAY

    call LOAD_OVERLAY

main_end:
    xor al, al
    mov ah, 4ch
    int 21h

MAIN ENDP

main_proc_end_byte:
CODE ENDS

END MAIN