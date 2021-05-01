AStack SEGMENT STACK
    dw 256 DUP(?)   ; 1 килобайт
AStack ENDS

DATA SEGMENT

NEWLINE db 0dh, 0ah, '$'

CONTROL_BLOCK dw 0
              db 0
              db 0
              db 0
COMMAND_LINE  db 1h, 0dh

DESTROYED_ERROR db "The control memory block was destroyed!", 0dh, 0ah, '$'
NOT_ENOUGH_MEMORY_ERROR db "The memory is not enogh!", 0dh, 0ah, '$'
INVALID_ADDRESS_ERROR db "The memory block's address is invalid!", 0dh, 0ah, '$'
INVALID_FUNCTION_ERROR db "Invalid function!", 0dh, 0ah, '$'
FILE_NOT_FOUND_ERROR db "File not found!", 0dh, 0ah, '$'
DISK_ERROR db "Disk error!", 0dh, 0ah, '$'
INVALID_ENV_STRING_ERROR db "Invalid environment string!", 0dh, 0ah, '$'
INVALID_FORMAT_ERROR db "Invalid format!", 0dh, 0ah, '$'

OKAY_EXITING_MESSAGE db "The program executed okay!", 0dh, 0ah, '$'
CTRL_BREAK_ERROR db "The program exited with Ctrl+Break!", 0dh, 0ah, '$'
DEVICE_ERROR db "Device error occured!", 0dh, 0ah, '$'
RESIDENT_31_ERROR db "Resident 31h function exiting!", 0dh, 0ah, '$'

RETURN_VALUE db "The program returned: $"

PROGRAM_NAME db 64 dup(0), '$'

KEEP_SS dw 0
KEEP_SP dw 0

DATA_SEGMENT_END db 0
DATA ENDS


CODE SEGMENT
    ASSUME cs:CODE, ds:DATA, ss:AStack


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

    cmp dx, 0
    je write_string_exit

    push ax

    mov ah, 9h
    int 21h

    pop ax

write_string_exit:
    ret

WRITE_STRING endp



ALLOCATE_MEMORY proc near

    push bx
    push dx
    push ax

    mov ax, offset main_procedure_end
    mov bx, offset DATA_SEGMENT_END
    add ax, bx
    mov bx, 10h
    xor dx, dx
    div bx
    mov bx, ax
    add bx, dx
    add bx, 100h

    mov ah, 4ah
    int 21h

    xor dx, dx
    
    ; проверяем возможные ошибки
    cmp ax, 7
    je write_destroyed_error

    cmp ax, 8
    je not_enough_memory

    cmp ax, 9
    je invalid_address

    jmp allocate_exit

write_destroyed_error:
    mov dx, offset DESTROYED_ERROR
    jmp allocate_exit

not_enough_memory:
    mov dx, offset NOT_ENOUGH_MEMORY_ERROR
    jmp allocate_exit

invalid_address:
    mov dx, offset INVALID_ADDRESS_ERROR

allocate_exit:
    call WRITE_STRING

    pop ax
    pop dx
    pop bx

    ret

ALLOCATE_MEMORY endp



PARSE_PROGRAM_NAME proc near

    push ax
    push cx
    push es
    push di
    push bx
    
    mov ah, 62h
    int 21h

    ; в bx - адрес начала PSP
    mov es, bx
    xor cx, cx
    mov cl, es:[80h]
    dec cl
    
    mov bx, 1
    mov di, offset PROGRAM_NAME
    extracting_program_name:
        mov al, es:[81h + bx]
        mov [di], al
        
        inc bx
        inc di
        loop extracting_program_name

    pop bx
    pop di
    pop es
    pop cx
    pop ax

    ret

PARSE_PROGRAM_NAME endp


FIIL_CONTROL_BLOCK proc near

    push bx
    push dx

    ; оставляем первое слово равным 0
    mov bx, offset CONTROL_BLOCK
    mov dx, offset COMMAND_LINE
    mov [bx+2], dx
    mov [bx+4], ds

    pop dx
    pop bx

    ret

FIIL_CONTROL_BLOCK endp


LOAD_MODULE proc near

    push es
    push bx
    push ax
    push dx

    call FIIL_CONTROL_BLOCK

    mov ax, seg CONTROL_BLOCK
    mov es, ax
    mov bx, offset CONTROL_BLOCK

    mov ax, sp
    mov KEEP_SP, ax
    mov ax, ss
    mov KEEP_SS, ax

    mov dx, offset PROGRAM_NAME

    mov ax, 4b00h
    int 21h

    xor dx, dx
    ; если CF = 0
    jnc restore_everything

    call PRINT_NEWLINE
    xor dx, dx

    cmp ax, 1
    je invalid_function
    cmp ax, 2
    je file_not_found
    cmp ax, 5
    je disk_error_label
    cmp ax, 8
    je load_not_enough_memory
    cmp ax, 10
    je invalid_env_string
    cmp ax, 11
    je invalid_format

invalid_function:
    mov dx, offset INVALID_FUNCTION_ERROR
    jmp restore_everything

file_not_found:
    mov dx, offset FILE_NOT_FOUND_ERROR
    jmp restore_everything

disk_error_label:
    mov dx, offset DISK_ERROR
    jmp restore_everything

load_not_enough_memory:
    mov dx, offset NOT_ENOUGH_MEMORY_ERROR
    jmp restore_everything

invalid_env_string:
    mov dx, offset INVALID_ENV_STRING_ERROR
    jmp restore_everything

invalid_format:
    mov dx, offset INVALID_FORMAT_ERROR

restore_everything:
    call WRITE_STRING

    mov ax, KEEP_SP
    mov sp, ax
    mov ax, KEEP_SS
    mov ss, ax

    mov ah, 4dh
    int 21h

    mov cl, al

    xor dx, dx
    cmp ah, 0
    je okay_exiting_code
    cmp ah, 1
    je ctrl_break_code
    cmp ah, 2
    je device_error_code
    cmp ah, 3
    je resident_31_code

okay_exiting_code:
    mov dx, offset OKAY_EXITING_MESSAGE
    jmp load_exit

ctrl_break_code:
    mov dx, offset CTRL_BREAK_ERROR
    jmp load_exit

device_error_code:
    mov dx, offset DEVICE_ERROR
    jmp load_exit

resident_31_code:
    mov dx, offset RESIDENT_31_ERROR

load_exit:
    call PRINT_NEWLINE
    call WRITE_STRING
    
    mov dx, offset RETURN_VALUE
    call WRITE_STRING
    mov dl, cl
    mov ah, 02h
    int 21h
    call PRINT_NEWLINE

    pop dx
    pop ax
    pop bx
    pop es

    ret

LOAD_MODULE endp




Main proc far
    mov ax, DATA
    mov ds, ax

    call ALLOCATE_MEMORY
    call PARSE_PROGRAM_NAME
    call LOAD_MODULE

    ; выход в DOS
    xor al, al
    mov ah, 4ch
    int 21h

main_procedure_end:
Main endp

CODE  ENDS
END Main