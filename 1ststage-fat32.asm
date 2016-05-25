; FAT32 bootloader for PC-compatibles.

; Minimum requirement: An i386 or compatible CPU.
; Limitations: To be determined. (512-byte sectors, no lfn)
; Bugs: To be determined.

bits 16
cpu 8086
org 0

; Choose the amount of RAM to reserve for the stack: 2 or 4 kB
%define RAMSIZE 2

fakestart:
    jmp start
    nop
    
    
                db 'ERROR!!!'
                dw 512
bpb_spercluster:db 2
bpb_sreserved:  dw 6270
bpb_fats:       db 2
                dw 0
                dw 0
                db 0xF8
                dw 0
                dw 63
                dw 255
bpb_startlba:   dd 0
                dd 253952
                dd 961
                dw 0x0000
                dw 0x0000
                dd 2
                dw 1
                dw 6
                times 12 db 0x00
bpb_drivenumber:db 0x80
                db 0x00
                db 0x29
                dd 0x12345678
                db '1STSTAGEVBR'
                db 'FAT32   '


start:
    cli
    int 0x12 ; find out how much of the "640k" is actually open
%if RAMSIZE == 4
    mov cl, 6
    shl ax, cl
    dec ah ; reserve 4k
%else
    dec ax ; reserve 2k
    dec ax ; of the space reserved, 1k is used by this bootloader
    mov cl, 6
    shl ax, cl
%endif
    mov es, ax
    mov ss, ax
%if RAMSIZE == 4
    mov sp, 0x1000 ; stack at 4k
%else
    mov sp, 0x0800 ; stack at 2k
%endif
    cld
    mov si, 0x7c00
    xor di, di
    mov ds, di
    mov cx, 0x100
    rep movsw
    ;mov ds, ax
    sti
    mov cl, highstart - fakestart
    push es
    push cx
    retf
highstart:
    mov si, msg_cpu
    xor ax, ax
    pushf
    push ax
    popf
    pushf
    pop ax
    popf
    and ah, 0xf0
    cmp ah, 0xf0
    je show_error
    cpu 286
    pushf
    push word 0x7000
    popf
    pushf
    pop ax
    popf
    and ah, 0x70
    jz show_error
    cpu 386
    mov [cs:bpb_drivenumber], dl
    mov si, msg_geometry
    int 0x13
    jc show_error
    
    mov si, msg_temp
    cpu 8086
show_error:
    mov bx, 0x0007
.loop:
    mov ah, 0x0e
    cs lodsb
    or al, al
    jz infinity
    int 0x10
    jmp .loop
infinity:
    hlt
    jmp infinity
    
msg_cpu:
    db "An i386 CPU is required.",0
msg_geometry:
    db "BIOS geometry error",0
msg_temp:
    db "No errors",0
    
times 0x1fe-($-$$) db 0
dw 0xaa55
times 0x3fe-($-$$) db 0
dw 0xaa55
