; FAT32 bootloader for PC-compatibles.

; Minimum requirement: An i386 or compatible CPU.
; Limitations: To be determined. (512-byte sectors, no lfn)
; Bugs: To be determined.

bits 16
cpu 8086
org 0

; Choose the amount of RAM to reserve for the stack: 2 or 4 kB
%define RAMSIZE 2
; Determine where the second half of the boot code is stored. DOS uses 2, NT
;   uses 12. Must be a value from 1 to 31.
%define SECOND_HALF 2

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
bpb_sectors:    dw 63
bpb_heads:      dw 255
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
    mov ds, ax
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
    mov [bpb_drivenumber], dl
    mov ah, 0x08
    int 0x13
    mov si, msg_geometry
    jc show_error
    movzx dx, dh
    inc dx
    mov [bpb_heads], dx
    and cx, 0x3f
    mov [bpb_sectors], cx
    mov ax, cs
    add ax, 0x20
    mov es, ax
    mov eax, SECOND_HALF
    cdq
    call readsector
    
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
    
    cpu 386
    
readsector:
    ; edx:eax - sector to read
    ; es:0 - destination address
    ; trashes everything
    push cs
    pop ds
    mov bp, sp
    add eax, [bpb_startlba]
    adc edx, 0
    push edx        ; bp-4: lba [63 .. 32]
    push eax        ; bp-8: lba [31 ... 0]
    push es         ; bp-10: destination segment
    push dword 1    ; bp-14: blocks to transfer
    push word 16    ; bp-16: size of packet
    jnz .lba
    mov cx, [bpb_sectors]; cx = SPT
    mov bx, [bpb_heads]; bx = HPC
    mov di, bx
    imul di, cx     ; di = HPC*SPT
    mov dx, [bp-6]
    cmp dx, di
    jae .lba
    div di      ; dx = C
    xchg ax, dx ; al = H
    div cl      ; ah = S-1
    mov cl, 2
    xchg ch, dl ; ch = low bits of C
    shr dx, cl
    xchg ah, cl ; cl = S-1
    inc cx      ; cl = S
    or cl, dl   ; cl = S | high bits of C
    xchg al, dh ; dh = H
    test al, al
    jz .int13
.lba:
    mov ax, 0x4200
.int13:
    inc ax
    les bx, [bp-12]
    mov dl, [bpb_drivenumber]
    int 0x13
    jc .error
    mov sp, bp
    ret
.error:
    xor si, si
    mov cx, 8
.outerloop:
    mov bx, cx
    mov cx, 2
    mov al, [bp+si-8]
    inc si
    mov dl, al
.innerloop:
    and al, 0x0f
    cmp al, 0x0a
    sbb al, 0x69
    das
    mov ah, 0x0e
    push ax
    mov al, dl
    shr al, 4
    loop .innerloop
    mov cx, bx
    loop .outerloop
    mov cx, 16
    mov bx, 0x0007
.poploop:
    pop ax
    int 0x10
    loop .poploop
    mov si, msg_read
    jmp show_error
    
msg_cpu:
    db "An i386 CPU is required.",0
msg_geometry:
    db "BIOS geometry error",0
msg_read:
    db " - Read error",0
    
times 0x1fe-($-$$) db 0
dw 0xaa55

msg_temp:
    db "No errors",0

times 0x3fe-($-$$) db 0
dw 0xaa55
