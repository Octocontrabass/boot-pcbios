; FAT32 bootloader for PC-compatibles.

; Minimum requirement: An i386 or compatible CPU.
; Limitations: To be determined. (512-byte sectors, no lfn)
; Bugs: To be determined.

bits 16
cpu 8086
org 0

; Choose the amount of RAM to reserve for the stack: 2 or 4 kB
; todo: probably should always be 4
%define RAMSIZE 4
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
bpb_sperfat:    dd 961
                dw 0x0000
                dw 0x0000
bpb_rootdir:    dd 2
                dw 1
                dw 6
temp_fatsector: times 4 db 0x00
temp_datastart: times 8 db 0x00
bpb_drivenumber:db 0x80
                db 0x00
                db 0x29
                dd 0x12345678
                db '1STSTAGEVBR'
                db 'FAT32   '


start:
    int 0x12 ; find out how much of the "640k" is actually open
    cli
%if RAMSIZE == 4
    mov cl, 6
    shl ax, cl
    dec ah ; reserve 4k
%else
    dec ax ; reserve 2k
    dec ax ; of the space reserved, 1.5k is used by this bootloader
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
    mov ax, [sig1]
    cmp ax, [sig2]
    mov si, msg_corrupt
    je loadfile
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
    ; assumes cs=ds
    ; trashes everything
    ;push cs
    ;pop ds
    mov bp, sp
    add eax, [bpb_startlba]
    adc edx, 0
    push edx        ; bp-4: lba [63 .. 32]
    push eax        ; bp-8: lba [31 ... 0]
    push es         ; bp-10: destination segment
    push dword 1    ; bp-14: blocks to transfer
    push word 16    ; bp-16: size of packet
    mov si, sp
    jnz .lba
    mov cx, [bpb_sectors]; cx = SPT
    mov bx, [bpb_heads]; bx = HPC
    imul bx, cx     ; bx = HPC*SPT
    mov dx, [bp-6]
    cmp dx, bx
    jae .lba
    div bx      ; dx = C
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
    xor bx, bx
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
    db "BIOS geometry error.",0
msg_read:
    db " - Read error.",0
msg_corrupt:
    db "Boot code is corrupt. Please reinstall."
    
times 0x1fe-($-$$) db 0
sig1:
dw 0xaa55

loadfile:
    mov dword [temp_fatsector], -1
    movzx eax, word [bpb_sreserved]
    cdq
    movzx cx, byte [bpb_fats]
.multiplyloop:
    add eax, [bpb_sperfat]
    adc edx, 0
    loop .multiplyloop ; avoid buggy 32-bit mul
    mov [temp_datastart], eax
    mov [temp_datastart+4], edx
    mov eax, [bpb_rootdir]
    xor ebx, ebx
    push word 0x60
    pop es
    call getcluster
    
    mov si, msg_temp
    jmp show_error
    
getcluster:
    ; eax - cluster number (auto-increment)
    ; bl - sector within cluster to load (auto-increment)
    ; es:0 - destination address
    ; assumes cs=ds, remaining bits of ebx are zero
    ; trashes everything but eax/ebx?
    push eax
    push ebx
    bsf cx, [bpb_spercluster]
    cdq
    sub eax, 2
    shld edx, eax, cl
    shl eax, cl ; avoid buggy 32-bit mul
    add eax, [temp_datastart]
    adc edx, [temp_datastart+4]
    add eax, ebx
    adc edx, 0
    call readsector
    pop ebx
    inc bx
    cmp bl, [bpb_spercluster]
    jnb .next
    pop eax
    ret
.next:
    pop edx
    mov si, dx
    shr edx, 7
    cmp edx, [temp_fatsector]
    je .skipfat
    push si
    mov [temp_fatsector], edx
    mov ax, cs
    add ax, 0x40
    mov es, ax
    movzx eax, word [bpb_sreserved]
    add eax, edx
    cdq
    call readsector
    pop si
.skipfat:
    and si, 0x7f
    shl si, 2
    mov eax, [si+0x400]
    and eax, 0x0fffffff
    xor ebx, ebx
    ret
    
    
msg_temp:
    db "No errors",0
filename:
    db "2NDSTAGEBIN" ; file name to load: "2ndstage.bin"

times 0x3fe-($-$$) db 0
sig2:
dw 0xaa55
