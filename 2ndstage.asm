bits 16
cpu 8086

cpu_8086 equ 0
cpu_i186 equ 1
;cpu_nec equ ???
cpu_i286 equ 2
cpu_i386 equ 3
cpu_x64 equ 4

section temp vfollows=data align=512 nobits

temp:   resb 1

section bootdata vstart=0x500 nobits

boot:
.drive:     resb 1
.cpu:       resb 1
.cylinders: resw 1
.heads:     resw 1
.sectors:   resb 1
            resb 1
.partition: resq 1

section code start=0 vstart=0x600

start:
    sti
    xor ax, ax
    mov ds, ax
    mov es, ax
    cld
    mov cx, 0x100 >> 1
    mov di, 0x500
    rep stosw
    mov [boot.drive], dl
cputype:
    mov si, .tcpu
    call print
    call getcputype
    mov al, [boot.cpu]
    cbw
    shl ax, 1
    xchg si, ax
    mov si, [si+.table]
    call printcrlf
    
section data follows=code vfollows=code

.tcpu:  db "CPU type: ",0
.t0:    db "8086",0
.t1:    db "i186",0
;.tv:    db "NEC",0
.t2:    db "i286",0
.t3:    db "i386",0
.tx:    db "x64",0
.table  dw .t0, .t1, .t2, .t3, .tx

section code

geometry:
    mov si, .tid
    call print
    mov al, [boot.drive]
    mov cx, 2
    call printhex
    mov si, .tcsp
    call print
    call getgeometry
    mov ax, [boot.cylinders]
    call printdec
    mov si, .tsl
    call print
    mov ax, [boot.heads]
    call printdec
    call print
    xor ax, ax
    mov al, [boot.sectors]
    call printdec
    call crlf
    
section data

.tid:   db "Disk: ",0
.tcsp:  db ", ",0
.tsl:   db "/",0

section code
    
partition:
    mov si, .tpart
    call print
    call getpartition
    mov cx, 4
    mov si, boot.partition+6
.printloop:
    std
    lodsw
    cld
    push cx
    mov cx, 4
    call printhex
    pop cx
    loop .printloop
    call crlf
    
section data

.tpart: db "Partition: 0x",0

section code
    
stinfinity:
    sti
infinity:
    hlt
    jmp infinity
    
getcputype:
    pushf
    xor ax, ax
    push ax
    popf
    pushf
    pop ax
    popf
    and ah, 0xf0
    cmp ah, 0xf0
    jne ._286
    mov dl, cpu_i186
    mov cx, 0xff21
    shr ch, cl
    jnz .done
    ; todo: if NEC CPUs are detected as 8086, put NEC detection here.
    ; NEC should ignore the operand to AAD, so use that to tell them apart.
    mov dl, cpu_8086
    jmp .done
._286:
    cpu 286
    pushf
    push word 0x7000
    popf
    pushf
    pop ax
    popf
    and ah, 0x70
    mov dl, cpu_i286
    jz .done
    cpu 386
    mov edi, .int6handler
    xor eax, eax
    cli
    xchg edi, [0x06 * 4]
    cpu 486
    cpuid
    cpu 386
    xchg [0x06 * 4], edi
    sti
    cmp eax, 1
    jb .done386 ; jump if CF=1
    mov eax, 0x80000000
    cpu 486
    cpuid
    cpu 386
    cmp eax, 0x80000001
    jb .done386 ; jump if CF=1
    mov eax, 0x80000001
    cpu 486
    cpuid
    cpu 386
    test edx, 1<<29
    stc ; make sure CF=1 before jumping
    jz .done386
    clc
.done386:
    mov dl, cpu_i386
    sbb dl, -1
    cpu 8086
.done:
    mov [boot.cpu], dl
    ret
    
    cpu 386
.int6handler:
    push bp
    mov bp, sp
    cmp word [bp + 4], 0
    jne .badint6
    mov bp, [bp + 2]
    cmp word [cs:bp], 0xa20f ; CPUID opcode
    jne .badint6
    mov bp, sp
    add word [bp + 2], 2
    pop bp
    iret
.badint6:
    push cs
    pop ds
    mov si, .int6message
    call print
    cli
    jmp infinity
    cpu 8086
    
section data

.int6message:   db " <<INT 06 ERROR>>",0

section code

getgeometry:
    mov ah, 0x08
    mov dl, [boot.drive]
    push es
    int 0x13
    pop es
    jc .badint13
    mov al, cl
    and al, 0x3f
    mov [boot.sectors], al
    xor ax, ax
    mov al, dh
    inc ax
    mov [boot.heads], ax
    xor ax, ax
    mov al, cl
    shl ax, 1
    shl ax, 1
    mov al, ch
    inc ax
    mov [boot.cylinders], ax
    ret
.badint13:
    mov si, .int13message
    call print
    jmp stinfinity
    
section data

.int13message:  db "<<GEOMETRY FAILURE>>",0

section code

getpartition:
    xor ax, ax
    cwd
    mov bx, temp
    call [getsector]
    mov cx, 4
    xor ax, ax
    xor bx, bx
    xor dx, dx
.validate:
    mov al, [temp+0x1be+bx]
    test al, 0x7f
    jnz .noparts
    or al, al
    jns .skip
    inc dx
    mov ax, [temp+0x1be+bx+8]
    mov [boot.partition], ax
    mov ax, [temp+0x1be+bx+8+2]
    mov [boot.partition+2], ax
.skip:
    add bx, 0x10
    loop .validate
    cmp dx, 1
    je .done
    ;jb .gpt ; todo
.noparts:
    xor ax, ax
    mov [boot.partition], ax
    mov [boot.partition+2], ax
.done:
    ret
    
section data

getsector:      dw .fixup
.overflowmsg:   db "LBA overflow: 0x",0
.diskmsg1:      db "Read error: 0x",0
.diskmsg2:      db ", LBA: 0x",0
.add1           db "1"
.add0           db 0

section code
    
.fixup:
    cmp byte [boot.cpu], -1;cpu_i386 ;force 16-bit for now
    jb .fixup16
    cpu 386
    mov word [getsector], .thunk
.thunk:
    xchg ax, dx
    shl eax, 16
    xchg ax, dx
    xor edx, edx
    call getsector32
    ret
    cpu 8086
.fixup16:
    mov word [getsector], getsector16
    ;jmp getsector16
    
getsector16:
    ; dx:ax - offset into partition to read
    ; es:bx - location in RAM
    add ax, [boot.partition]
    adc dx, [boot.partition+2]
    mov si, getsector.add1
    push ax
    push dx
    jc .overflow
    ;call .debug
    call lba2chs
    mov si, getsector.add0
    jc .overflow
    mov al, 1
    int 0x13
    jc .diskerror
    add sp, 4
    ret
    
.overflow:
    push si
    mov si, getsector.overflowmsg
    call print
    pop si
    call print
    jmp .hex
.diskerror:
    mov si, getsector.diskmsg1
    call print
    mov cx, 8
    shr ax, cl
    mov cl, 2
    call printhex
    mov si, getsector.diskmsg2
    call print
.hex:
    mov cx, 4
    pop ax
    call printhex
    pop ax
    call printhex
    jmp stinfinity
    
; .debug:
    ; mov si, .debugmsg
    ; call print
    ; mov cx, 4
    ; xchg ax, dx
    ; call printhex
    ; xchg ax, dx
    ; call printhex
    ; call crlf
    ; ret
    
; section data

; .debugmsg:  db "Debug: 0x",0

; section code

getsector32:
    cpu 386
    jmp stinfinity ; todo
    ret
    cpu 8086
    
lba2chs:
    ; Convert the LBA in DX:AX to CHS.
    ; If it works, carry is clear, ax = 0x02ff,
    ; and cx and dx are ready for int 0x13 ah=0x02.
    ; If there's a problem, carry is set, and
    ; ax, cx, and dx are trashed.
    push bx
    xchg bx, ax             ; dx:bx = LBA
    mov al, [boot.sectors]  ; al = SPT
    cbw                     ; ax = SPT
    xchg cx, ax             ; cx = SPT
    mov ax, [boot.heads]    ; ax = HPC
    push dx
    mul cx              ; ax = SPT*HPC
    pop dx
    cmp dx, ax      ; clears carry if there's overflow
    cmc             ; but we want it set
    jc .ret
    xchg ax, bx     ; bx = SPT*HPC, dx:ax = LBA
    div bx          ; ax = C, dx = sector in cylinder
    xchg ax, dx     ; dx = C, ax = sector in cylinder
    div cl          ; al = H, ah = S-1
    mov cl, 2       ; cx = 2
    xchg ch, dl     ; ch = low bits of C, dl = 0
    shr dx, cl      ; dl = high bits of C, dh = overflow
    xchg ah, cl      ; cl = S-1, ah = 2
    inc cx          ; cl = S
    or cl, dl       ; cl = hi-C | S
    xchg al, dh     ; dh = H, al = overflow
    mov dl, [boot.drive]
    neg al          ; set carry if al is nonzero
.ret:
    pop bx
    ret
    
    
print:
    ; Print the null-terminated string at DS:SI.
    push ax
    push bx
    push bp ; BIOS is allowed to overwrite this for some reason
    push si
    mov bx, 0x0007
.loop:
    mov ah, 0x0e
    lodsb
    or al, al
    jz .done
    int 0x10
    jmp .loop
.done:
    pop si
    pop bp
    pop bx
    pop ax
    ret
    
crlf:
    push si
    mov si, .crlf
    call print
    pop si
    ret
    
section data

.crlf   db 13,10,0

section code

printcrlf:
    call print
    call crlf
    ret
    
printhex:
    ; Print a variable number of hex digits.
    ; Place the digits in ax, the length in cx, and call.
    push ax
    push cx
    push dx
    push bx
    push bp
    mov bx, cx
.pushloop
    mov dx, ax
    and al, 0x0f
    cmp al, 0x0a
    sbb al, 0x69
    das
    mov ah, 0x0e
    push ax
    xchg ax, dx
    push cx
    mov cl, 4
    shr ax, cl
    pop cx
    loop .pushloop
    mov cx, bx
    mov bx, 0x0007
.poploop
    pop ax
    int 0x10
    loop .poploop
    pop bp
    pop bx
    pop dx
    pop cx
    pop ax
    ret
    
printdec:
    push ax
    push cx
    push dx
    push bx
    push bp
    xor cx, cx
    mov bx, 10
.pushloop:
    inc cx
    xor dx, dx
    div bx
    or dx, 0x0e00 | '0'
    push dx
    test ax, ax
    jnz .pushloop
    mov bx, 0x0007
.poploop:
    pop ax
    int 0x10
    loop .poploop
    pop bp
    pop bx
    pop dx
    pop cx
    pop ax
    ret
