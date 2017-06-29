;%define DEBUG_E820
%define fdisi db 0x9b,0xdb,0xe1
%define fndisi db 0xdb,0xe1

cpu 8086
bits 16

cpu_8086 equ 0
cpu_v20 equ 1
cpu_186 equ 2
cpu_286 equ 3
cpu_386 equ 4
cpu_486 equ 5

fpu_none equ 0
fpu_8087 equ 1
fpu_287 equ 2
fpu_387 equ 3
fpu_internal equ 4

section .temp vfollows=.data align=8 nobits

temp:   resb 1 ; generic scratch space

section .bss vstart=0x501 valign=1

drivenumber:    resb 1 ; BIOS drive number
cputype:        resb 1 ; detected CPU type
fputype:        resb 1 ; detected FPU type
cpusig:         resd 1 ; CPU signature (0 if unable to detect)
cpuid_max:      resd 1 ; maximum CPUID level (0 if unavailable or buggy Pentium)
mem_start:
mem_12:         resw 1 ; int 0x12
mem_88:         resw 1 ; int 0x15 ah=0x88
mem_da88:       resd 1 ; int 0x15 ax=0xda88
mem_8a:         resd 1 ; int 0x15 ah=0x8a
mem_e801:       resw 4 ; int 0x15 ax=0xe801
mem_c7:         resw 19 ; int 0x15 ah=0xc7
mem_e820_count: resw 1 ; int 0x15 eax=0xe820
mem_e820_point: resd 1
mem_end:

section .data follows=.text vfollows=.text align=1

nope_text:  db "not detected",0
yes_text:   db "yes",0
no_text:    db "no",0

section .text start=0 vstart=0x600

start:
    jmp 0:.go
.go
    push cs
    pop ds
    mov [drivenumber], dl
    cld
    
getcpu:
    mov si, .cputext
    call print
    call detect_cpu
    xor bx, bx
    mov bl, [cputype]
    shl bx, 1
    mov si, [.cputable+bx]
    call printcrlf
    
section .data

.cputext:   db "CPU type: ",0
.t86        db "8088 or 8086",0
.tnec       db "V20 or V30",0
.t1         db "80188 or 80186",0
.t2         db "80286",0
.t3         db "i386",0
.t4         db "i486",0
.cputable   dw .t86, .tnec, .t1, .t2, .t3, .t4

section .text

getcpuid:
    cmp byte [cputype], cpu_386
    jb .done
    cpu 386
    mov si, .cputext
    call print
    call detect_cpuid
    cmp dword [cpuid_max], 0
    je .nocpuid
    xor eax, eax
    cpu 486
    cpuid
    cpu 386
    mov [temp+0], ebx
    mov [temp+4], edx
    mov [temp+8], ecx
    mov word [temp+12], ' '
    mov si, temp
    call print
.nocpuid:
    mov eax, [cpusig]
    test eax, eax
    jz .nope
    mov cx, 8
    call printhex_386
    call crlf
    jmp .done
.nope:
    mov si, nope_text
    call printcrlf
    cpu 8086
.done:
    
section .data

.cputext:   db "CPU ID: ",0

section .text

getbrand:
    cmp byte [cputype], cpu_386
    jb .done
    cpu 386
    cmp dword [cpuid_max], 0
    je .done
    mov eax, 0x80000000
    cpu 486
    cpuid
    cpu 386
    cmp eax, 0x80000004
    jb .done
    mov eax, 0x80000002
    push eax
    cpu 486
    cpuid
    cpu 386
    mov [temp+0x00], eax
    mov [temp+0x04], ebx
    mov [temp+0x08], ecx
    mov [temp+0x0c], edx
    pop eax
    inc ax
    push eax
    cpu 486
    cpuid
    cpu 386
    mov [temp+0x10], eax
    mov [temp+0x14], ebx
    mov [temp+0x18], ecx
    mov [temp+0x1c], edx
    pop eax
    inc ax
    cpu 486
    cpuid
    cpu 386
    mov [temp+0x20], eax
    mov [temp+0x24], ebx
    mov [temp+0x28], ecx
    mov [temp+0x2c], edx
    mov byte [temp+0x30], 0
    mov si, temp
    call printcrlf
    cpu 8086
.done:

getfpu:
    mov si, .fputext
    call print
    call detect_fpu
    xor bx, bx
    mov bl, [fputype]
    shl bx, 1
    mov si, [.fputable+bx]
    call printcrlf
    
section .data

.fputext:   db "FPU type: ",0
.t87        db "8087",0
.t2         db "80287",0
.t3         db "i387",0
.internal:  db "internal",0
.fputable:  dw nope_text, .t87, .t2, .t3, .internal

section .text


get64:
    cmp byte [cputype], cpu_386
    jb .done
    cpu 386
    cmp dword [cpuid_max], 0
    je .done
    mov eax, 0x80000000
    cpu 486
    cpuid
    cpu 386
    cmp eax, 0x80000001
    jb .done
    mov si, ._64text
    call print
    mov eax, 0x80000001
    cpu 486
    cpuid
    cpu 386
    test edx, 1<<29
    mov si, no_text
    jz .print
    mov si, yes_text
.print:
    call printcrlf
    cpu 8086
.done:

section .data

._64text:   db "64-bit: ",0

section .text

getram:
    ; NOTE: Some Award BIOSes have a bug where INT 0x15 AH=0x8A through 0x8F
    ; push BX and then return. This causes IP:BX to be used as the return
    ; address, and leaves the flags on the stack.
    xor ax, ax
    mov cx, (mem_end - mem_start) >> 1
    mov di, mem_start
    push ds
    pop es
    rep stosw
    mov si, .t12
    call print
    int 0x12
    mov [mem_12], ax
    mov cx, 4
    call printhex
    call crlf
    mov ah, 0x88
    clc ; todo: research buggy BIOSes
    int 0x15
    jc .no88
    mov [mem_88], ax
    mov si, .t88
    call print
    mov cl, 4
    call printhex
    call crlf
.no88:
    mov ax, 0xda88
    int 0x15
    jc .noda88
    mov [mem_da88], bx
    mov [mem_da88+2], cl
    mov si, .tda88
    call print
    xchg ax, cx
    mov cx, 2
    call printhex
    xchg ax, bx
    mov cl, 4
    call printhex
    call crlf
.noda88:
    mov ax, 0x15cd ; int 0x15
    xchg [0x0e], ax
    mov [temp], ax
    mov ax, 0xe7ff ; jmp di
    xchg [0x10], ax
    mov [temp+2], ax
    mov ah, 0x8a
    mov bx, .awardbug - 0x100
    mov di, .nobug
    jmp 0x0e
.awardbug:
    jmp 0:.bugfix
.bugfix:
    add sp, 2
    mov si, .awardbugtext
    call printcrlf
    jmp .no8a
.nobug:
    jc .no8a
    mov [mem_8a], ax
    mov [mem_8a+2], dx
    mov si, .t8a
    call print
    mov cx, 4
    xchg ax, dx
    call printhex
    xchg dx, ax
    call printhex
    call crlf
.no8a:
    mov ax, [temp]
    mov [0x0e], ax
    mov ax, [temp+2]
    mov [0x10], ax
    mov ax, 0xe801
    int 0x15
    jc .noe801
    mov [mem_e801], ax
    mov [mem_e801+2], bx
    mov [mem_e801+4], cx
    mov [mem_e801+6], dx
    mov si, .te801
    call print
    push cx
    mov cx, 4
    call printhex
    call separator
    xchg ax, bx
    call printhex
    call separator
    pop ax
    call printhex
    call separator
    xchg ax, dx
    call printhex
    call crlf
.noe801:
    mov ah, 0xc7
    mov si, temp
    int 0x15
    jc .noc7
    push si
    mov si, .tc7
    call print
    pop si
    mov di, mem_c7
    mov cx, 4
    mov ax, [si]
    mov [di], ax
    call printhex
    call crlf
    mov cx, 8
    add si, 2
    add di, 2
    jmp .intoc7
.loopc7:
    call separator
.intoc7:
    push cx
    mov cx, 4
    mov ax, [si+2]
    mov [di+2], ax
    call printhex
    mov ax, [si]
    mov [di], ax
    call printhex
    pop cx
    add si, 4
    add di, 4
    loop .loopc7
    call crlf
    mov cx, 4
    mov ax, [si]
    mov [di], ax
    call printhex
    call separator
    mov ax, [si+2]
    mov [di+2], ax
    call printhex
    call crlf
.noc7:
    cmp byte [cputype], cpu_386
%ifdef DEBUG_E820
    jnb .go
    jmp .done
.go:
%else
    jb .done ; wow it doesn't fit
%endif
    cpu 386
    mov si, .te820
%ifdef DEBUG_E820
    call printcrlf
%else
    call print
%endif
    mov edx, 0x534d4150
    xor ebx, ebx
    xor si, si
    mov di, temp ; ES is already set from earlier
    mov [mem_e820_point], di
.again:
    mov eax, 0xe820
    mov ecx, 24
    mov dword [di+20], 1
    int 0x15
    jc .end
    mov edx, 0x534d4150
    cmp eax, edx
    jne .end
    cmp ecx, 24
    ja .end
    cmp cx, 20
    jb .end
%ifdef DEBUG_E820
    mov cx, 8
    mov eax, [di+4]
    call printhex_386
    mov eax, [di]
    call printhex_386
    call separator
    mov eax, [di+12]
    call printhex_386
    mov eax, [di+8]
    call printhex_386
    call separator
    mov eax, [di+16]
    call printhex_386
    call separator
    mov eax, [di+20]
    call printhex_386
    call crlf
%endif
    inc si
    add di, 24
.skip:
    test ebx, ebx
    jnz .again
.end:
    mov [mem_e820_count], si
    xchg ax, si
    mov cx, 4
    call printhex
    call crlf
    cpu 8086
.done:


section .data

.t12:   db "12: ",0
.t88:   db "88: ",0
.tda88: db "DA88: ",0
.t8a:   db "8A: ",0
.te801: db "E801: ",0
.te881: db "E881: ",0
.tc7:   db "C7: ",0
.te820: db "E820: ",0
.awardbugtext:  db "8A: Award BIOS bug detected!",0

section .text

    call enable_a20
infinity:
    sti
    hlt
    jmp infinity
    
    
detect_cpu:
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
    mov dl, cpu_186
    mov cx, 0xff21
    shr ch, cl
    jnz .done
    mov dl, cpu_v20
    mov ax, 0x0102
    aad 16
    cmp al, 0x12
    jne .done
    mov dl, cpu_8086
    jmp .done
._286:
    cpu 286
    mov dl, cpu_286
    pushf
    push word 0x7000
    popf
    pushf
    pop ax
    popf
    and ah, 0x70
    jz .done
    cpu 386
    mov dl, cpu_386
    pushfd
    cli
    pushfd
    pop eax
    mov ecx, eax
    xor eax, 1<<18
    push eax
    popfd
    pushfd
    pop eax
    popfd
    xor eax, ecx
    jz .done
    cpu 486
    mov dl, cpu_486
    cpu 8086
.done:
    mov [cputype], dl
    ret
    
    cpu 386
detect_cpuid:
    pushf
    cli
    mov eax, .cpuidhandler
    xchg [0x06*4], eax
    push eax
    xor eax, eax
    cpu 486
    cpuid
    cpu 386
    pop ebx
    mov [0x06*4], ebx
    popf
    test ah, ah
    jnz .pentiumbug
    mov [cpuid_max], eax
    test eax, eax
    jz .nocpuid
    xor eax, eax
    inc ax
    cpu 486
    cpuid
    cpu 386
    mov [cpusig], eax
    ret
.pentiumbug:
    mov [cpusig], eax
    xor eax, eax
    mov [cpuid_max], eax
    ret
    
.nocpuid:
    ; Todo: detect CPUs where CPUID is disabled and enable it
    ; Todo: is it necessary to save and restore anything else?
    ; Todo: method 1 crashes my stupid knockoff 486, maybe because
    ; the FPU jumpers aren't set correctly?
    ; Todo: ask the BIOS nicely before resorting to nasty hacks...
    ;
    ; Method 1: abuse A20 and bus termination to force the CPU into
    ; executing invalid opcode FF /7 immediately after reset, saving
    ; EDX in the exception handler, and then returning control to the
    ; BIOS. If the bus is not fully-terminated, this method will be
    ; skipped. If the motherboard is smart enough to prevent A20
    ; abuse, this method will do nothing.
    ; Method 2: use a couple of "return from 286 protected mode"
    ; resets (once with as little initialization as possible, then a
    ; second time to clean up) and hope EDX contains a useful value
    ; afterwards. Some BIOSes use a near jump instead of a far jump,
    ; so execution is directed through the breakpoint handler using
    ; opcode CC (int3) somewhere in the BIOS ROM.
    ;
    ; More useful info:
    ; http://debs.future.easyspace.com/Programming/OS/cpuid.txt
    mov [cpusig], eax ; always 0
    cli
    push ds
    lgdt [.gdt]
    mov eax, cr0
    or al, 0x01
    mov cr0, eax
    jmp $+2 ; clear prefetch buffer (for 386/486, maybe pentium?)
    mov cx, 1<<3
    mov ds, cx
    and al, ~0x01
    mov cr0, eax ; might not disable pmode on A-step 386?
    jmp $+2 ; seems to be necessary on my stupid knockoff 486
    pop ds
    mov eax, [dword 0xfffffff0&~(1<<20)]
    sti
    inc eax
    jz .skip_a20
    call enable_a20
.skip_a20
    mov ax, 0xf000
    mov es, ax
    mov al, 0xcc ; int3 opcode
    mov cx, 0xfff0 ; number of bytes to scan
    mov di, 0xffef ; first byte to scan
    std
    repne scasb
    cld
    je .setup
    ret ; give up if there's no int3 opcode (very unlikely)
.setup
    push dword [4*0x03] ; int 3 - breakpoint
    push dword [4*0x06] ; int 6 - invalid opcode
    push dword [0x467] ; shutdown resume address
    mov al, 0x0f
    cli
    out 0x70, al
    in al, 0x21
    push ax ; master pic mask
    in al, 0xa1
    push ax ; slave pic mask
    in al, 0x71
    push ax ; shutdown status byte
    xor eax, eax
    mov ax, .next1
    mov [4*0x03], eax
    mov ax, .int6handler
    mov [4*0x06], eax
    mov eax, 0xf0000000
    lea ax, [di+1]
    mov [0x467], eax
    mov al, 0x8f
    out 0x70, al
    mov al, 0xff
    out 0xa1, al
    out 0x21, al
    mov al, 0x0a ; first shutdown using type a
    out 0x71, al
    mov al, 0xfe
    mov [temp+0], sp
    mov [temp+2], ss
    out 0x64, al
    jmp infinity
.next1
    cli
    mov al, 0x8f
    out 0x70, al
    push cs
    pop ds
    cmp dword [cpusig], 0
    jne .skip1
    mov [cpusig], edx
.skip1
    mov word [4*0x03], .next2
    mov al, 0x05 ; second shutdown using type 5
    out 0x71, al
    mov al, 0xfe
    out 0x64, al
    jmp infinity
.next2
    cli
    push cs
    pop ds
    lss sp, [temp]
    mov al, 0x0f
    out 0x70, al
    pop bx
    pop ax
    out 0xa1, al
    pop ax
    out 0x21, al
    xchg ax, bx
    out 0x71, al
    pop dword [0x467]
    pop dword [4*0x06]
    pop dword [4*0x03]
    sti
    cmp dword [cpusig], 0
    jne .skip2
    mov [cpusig], edx
.skip2:
    ret
    
section .data

.gdt:   dw .gdtend - .gdt - 1
        dd .gdt
        dw 0
        db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
        .gdtend

section .text
    
.cpuidhandler:
    push bp
    mov bp, sp
    add word [bp+2], 2
    pop bp
    iret
    
.int6handler:
    cmp dword [cpusig], 0
    jne .int6skip
    mov [cpusig], edx
.int6skip:
    iret
    cpu 8086
    
    
detect_fpu:
    cmp byte [cputype], cpu_386
    jb .nocpuid
    cpu 386
    xor eax, eax
    inc ax
    cmp [cpuid_max], eax
    jb .nocpuid
    cpu 486
    cpuid
    cpu 386
    test dl, 1
    jz .nocpuid
    mov byte [fputype], fpu_internal
    ret
    cpu 8086
.nocpuid:
    xor dx, dx
    cmp byte [cputype], cpu_286
    jb .skipsetup
    cpu 286
    smsw ax
    push ax
    mov al, 0010b
    lmsw ax
    cpu 8086
.skipsetup:
    cpu 8086 fpu
    mov ax, -1
    mov [temp], ax
    fninit
    xor cx, cx
    jmp $+2
    fnstsw [temp]
    xor cx, cx
    jmp $+2
    xor cx, cx
    jmp $+2
    cmp [temp], dx
    jne .done
    fnstcw [temp]
    xor cx, cx
    jmp $+2
    xor cx, cx
    jmp $+2
    mov ax, [temp]
    and ax, 0x103f
    cmp ax, 0x003f
    jne .done
    inc dx
    and byte [temp], 0x7f
    fwait
    fldcw [temp]
    fdisi
    fstcw [temp]
    fwait
    test byte [temp], 0x80
    jnz .done
    inc dx
    finit
    fwait
    fld1
    fwait
    fldz
    fwait
    fdivp
    fwait
    fld st0
    fwait
    fchs
    fwait
    fcompp
    fstsw [temp]
    fwait
    mov ax, [temp]
    sahf
    je .done
    inc dx
    cpu 8086
.done:
    cmp byte [cputype], cpu_286
    jb .skipteardown
    cpu 286
    pop ax
    lmsw ax
    cpu 8086
.skipteardown:
    mov [fputype], dl
    ret
    
    
enable_a20:
    cpu 286
    mov si, .a20text
    call print
    call .test
    je .bios
    mov si, .already
    call print
    ret
.bios:
    mov ax, 0x2401
    int 0x15
    xor cx, cx
    call .test
    je .kbc
    mov si, .biostext
    call print
    ret
.kbc:
    cli ; interrupts should be disabled already, but...
    call .kbcwait
    jnz .fast
    mov al, 0xd1
    out 0x64, al
    call .kbcwait
    jnz .fast
    mov al, 0xdf ; should be pretty safe
    out 0x60, al
    call .kbcwait
    jnz .fast
    mov al, 0xff
    out 0x64, al
    call .kbcwait
    call .test
    je .fast
    mov si, .kbctext
    call print
    ret
.fast:
    ; todo: try port 0x92 "fast" a20
    ;in al, 0x92
    ;test al, 0x02
    ;jnz .fail
    ;or al, 0x02
    ;and al, ~0x01
    ;out 0x92, al
    
    
    mov si, .failtext
    call print
    jmp infinity
    ret
    
section .data

.a20text:   db "(A20: ",0
.already:   db "on) ",0
.biostext:  db "BIOS) ",0
.kbctext:   db "8042) ",0
;.fasttext:  db "fast) ",0
.failtext:  db "FAILED!)",13,10,13,10,"Whoops. I need to fix that.",0

section .text

.test:
    mov ax, 0xffff
    mov es, ax
    mov ax, 0xaaaa
    mov cx, 0x8 ; is this enough for all cases?
.outerloop:
    push cx
    xor cx, cx
.innerloop:
    mov word [0x600], 0x5555
    mov [es:0x610], ax
    out 0x80, al
    cmp [0x600], ax
    loope .innerloop
    pop cx
    loope .outerloop
    ret
    
.kbcwait:
    xor cx, cx
.kbcwaitloop:
    in al, 0x64
    test al, 0x02
    loopnz .kbcwaitloop
    ret
    cpu 8086
    
print:
    ; Print a null-terminated string.
    push ax
    push bx
    push bp ; BIOS bugs
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
    
    
printcrlf:
    call print
    call crlf
    ret
    
    
crlf:
    push si
    mov si, .crlf
    call print
    pop si
    ret
    
section .data

.crlf   db 13,10,0

section .text


separator:
    push si
    mov si, .separator
    call print
    pop si
    ret
    
section .data

.separator: db "-",0

section .text


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
    
    
    cpu 386
printhex_386:
    cmp cx, 4
    jbe printhex
    push cx
    push eax
    shr eax, 16
    sub cx, 4
    call printhex
    pop eax
    mov cx, 4
    call printhex
    pop cx
    ret
    cpu 8086
