; Master boot record for PC-compatibles.

; Minimum requirement: It probably shouldn't go on a floppy disk.
; Limitations: The only supported "standard" MBR-to-VBR handoff is DL. GPT is
;   not supported.
; Bugs: None?

BITS 16
CPU 8086
ORG 0

; Choose the number of times to try reading the disk before giving up.
%define TRY_COUNT 3

fakestart:
	cli
	call realstart
realstart:
	pop si
	lea si, [si-(realstart-fakestart)]
	xor ax, ax
	mov ss, ax
	mov sp, 0x7c00
	mov al, 0x60
	cld
	mov es, ax
	xor di, di
	mov cx, 0x100
	rep cs movsw
	sti
	mov cl, highstart - fakestart
	push es
	push cx
	retf
highstart:
	push cs
	pop ds
	mov si, partitions
	mov cl, 4
.loop:
	test byte [si], 0x80
	jnz load
	add si, 16
	loop .loop
	mov si, msg_noboot
	jmp print
load:
	mov ax, 0x07c0
	mov es, ax
	mov bx, dx
	mov ax, [si+0x8]
	mov dx, [si+0xa]
	call readsector
	mov si, msg_notfound
	cmp word [es:0x1fe], 0xaa55
	jne print
	mov dx, bx
	jmp 0x0000:0x7C00
	
	
disk_error:
	mov si, msg_diskerror
print:
	mov bx, 0x0007
.loop:
	mov ah, 0x0E
	lodsb
	or al, al
	jz infinity
	int 0x10
	jmp .loop
infinity:
	hlt
	jmp infinity
	
readsector:
	; dx:ax - sector to read (auto-incremented)
	; es:0x0000 - destination address
	; bl - drive number
	; Trashes: nothing, i think
	push bp		; registers to save (that aren't covered below)
	push ds
	;push es	; don't need to save this, see "les bx, [something]"
	push di
	push si
	push ax
	push cx
	push dx
	mov cx, TRY_COUNT
	;add ax, [bpb_startlba]
	;adc dx, [bpb_startlba+2]
	push bx		; bp+2 - drive number
;.top
.retry:
	push cx
	mov bp, sp
	xor cx, cx
	push cx		; bp-2 - LBA [63 .. 48]
	push cx		; bp-4 - LBA [47 .. 32]
	push dx		; bp-6 - LBA [31 .. 16]
	push ax		; bp-8 - LBA [15 ... 0]
	push es		; bp-10 - destination segment
	push cx		; bp-12 - destination offset
	inc cx
	push cx		; bp-14 - blocks to transfer
	mov cl, 16
	push cx		; bp-16 - size of packet
	mov si, sp
	mov ah, 0x08	; changes ax, bx, cx, dx, di, es
	mov dl, [bp+2]
	int 0x13		; HPC-1 = dh; SPT = cl[5:0]; no one cares about max cylinders
	;xchg bx, bx
	jc disk_error
	mov ax, 0x3f
	and cx, ax		; cx = SPT
	mov al, dh
	inc ax			; ax = HPC
	mul cx
	xchg bx, ax		; bx = HPC*SPT
	mov dx, [bp-6]
	cmp dx, bx
	jae .lba		; use LBA if the division would overflow
	mov ax, [bp-8]
	div bx			; dx = C
	xchg ax, dx		; al = H
	div cl			; ah = S-1
	mov cl, 2
	xchg ch, dl		; dl = zero; ch = low bits of C (done)
	shr dx, cl		; dx >>= 2 (dh = zero if no overflow; dl = high bits of C)
	xchg ah, cl		; cl = S - 1; ah = 2
	inc cx			; cl = S; ch can't possibly change
	or cl, dl		; cl = S | high bits of C
	xchg al, dh		; dh = H; al = zero if no overflow
	test al, al
	jz .int13
.lba:
	mov ax, 0x4200
.int13:
	inc ax			; al = 1
	les bx, [bp-12]
	mov dl, [bp+2]
	push ss
	pop ds
	int 0x13
;	jc disk_error
	mov sp, bp
	pop cx
	jnc .done
	loop .retry
;	jmp disk_error
.done:
	pop bx
	pop dx
	pop cx
	pop ax
	pop si
	pop di
	pop ds
	pop bp
    jc .disk_error
	inc ax
	jnz .ret
	inc dx
.ret:
	ret
.disk_error:
    jmp disk_error
;.retry:
;	push ax
;	xchg bx, dx
;	mov ah, 0
;	int 0x13
;	xchg dx, bx
;	pop ax
;	jmp .top
	
	
msg_diskerror:
	db "Disk error",0
msg_noboot:
	db "No active partition!",0
msg_notfound:
	db "The active partition doesn't look bootable!",13,10,"Is the wrong partition marked active?",0
	
	
TIMES 446-($-$$) db 0
partitions:
	db 0x80
	db 0x02,0x03,0x00
	db 0x06
	db 0xFE,0x3F,0x0E
	dd 128
	dd 249856
	
TIMES 510-($-$$) db 0
dw 0xaa55