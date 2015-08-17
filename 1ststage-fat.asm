; FAT12/FAT16 bootloader for PC-compatibles.

; Minimum requirement: INT 13 function 08 must work on the boot device. For
;   hard disks, this function should always be available. For floppy disks,
;   this function should be available if the BIOS supports 3½-inch disks (even
;   when booting a 5¼-inch disk).
; Limitations: Sectors must be 512 bytes. The filesystem must be within the
;   first 2TiB of the disk.
; Bugs: None? The disk address packet is only 2-byte aligned, but so far that
;   hasn't caused any issues.

BITS 16
CPU 8086
ORG 0

; Choose the amount of RAM to reserve for the stack: 2 or 4 kB
%define RAMSIZE 2
; Choose the number of times to try reading the disk before giving up.
%define TRY_COUNT 3
; Choose between FAT12 and FAT16 support.
;%define FAT 12

fakestart:
	jmp start
	nop
	
	
				db 'ERROR!!!'
				dw 512
bpb_spercluster:db 1
bpb_sreserved:	dw 1
bpb_fats:		db 2
bpb_entries:	dw 224
				dw 2880
				db 0xF0
bpb_sperfat:	dw 9
				dw 18
				dw 2
bpb_startlba:	dd 0
				dd 0
				db 0x00
				db 0x00
				db 0x29
				dd 0x867D5309
				db '1STSTAGEVBR'
%if FAT == 16
				db 'FAT16   '
%else
				db 'FAT12   '
%endif


start:
	cli
	call realstart
realstart:
	pop si
	lea si, [si-(realstart-fakestart)] ; cs:si points to self
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
	mov ds, ax
	mov es, ax
	mov ss, ax
%if RAMSIZE == 4
	mov sp, 0x1000 ; stack at 4k
%else
	mov sp, 0x0800 ; stack at 2k
%endif
	cld
	xor di, di
	mov cx, 0x100
	rep cs movsw
	sti
	mov cl, highstart - fakestart
	push es
	push cx
	retf
highstart:
	;xchg bx, bx
	mov bp, sp
	push dx		; bp-2 -> drive number
	mov ax, [bpb_sreserved]
	push ax		; bp-4 -> first sector of FAT
	xchg bx, ax
	xor ax, ax
	push ax
	mov al, [bpb_fats]
	mul word [bpb_sperfat]
	add ax, bx
	pop bx
	adc dx, bx
	push dx
	push ax		; bp-8 -> first sector of root directory
	mov cl, 0x60
	mov es, cx
	mov bl, [bp-2]
	mov cx, [bpb_entries]
	mov si, filename
	xor di, di
	push bp
	mov bp, sp
.newsector:
	call readsector
.loop:
	push cx
	push si
	mov cx, 11
	repe cmpsb
	je loadfile
	pop si
	pop cx
	dec cx
	add di, 32
	and di, 0x01e0
	jcxz .disk_error
	jz .newsector
	jmp .loop
.disk_error:
	jmp disk_error
loadfile:
	;xchg bx, bx
	mov sp, bp
	pop bp
	xor ax, ax
	dec ax
	push ax		; bp-10 -> currently loaded FAT sector
	mov ax, [bpb_entries]
	mov cx, 4
	shr ax, cl
	cwd
	add [bp-8], ax
	adc [bp-6], dx		; bp-8 -> first sector of data area
	mov ax, [es:di+0xf]
.fileloop:
	push ax
	dec ax
	dec ax
	mov cl, [bpb_spercluster]
	mul cx
	add ax, [bp-8]
	adc dx, [bp-6]
	mov bl, [bp-2]		; for readsector AND getfat
.clusterloop:
	push cx
	call readsector
	;xchg bx, bx
	mov cl, 0x20
	mov si, es
	mov di, cs
	add si, cx
	sub di, cx
	cmp si, di
	ja disk_error
	mov es, si
	pop cx
	loop .clusterloop
%if FAT == 16
	pop si
	call getfat
	and si, 0xFF
	shl si, 1
	mov ax, [si+0x200]
	cmp ax, 0xFFF7
%else
	pop ax
	mov si, ax
	shr ax, 1
	rcl cx, 1
	push cx
	add si, ax
	push si
	call getfat
	mov dx, 0x1FF
	and si, dx
	mov al, [si+0x200]
	pop si
	inc si
	call getfat
	and si, dx
	mov ah, [si+0x200]
	pop cx
	test cx, cx
	jz .noshift
	mov cl, 4
	shr ax, cl
.noshift:
	and ax, 0xFFF
	cmp ax, 0xFF7
%endif
	jb .fileloop
done:
	mov dx, bx
	mov sp, bp
	jmp 0x0000:0x0600
	
	
	
disk_error:
	;xchg bx, bx
	mov cx, msg_error_len
	mov si, msg_error
	mov bx, 0x0007
.loop:
	mov ah, 0x0e
	cs lodsb
;	jz infinity		; looks like i forgot to remove this when i was cleaning up
	int 0x10		; overwrites ah, bp
	loop .loop
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
	add ax, [bpb_startlba]
	adc dx, [bpb_startlba+2]
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
	;xchg bx, bx
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
	
	
getfat:
	; si - byte/word of FAT that needs to be available, relative to start of FAT
	; cs:0200 - buffer to store portion of FAT as needed
	; [bp-10] - current FAT sector in memory
    ; [bp-4] - first FAT sector
	; ch - MUST BE ZERO
	; bl - drive number
	; Trashes CL
	push si
	;push cx
%if FAT == 16
	mov cl, 8
%else
	mov cl, 9
%endif
	shr si, cl
	cmp si, [bp-10]
	je .ret
	mov [bp-10], si
	push dx
	push ax
	push es
	mov ax, cs
	add ax, 0x20
	mov es, ax
	mov ax, [bp-4]
	xor dx, dx
	add ax, si
	adc dx, 0
	call readsector
	pop es
	pop ax
	pop dx
.ret:
	;pop cx
	pop si
	ret
	
	
msg_error:
	db "Error!"
	msg_error_len equ ($-msg_error)
	
filename:
	db "2NDSTAGEBIN" ; file name to load: "2ndstage.bin"
	
TIMES 510-($-$$) db 0
dw 0xaa55