; FAT12/FAT16 bootloader for PC-compatible hard disks

; This bootloader cheats a bit and assumes that DL will be set according to
; the PnP specification. On hard disks, it requires the MBR to set DL
; appropriately in case the BIOS does not support PnP. USB flash drives don't
; always have a MBR, but the BIOS should support PnP if it can boot from USB.

%ifndef FAT
%define FAT 12
%endif
%if (FAT != 12) && (FAT != 16)
%error "Nice try, but only FAT12 and FAT16 are allowed."
%endif

bits 16
cpu 8086
org 0x7c00

	jmp start
	nop
	
bpb:
					db 'invalid!'
.sectorsize:		dw 0
.clustersize:		db 0
.reservedsectors:	dw 0
.fats:				db 0
.rootentries:		dw 0
					dw 0
					db 0
.fatsize:			dw 0
.spt:				dw 0
.heads:				dw 0
.lba:				dd 0
					dd 0
.drive:				db 0
					db 0
					db 0
					dd 0
					db '1STSTAGEVBR'
%if FAT == 12
					db 'FAT12   '
%else
					db 'FAT16   '
%endif


start:
	cld
	xor ax, ax
	mov ds, ax
	cli
	mov ss, ax ; 8086 errata, if anyone's got a hard disk in one of those
	mov sp, 0x7c00
	sti
	push es
	push di
	mov [bpb.drive], dl ; USB implies PnP right? the MBR can handle hard disks
	mov ah, 0x08
	push ds
	int 0x13 ; one 8088 BIOS clobbers DI, SI, BP, DS, and ES
	pop ds
	xchg ax, cx
	and ax, 0x3f
	mov [bpb.spt], ax
	mov al, dh
	inc ax
	mov [bpb.heads], ax
	dec ax
	mov al, [bpb.fats]
	mul word [bpb.fatsize]
	add ax, [bpb.reservedsectors]
	adc dx, 0
	push dx
	push ax
	mov cx, [bpb.rootentries]
	mov di, 0x0800
	mov es, di
	xor di, di
.dirloop:
	and di, 0x1e0
	jnz .skip
	call readsector
.skip:
	mov si, filename
	push cx
	mov cx, 11
	repe cmpsb
	pop cx
	jne .next
	test byte [es:di], 0x18
	jz loadfile
.next:
	add di, 0x20
	loop .dirloop
	jmp err
loadfile:
	mov ax, [es:di+28-11]
	mov dx, [es:di+30-11]
	mov cx, 0x1ff
	add ax, cx
	adc dx, 0
	jc err
	inc cx
	cmp dx, cx
	jae err
	div cx
	xchg ax, bp
	int 0x12
	sub ax, 32
	shl ax, 1
	cmp ax, bp
	jb err
	mov ax, [bpb.rootentries]
	mov cl, 4
	shr ax, cl
	pop si
	add si, ax
	mov ax, [es:di+26-11]
	pop di
	adc di, 0
	mov word [filename], -1
	mov ch, 0
.nextcluster:
	push ax
	sub ax, 2
	jb err
	mov cl, [bpb.clustersize]
	mul cx
	add ax, si
	adc dx, di
.nextsector:
	call readsector
	dec bp
	jnz .notdone
	pop di
	pop di
	pop es
	mov dl, [bpb.drive]
	jmp 0:0x8000
.notdone:
	mov bx, es
	add bx, 0x20
	mov es, bx
	loop .nextsector
%if FAT == 12
	pop ax
	mov bx, ax
	shr ax, 1
	sbb cl, cl
	add bx, ax
	push bx
	call getfat
	and bh, 0x1
	mov al, [bx+0x7e00]
	pop bx
	inc bx
	call getfat
	and bh, 0x1
	mov ah, [bx+0x7e00]
	jcxz .noshift
	mov cl, 4
	shr ax, cl
.noshift:
	and ax, 0xfff
%else
	pop bx
	call getfat
	mov bh, 0x7e >> 1
	shl bx, 1
	mov ax, [bx]
%endif
	jmp .nextcluster
	
err:
	mov si, message
.again:
	lodsb
	or al, al
	jz forever
	mov ah, 0x0e
	mov bx, 0x0007
	int 0x10
	jmp .again
forever:
	hlt
	jmp forever
	
readsector:
	; Read sector.
	; Input:
	; DX:AX = block address, relative to BPB start LBA
	; ES:0 = buffer
	; Output:
	; DX:AX = block address plus one
	push ax
	push cx
	push dx
	push bx
	push si
	xor cx, cx
	add ax, [bpb.lba]
	adc dx, [bpb.lba+2]
	adc cx, 0
	mov [dap.seg], es
	mov [dap.lba], ax
	mov [dap.lba+2], dx
	mov [dap.lba+4], cx
	jnz .lba
	xchg ax, bx
	push dx
	mov ax, [bpb.heads]
	mov cx, [bpb.spt]
	mul cx
	xchg ax, bx
	pop dx
	cmp dx, bx
	jae .lba
	div bx			; ax = cylinder, dx = sector in cylinder
	xchg ax, dx		; dx = cylinder, ax = sector in cylinder
	div cl			; al = head, ah = sector minus one
	mov cl, 2
	xchg ch, dl		; ch = low bits of C, dl = 0
	shr dx, cl		; dh = overflow, dl = high bits of C
	xchg ah, cl		; ah = 2, cl = S-1
	inc cx			; cl = S
	or cl, dl
	xchg al, dh		; dh = head, al = overflow
	xor bx, bx
	neg al ; set carry if nonzero
	inc ax ; does not modify carry
	jnc .int13
.lba:
	mov ah, 0x42
	mov si, dap
.int13:
	mov dl, [bpb.drive]
	int 0x13
	jc err
	pop si
	pop bx
	pop dx
	pop cx
	pop ax
	inc ax
	jnz .skip
	inc dx
.skip:
	ret
	
getfat:
	; get FAT sector
	; Input:
	; BX=byte or word offset into FAT
	; Output:
	; None
	; Clobber:
	; None
	push ax
	push cx
	mov ax, bx
%if FAT == 12
	mov cl, 9
%else
	mov cl, 8
%endif
	shr ax, cl
	cmp ax, [filename]
	je .done
	push dx
	push es
	mov cx, 0x7e0
	mov es, cx
	mov [filename], ax
	xor dx, dx
	add ax, [bpb.reservedsectors]
	adc dx, 0
	call readsector
	pop es
	pop dx
.done:
	pop cx
	pop ax
	ret
	
dap:
		db 16
		db 0
		db 1
		db 0
		dw 0
.seg:	dw 0
.lba:	dq 0

filename:
	db '2NDSTAGEBIN'
message:
	db 'Error!',0

times 0x1fe-($-$$) db 0
dw 0xaa55
