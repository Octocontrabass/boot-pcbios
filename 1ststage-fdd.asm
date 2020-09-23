; FAT12 bootloader for PC-compatible floppy disks

; Floppy disks are unique in that the BIOS can't accurately report the disk's
; geometry. Hard disks don't have this issue, as the BIOS may either ask the
; drive directly, or require the user to enter the correct parameters into the
; setup program. Since a floppy disk's geometry is a physical property of the
; disk, this bootloader uses the geometry written in the BIOS parameter block.

; Floppy disks have the convenient property that physical limitations of the
; format make it possible to safely assume that the geometry will never exceed
; particular values, allowing for simpler code to locate data on the disk.
; This bootloader is designed for no more than 255 cylinders, 2 heads, and 128
; sectors per track, but no existing disks come close to that.


bits 16
cpu 8086
org 0x7c00

	jmp start
	nop
	
bpb:
					db 'invalid!'
.sectorsize:		dw 512
.clustersize:		db 1
.reservedsectors:	dw 1
.fats:				db 2
.rootentries:		dw 224
					dw 2880
					db 0xf0
.fatsize:			dw 9
.spt:				dw 18
.heads:				dw 2
					dd 0
					dd 0
.drive:				db 0x00
					db 0x00
					db 0x29
					dd 0x12345678
					db '1STSTAGEFDD'
					db 'FAT12   '

start:
	cld
	xor cx, cx
	mov ds, cx
	cli
	mov ss, cx ; 8086 errata: this instruction may not inhibit interrupts
	mov sp, 0x7c00
	sti
	push es
	push di
	mov cl, 2
	mov si, pnpsig
	repe cmpsw
	jne .nopnp
	mov [bpb.drive], dl
.nopnp:
	xchg ax, cx
	mov al, [bpb.fats]
	mul word [bpb.fatsize]
	add ax, [bpb.reservedsectors]
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
	add ax, 0x1ff
	adc dx, 0
	div word [bpb.sectorsize] ; need "div imm16"
	xchg ax, bp
	int 0x12
	sub ax, 32
	shl ax, 1
	cmp ax, bp ; bp = number of sectors
	jb err
	mov ax, [bpb.rootentries]
	mov cl, 4
	shr ax, cl
	pop si
	add si, ax ; si = first sector of data area
	mov ax, [es:di+26-11]
	mov di, -1
	mov ch, 0
.nextcluster:
	push ax
	sub ax, 2
	jb err
	mov cl, [bpb.clustersize]
	mul cx
	add ax, si
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
	; AX=LBA, ES:0=buffer
	; Output:
	; AX=LBA+1
	; Clobber:
	; none?
	push bx
	push dx
	push cx
	mov cx, 3
.loop:
	push ax
	push cx
	xchg ax, cx
	mov ax, [bpb.heads]
	mul word [bpb.spt] ; DX = 0
	xchg ax, cx ; AX = LBA, CX = sectors per cylinder
	div cx ; AX = cylinder, DX = sector in cylinder
	mov ch, al ; CH = cylinder
	xchg ax, dx ; AX = sector in cylinder
	div byte [bpb.spt] ; AL = head, AH = sector minus 1
	mov cl, ah
	inc cx
	mov dh, al
	mov dl, [bpb.drive]
	xor bx, bx
	mov ax, 0x0201
	int 0x13
	jc .fail
	pop ax
	pop ax
	pop cx
	pop dx
	pop bx
	inc ax
	ret
.fail:
	pop cx
	jcxz err
	mov ah, 0x00
	int 0x13
	jc err
	dec cx
	pop ax
	jmp .loop
	
	
getfat:
	; get FAT sector
	; Input:
	; BX=byte offset into FAT
	; DI=loaded FAT sector
	; Output:
	; DI=loaded FAT sector
	; Clobber:
	; DX
	xchg ax, dx
	mov ax, bx
	push cx
	mov cl, 9
	shr ax, cl
	pop cx
	cmp ax, di
	je .done
	push es
	mov di, 0x7e0
	mov es, di
	mov di, ax
	add ax, [bpb.reservedsectors]
	call readsector
	pop es
.done:
	xchg ax, dx
	ret
	
pnpsig:
	db '$PnP'
filename:
	db '2NDSTAGEBIN'
message:
	db 'Error!',0

times 0x1fe-($-$$) db 0
dw 0xaa55
