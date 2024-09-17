org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

;
; FAT12 header
;

jmp short start
nop

dbd_oem: 					db 'MSWIN4.1' ;8 bytes 
dbd_bytes_per_sector: 		dw 512
dbd_sectors_per_cluster: 	db 1 
dbd_reserved_sectors: 		db 1 
dbd_fat_count:				db 2
dbd_dir_entries_count:		dw 0E0h
dbd_total_sectors: 			dw 2880 ; 2880*512 = 1.4Mb
dbd_media_descriptor_type:  db 0F0h
dbd_sectors_per_fat: 		dw 9 
dbd_sectors_per_track: 		dw 18
dbd_heads: 					dw 2
dbd_hidden_sectors: 		dd 0
dbd_large_sector_count: 	dd 0


; extended boot record
edr_drive_number:			db 0
							db 0
edr_signature:				db 29h
edr_volume_id:				db 12h, 34h, 56h, 78h
edr_volume_label 			db 'NANOBYTE OS'	 			
edr_system_id 				db 'FAT12	'

start: 
	jmp main

puts:
	push si
	push ax
	push bx

.loop:
	lodsb
	or al, al
	jz .done

	mov ah, 0x0E
	mov bh, 0
	int 0x10

	jmp .loop

.done:
    pop bx
    pop ax
    pop si    
    ret

main: 
	mov ax, 0
	mov ds, ax
	mov es, ax

	mov ss, ax
	mov sp, 0x7C00

	; read smthn from floppy disk
	; BIOS should set dl to drive number
	mov [edr_drive_number], dl
	mov ax, 1						;LBA = 1, second sector from disk
	mov cl, 1						;1 sector to read
	mov bx, 0x7E00					;data should be after bootloader
	call disk_read


	;print hello world message
	mov si, msg_hello
    call puts

	cli						;disable interupts, this way CPU cant get out of halt state
	hlt



;
; Error handlers
;

floppy_error: 
	mov si, msg_read_failed
	call puts
	jmp wait_key_and_reboot
wait_key_and_reboot:
	mov ah, 0 
	int 16h					;wait for keypress
	jmp 0FFFFh:0			;jump to beginning of bios, should reboot


.halt:
	cli						;disable interupts, this way CPU cant get out of halt state
	hlt


;
; Disk routines
;

; Converts an LBA adress to CHS adress
; Parameters: 
;	ax: 
;		-LBA adress
; Returns:
;	- cx [bits 0 - 5]: sector number
;	- cx [bits 5 - 16] cylinder number
; 	- dh: head



lba_to_chs:
    push ax
    push dx

    xor dx, dx                          ; dx = 0
    div word [dbd_sectors_per_track]    ; ax = LBA / SectorsPerTrack
                                        ; dx = LBA % SectorsPerTrack

    inc dx                              ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx                          ; cx = sector

    xor dx, dx                          ; dx = 0
    div word [dbd_heads]                ; ax = (LBA / SectorsPerTrack) / Heads = cylinder
                                        ; dx = (LBA / SectorsPerTrack) % Heads = head
    mov dh, dl                          ; dh = head
    mov ch, al                          ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah                           ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al                          ; restore DL
    pop ax
    ret

;
; Reads sector from a disk
;
; Params: 
;	- ax: LBA adress
;	- cl: num of sectors to read
;	- dl: drive number
;	- es:bx: memory adress where to store read data
;	

disk_read:
	push ax					;save registers that will be modified 
	push bx
	push cx 
	push dx
	push di

 
	push cx					;temporarirly saves CL (number of sectors to read)
	call lba_to_chs			;compute CHS
	pop ax					;AL = number of sectoes to read

	mov ah, 02h
	mov di, 3				;retry count



.retry:
	pusha 					;save all registers, i dont know which bios modifies
	stc						;set carry flag
	int 13h					;carry flag cleared = success
	jnc .done				;jump if carry not set
	
	;--failed--
	popa
	call disk_reset
	dec di
	test di, di
	jnz .retry

.fail:
	;all attempts are exhausted
	jmp floppy_error
 

.done:
	popa

	pop di
	pop dx
	pop cx 
	pop bx
	pop ax					;restorse registers modified 
	ret


;
; Reset disk controller
; Parameters:
;	- dl: drive number
disk_reset:
	pusha
	mov ah, 0
	int 13h
	jc floppy_error
	popa
	ret 


msg_hello: db 'Hello world!', ENDL, 0
msg_read_failed db 'Read from disk failed!', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
