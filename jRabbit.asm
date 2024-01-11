[org 0x0100]
jmp start

rows: db 0x19 ;25 in Hex
cols: db 0x50 ;80 in Hex
screenBuffer: times 4000 db 0 
BuildingWallRepition: dw 7
RoadStart: dw 1440
RoadLines: dw 2092
RoadLineStart: dw 2080
CarStart: dw 40
CarWidth: dw 14
linesAmount: dw 6
endRoadLine: dw 2238  ;2078 initial
SkyStart: dw 2400
yellowBrickStart: dw 3240
rabbitStart: dw 3440
rabbithead: dw 3282
GrassStart: dw 3520
SandStart: dw 3840
RabbitColor: db 0x7F
RabbitEyeColor: db 0xF0
JumpKey: dw 0x39
exitKey: dw 0x27
OldKeyboardInterrupt: dd 0
OldTimerInterrupt: dd 0
tickcount: dw 0
endCondition: db 0
BlueBrickCount: dw 0
BlueBrickTemp: dw 0
BusyMode: db 0
Score:                    db 'SCORE: '
Num: 					  db 200
ScoreLength:				dw 7
ScoreToDisplay:				dw 0000
RNG: dw 0
CarrotFlag: dw 0
DaySkyColor: dw 0x1920
NightSkyColor: dw 0020
GREEN_COLOR    equ 0x2F20
RED_COLOR     equ 0x4020
ORANGE_COLOR  equ 0xEE20
CYAN_COLOR	  equ 0x3320
currentColor  dw  RED_COLOR  ; Initial color
music_length dw 18644
ScreenSize: dw 25 * 80 * 2
ColumnSize: dw 80
; speed Brick: line 379
; introScreenKeyStroke: line 2467
; speed building&car : line 340

Title0:					   db '--------------------The Royal Game--------------------'
Intro0:					   db 'Group Members'
Intro1:                    db 'M. Talha Iqbal  22L-6910'
Intro2:					   db 'Daniyal Sultan  22L-6845'
Instruction00			   db 'Instructions'
Instruction0:			   db 'Experience the multi-animation of cars and buildings'
Instruction1:			   db 'Press Space to jump the rabbit and score points by catching the carrot'

Conclusion0:			   db 'GAME OVER'
Conclusion1: 			   db 'Your Score: '

Confirmation0:			   db 'Are You Sure You Want To Exit?'
Confirmation1:  		   db 'Press Escape To Exit or any other key To Return'

;Subroutine To Clear Screen
clrscr:		push es
			push ax
			push di
			push bx
			
			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov bx, [rows]
			mov ax, [cols]
			mul bx			
			shl bx, 1					; Configurable screen by making bl dependent on rows n cols
			mov di, 0					; point di to top left column

nextloc:	mov word [es:di], 0x0720	; clear next char on screen (07 is Attribute for a White-colored command line and a black bg, and 20 is ASCII for space)
			add di, 2					; move to next screen location
			cmp di, bx					; Total bytes in a screen
			jne nextloc					; if no clear next position

			pop bx
			pop di
			pop ax
			pop es
			ret
			
	delay:  push cx
			mov cx, 0xFFFF
		loop1:	loop loop1
				mov cx, 0xFFFF
		loop2:	loop loop2
				pop cx
				ret
				
	HyperDelay:  push cx
			mov cx, 0xFFFF
			mov dx, 2

		loop6:	loop loop6
				mov cx, 0xFFFF
		loop5:	loop loop5
				mov cx, 0xFFFF 	
		loop3:	loop loop3
				mov cx, 0xFFFF
				
				dec dx
				cmp dx, 0
				jne loop6
				
		loop4:	loop loop4
				pop cx
				ret	
				
				ret
		
		miniDelay:
			call HyperDelay
			call HyperDelay	
			call HyperDelay
			
			ret
				
				
ScrollBGRight:
	push bp
	mov bp, sp
	sub sp, 4 ; locals

	push ax
	; bp - 2 = letter
	; bp - 4 = column size
	
	mov ax, 0
	mov al, byte [cols]
	mov [bp-4], ax

	push es
	push ds
	push di
	push si
	push cx

	mov ax, 0xb800
	mov es, ax
	mov ds, ax
	
	; Copy
	mov di, [bp-4] ;Row which is to be copied
	mov ax, word[bp+4] ;Current row number which is to be pasted
	
	add ax, ax
	imul di, ax 
	
	sub di, 2
	mov ax, [es:di]
	mov [bp-2], ax
	
	; Moving
	mov di, [bp-4]
	imul di, word[bp+4]
	imul di, 2
	sub di, 2
	
	mov si, [bp-4]
	imul si, word[bp+4]
	imul si, 2
	sub si, 4
	
	mov cx, [bp-4]
	std
	rep movsw
	
	; Paste
	mov di, [bp-4]
	mov ax, word[bp+4]
	sub ax, 1
	add ax, ax
	imul di, ax
	mov ax, [bp-2]
	mov [es:di], ax
	
	pop cx
	pop si
	pop di
	pop ds
	pop es
	pop ax
	mov sp, bp
	pop bp
	ret 2
	
	
ScrollBGLeft:
	push bp
	mov bp, sp
	sub sp, 4 ; 1 word space for local variable
	
	push ax
	; bp - 2 = temporary letter
	; bp - 4 = column size since ds is changed
	mov ax, [ColumnSize]
	mov [bp - 4], ax
	
	push es
	push ds
	push di
	push si
	push cx

	mov ax, 0xb800
	mov es, ax
	mov ds, ax
	
	; Copy extreme left character ((2 * columnSize * (row - 1))
	mov di, [bp - 4]
	mov ax, [bp + 4]
	dec ax
	imul di, ax
	imul di, 2
	mov ax, [es:di]
	mov [bp-2], ax
	
	; [160] = [162]
	; [es:((row - 1) * ColumnSize * 2)] = [ds:(((row - 1) * ColumnSize * 2) + 2)]
	mov di, [bp - 4]
	mov ax, word[bp + 4]
	dec ax
	imul di, ax
	imul di, 2
	
	mov si, [bp - 4]
	imul si, ax
	imul si, 2
	add si, 2
	
	mov cx, [bp - 4]
	
	cld
	rep movsw
	
	; Paste extreme left character to extreme right ((2 * columnSize * row) - 2)
	mov di, [bp - 4]
	imul di, word[bp + 4]
	imul di, 2
	sub di, 2
	mov ax, [bp-2]
	mov [es:di], ax
	
	pop cx
	pop si
	pop di
	pop ds
	pop es
	pop ax
	mov sp, bp
	pop bp
	ret 2
	
ScrollLeft:
    push bp
    mov bp, sp
    sub sp, 4 ; locals

    push ax
    ; bp - 2 = letter
    ; bp - 4 = column size

    mov ax, 0
    mov al, byte [cols]
    mov [bp-4], ax

    push es
    push ds
    push di
    push si
    push cx

    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    
    ; Copy extreme left character (0)
    mov di, 0
    mov ax, word[bp+4] ; Current row number which is to be pasted
    add ax, ax
    imul di, ax
    
    mov ax, [es:di]
    mov [bp-2], ax

    ; Moving (i * ColumnSize * 2) = (i * ColumnSize * 2) + 2
    mov di, [bp-4]
    imul di, word[bp+4]
    imul di, 2
    add di, 2
    
    mov si, [bp-4]
    imul si, word[bp+4]
    imul si, 2
    
    mov cx, [bp-4]
    std
    rep movsw
    
    ; Paste extreme left character (0)
    mov di, [bp-4]
    mov ax, word[bp+4]
    add ax, ax
    imul di, ax
    mov ax, [bp-2]
    mov [es:di], ax

    pop cx
    pop si
    pop di
    pop ds
    pop es
    pop ax
    mov sp, bp
    pop bp
    ret 2
	


	; ScrollRoadRight:
				; push bp
				; mov bp, sp
				; sub sp, 4 ; 2 word space for local variable
	
				; push ax
				; ; bp - 2 = temporary letter
				; ; bp - 4 = column size since ds is changed
				; xor ax, ax
				; mov al, byte [cols]
				; mov [bp-4], ax
	
				; push es
				; push ds
				; push di
				; push si
				; push cx

	; mov ax, 0xb800
	; mov es, ax
	; mov ds, ax
	
	; ; Copy extreme right character ((2 * (cols * row)) - 2)
	; mov di, [bp-4]
	; mov ax, word[bp+4]
	; imul di, ax 
	; imul di, 2
	; sub di, 2
	; mov ax, [es:di]
	; mov [bp-2], ax
	
	; ; [es:(i * cols * 2) - 2] = [ds:((i * cols * 2) - 4)]
	; mov di, [bp-4]
	; imul di, word[bp+4]
	; imul di, 2
	; sub di, 2
	
	; mov si, [bp-4]
	; imul si, word[bp+4]
	; imul si, 2
	; sub si, 4
	
	; mov cx, [bp-4]
	; std
	; rep movsw
	
	; ; Paste extreme right character ((2 * (cols * (row - 1))))
	; mov di, [bp-4]
	; mov ax, word[bp+4]
	; sub ax, 1
	; imul di, ax 
	; imul di, 2
	; mov ax, [bp-2]
	; mov [es:di], ax
	
	; pop cx
	; pop si
	; pop di
	; pop ds
	; pop es
	; pop ax
	; mov sp, bp
	; pop bp
	; ret 2


PlaceToMove:
	push ax
	push bx
	push cx
	
	xor ax, ax
	PlaceToMove_ScrollBGRight:
		mov cx, ax
		push cx
		call ScrollBGRight
		
		; push cx
		; call ScrollBGRight   ;speed
	
		inc ax
		mov bx, 10
		inc bx ; 1 + FirstSegmentSize (because of row starting at 1)
		cmp ax, bx
		jnz PlaceToMove_ScrollBGRight
	
	pop cx
	pop bx
	pop ax
	ret 
	
	
PlaceToMoveCar:
	push ax
	push bx
	push cx
	
	mov ax, 11

	PlaceToMoveCar_ScrollBGLeft:
		mov cx, ax
		push cx
		call ScrollBGLeft
	
		inc ax
		mov bx, 16;was 16
		; add bx, 10
		cmp ax, bx
		jnz PlaceToMoveCar_ScrollBGLeft
	
	pop cx
	pop bx
	pop ax
	ret 
	

BrickToMove:
	push 20  ;brick row which moves

	call ScrollBGRight
	
	; push 20
	; call ScrollBGRight ;speed
	ret 
	
; MoveSecondSegment:
	; push ax
	; push bx
	; push cx
	; push dx
	
	; xor ax, ax
	; mov dx, ax
	; MoveSecondSegment_ScrollRoadRight:
		; mov cx, ax
		; push cx
		; call ScrollRoadRight
	
		; inc ax
		; inc dx
		
		; xor bx, bx
		; add bx, 80
		; cmp dx, bx
		; jnz MoveSecondSegment_ScrollRoadRight
		
	; pop dx
	; pop cx
	; pop bx
	; pop ax
	; ret
				
				
printCar:			push bp
					mov bp, sp
					push es
					push ax
					push cx
					push si
					push di
					push bx

					mov ax, 0xb800
					mov es, ax
					mov di, [RoadLineStart]	;Initializing offset
					sub di, 480 ;We're putting the car 4 rows above the road
					add di, [CarStart] ;Starting position of car
					mov bx, [CarWidth]
		CarRoof:	mov al, 0x20
					mov ah, 0x40 ;White Color
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					jnz CarRoof
					
					
					;Car's 2nd Row
					mov di, [RoadLineStart]	;Initializing offset
					sub di, 320 ;We're putting the car 3 rows above the road
					add di, [CarStart]
					mov bx, [CarWidth]
					sub bx, 10
		CarSpace1:	mov al, 0x20
					mov ah, 0x40 ;White Color
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					jnz CarSpace1
					mov bx, 6
		CarWindow:	mov al, 0x20
					mov ah, 0xF0 ;Black Color
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					jnz CarWindow			
					mov bx, [CarWidth]
					sub bx, 10
		CarSpace2:	mov al, 0x20
					mov ah, 0x40 ;White Color
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					jnz CarSpace2
					mov si, 2
					;Car's 3rd row
					mov di, [RoadLineStart]
					sub di, 160
					add di, [CarStart]
					sub di, 8 ;Starting this row 4 columns before the roof
					mov bx, [CarWidth]
					add bx, 8
	CarMainBody:	mov al, 0x20
					mov ah, 0x40 ;White Color
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					jnz CarMainBody	
					
					;Car's 4th row
					mov di, [RoadLineStart]
					add di, [CarStart]
					sub di, 8
					mov word [es:di], 0x7020
					add di,2 
					mov word [es:di], 0x7020
					add di, 2
					mov bx, 4
		CarSpace3:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace3
					
				;Making the First Tire
					mov si, 3
			Tire1:	mov word [es:di], 0x0720
					add di, 2
					sub si, 1
					jnz Tire1
					
					
					mov bx, 4
		CarSpace4:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace4
					
					
					;Making the Second Tire
					mov si, 3
			Tire2:	mov word [es:di], 0x0720
					add di,2
					sub si, 1
					jnz Tire2
					
					
					mov bx, 2
		CarSpace5:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace5
					; mov word [es:di], 0x6020 ;Yellow headlight
					; add di, 2
					; mov word [es:di], 0x7020
					; add di, 2
					
					;Car's 5th row
					mov di, [RoadLineStart]
					add di, 160
					add di, [CarStart]
					sub di, 8
					mov bx, 6
		CarSpace6:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace6
					
					
					; mov cx, 4
		; TireBase1:	mov word [es:di], 0x875F ;Drawing the First Tire Base
					; add di,2
					; loop TireBase1					
					
					mov bx, 4
		CarSpace7:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace7
					
					; mov cx, 4
		; TireBase2:	mov word [es:di], 0x875F ;Drawing the Second Tire Base
					; add di,2
					; loop TireBase2
					
					mov bx, 4
		CarSpace8:	mov word [es:di], 0x7020
					add di,2
					sub bx, 1
					jnz CarSpace8
					
					
					
					
		
			exitCar:pop bx
					pop di
					pop si
					pop cx
					pop ax
					pop es
					pop bp
					ret
	;-------------------------------------- ----------------------------------- ----------------------------------
	;1st Part
			
printBackground:	push bp
					mov bp, sp
					push es
					push ax
					push cx
					push si
					push di
					push bx

					mov ax, 0xb800
					mov es, ax
					mov di, 0	;Initializing offset
					mov cx, 80
			l1:		mov al, ' '
					mov ah, 0x10
					mov word [es:di], ax
					add di, 2
					loop l1
					
					mov cx, 8 ; for the Top half of the buildings
		Roof:		mov bx, 2 ;2 spaces are to be input
		SpaceLoop1:	mov al, ' '
					mov ah, 0x10 ;Blue background, can be turned to black with 0x00
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne SpaceLoop1
					mov bx, 6 ;6 cells for the roof of each building
		Ceiling:	mov al, '-'
					mov ah, 0x04 ;Red color because it's vibrant
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne Ceiling
					mov bx, 2 ;2 spaces are to be input
		SpaceLoop2:	mov al, ' '
					mov ah, 0x10 ;Blue background, can be turned to black with 0x00
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne SpaceLoop2
					loop Roof
					
					; ;Once Roof's done, we'll move to making the walls and floors
					mov si, [BuildingWallRepition] ;Floors of buildings which can now be configurable
		FloorSize:	mov cx, 8
		Walls:		mov bx, 2 ;2 spaces are to be input
		SpaceLoop3:	mov al, ' '
					mov ah, 0x10 ;Blue background, can be turned to black with 0x00
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne SpaceLoop3
					mov ah, 0x06
					mov al, '|'
					mov word [es:di], ax ;Starting wall with orange
					add di, 2
					mov bx, 4 
		Floor:		mov al, '_'
					mov ah, 0x05 ;Pink color because it matches the orange
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne Floor
					mov ah, 0x06 
					mov al, '|'
					mov word [es:di], ax ;Ending Wall
					add di, 2
					mov bx, 2 ;2 spaces are to be input
		SpaceLoop4:	mov al, ' '
					mov ah, 0x10 ;Blue background, can be turned to black with 0x00
					mov word [es:di], ax
					add di, 2
					sub bx, 1
					cmp bx, 0
					jne SpaceLoop4
					loop Walls
					sub si, 1
					cmp si, 0
					jne FloorSize
		
			exitBg:	pop bx
					pop di
					pop si
					pop cx
					pop ax
					pop es
					pop bp
					ret
					
	;-------------------------------------- ----------------------------------- ----------------------------------
	;2nd Part
	
	printroad:
		push es
		push ax
		push bx
		push si
		push di
		push cx
		push dx
		
		
		mov ax, 0xb800
		mov es, ax
		
		mov di, [RoadStart]
		road:
			mov word [es:di], 0xF220
			add di, 2
			cmp di, [SkyStart]
			jne road
			
		mov di, [RoadLines]
	
	roadLoop:
		mov al, '-'
		mov ah, 0x7E ;Yellow color for road lines with grey bg
		mov cx, 6		

		lines:
			mov word [es:di], ax
			add di, 2
			loop lines
		
		mov al, ' '
		mov ah, 0xF2
		mov cx, 6
		
		space:
			mov word [es:di], ax
			add di, 2
			loop space	
	
	add bx, 1	
	cmp bx, [linesAmount]
	jne roadLoop
	

	end:
		pop dx
		pop cx
		pop di
		pop si
		pop bx
		pop ax
		pop es
		ret
		
	;----------------------------------- -------------------------------------------
	;3rd Part
	
	scrollup:	
			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 80 ; load chars per row in ax
			mul byte [bp+4] ; calculate source position
			mov si, ax ; load source position in si
			push si ; save position for later use
			shl si, 1 ; convert to byte offset

			mov cx, 2000 ; number of screen locations
			sub cx, ax ; count of words to move

			mov ax, 0xb800
			mov es, ax ; point es to video base
			mov ds, ax ; point ds to video base
		
			xor di, di ; point di to top left column
			cld ; set auto increment mode
			rep movsw ; scroll up
			;[es:di] = [ds:si]

			mov ax, 0x0720 ; space in normal attribute
			pop cx ; count of positions to clear
			rep stosw ; clear the scrolled space
		
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 2


scrolldown:	
	push bp
	mov bp, sp
	
	push ax
	push es
	push ds
	push di
	push si
	push cx

	mov ax, 0xb800
	mov es, ax
	mov ds, ax
	
	mov di, [cs:ScreenSize]
	sub di, 2
	
	mov si, di
	mov ax, [cs:ColumnSize]
	add ax, ax
	mul word[bp + 4]
	
	sub si, ax
	
	mov cx, [cs:ScreenSize]
	shr cx, 1
	
	sub cx, 80 * 16

	shr ax, 1
	sub cx, ax
	
	std
	rep movsw
	
	mov cx, ax
	std
	mov ax, [cs:DaySkyColor]
	rep stosw
	
		; call delay
	
	
	pop cx
	pop si
	pop di
	pop ds
	pop es
	pop ax
	
	pop bp
	ret 2
			
removeOriginalRabbit:
	push bp
	mov bp, sp
	
	push es
	push ax
	push bx
	push cx
	push si
	push di
	
	;Checking if rabbit can jump
	canJump:
		mov ax, 0xb800
		mov es, ax
		xor si, si
		
		;checking above rabbits head
		mov si, [rabbithead]
		mov di, si
		sub di, 160
		
		push di
		call ScoreUpdateWrapper	
		
		mov dx, [DaySkyColor]
		mov bx, [carrot]
		
		cmp word [es:di], dx  ;check if above is sky(no brick)
		jne RemoveProcedure
		
		mov byte[endCondition], 1
		jmp popRemove
	
	RemoveProcedure:
	
		mov ax, 0xb800
		mov es, ax
		mov bx, [DaySkyColor]
		mov di, [rabbithead]
		xor cx, cx
		
		;remove rabbit head
		removeHead:
			mov word [es:di], bx
			
			add di, 2
			add cx, 1
			cmp cx, 2
			jne removeHead

		xor cx, cx
		mov di, [rabbitStart]
		
		;remove rabbit legs
		removeLegs:
			mov word [es:di], bx
			
			add di, 2
			add cx, 1
			cmp cx, 5
			jne removeLegs
		
		call HyperDelay
		
		call addNewRabbit
		call HyperDelay
		call HyperDelay
		
		push 3
		call scrolldown
		call spawnBricks
		; call checkBlueBrick
		call PrintCarrot
		
		mov cx, [ScoreToDisplay]
		push cx
		call PrintScore
		
		
		; call rabbitAndArea
		
	
popRemove:
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	
	ret 2
	
	
addNewRabbit:
	push es
	push di
	push si
	push ax
	push cx
	
	mov di, [rabbitStart]
	sub di, 480
	
	mov ax, 0xb800
	mov es, ax
	
nextproc:
	mov cx, 5
	mov ah, [RabbitColor]
	mov al, ' '
	
	pR:
		mov word [es:di], ax
		
		add di, 2
		loop pR
	

nextproc2:
	mov di, [rabbithead]
	sub di, 480
	mov cx, 2
	mov ah, byte [RabbitEyeColor]
	mov al, '.'
	
	pR2:
		mov word [es:di], ax
		
		add di, 2
		loop pR2
	
	pop cx
	pop ax
	pop si
	pop di
	pop es
	
	ret 


MoveRabbit:
	push bp
	mov bp, sp  ;bp + 4 = di postion
	
	push word [bp + 4]
	call removeOriginalRabbit
	
	pop ax
	
	ret 2

					
	rabbitAndArea:
				push bp
				mov bp, sp
				
				push es
				push ax
				push bx
				push di
				push si
				push dx
	
				mov ax, 0xb800
				mov es, ax
				mov di, 0	
				mov bx, 0xb800
				mov es, bx
				mov di, 0
					
				mov di, [SkyStart]
		
				mov ax, [DaySkyColor]   ;blue color for sky
				
		sky:
			mov word [es:di], ax
			
			add di, 2
			cmp di, 4000
			jne sky


				mov ah, 0x60  ;brown color
				mov al, ' ' 
				mov bh, 0x22  ;green color
				mov bl, ' '  ;wavy water wave pattern
				
				
		mov bx, [ScoreToDisplay]		;default score
		push bx
		call PrintScore		
	
		call PrintCarrot
		
		; push 3682
		; call clouds
		
		; push 3810
		; call clouds
		
		
		mov di, [GrassStart]
		
		; grass:
			; mov word [es:di], bx

			; add di, 2
			; cmp di, 3680  ;run from GrassStart till its line end
			; jne grass

		mov bh, 0x7F
		mov bl, ' '
			
		mov di, [rabbitStart]
		rabbit1:
			mov word [es:di], bx
			
			add di, 2
			cmp di, 3450
			jne rabbit1
			
		mov bh, 0xFF
		mov bl, '.'
		mov di, [rabbitStart]
		sub di, 158
		rabbit2:
			mov word [es:di], bx
			
			add di, 2
			cmp di, 3286
			jne rabbit2
			
		mov di, [rabbitStart]
		sub di, 318

		mov di, 3680
		; sand:
			; mov word [es:di], ax
			
			; add di, 2
			; cmp di, 4000
			; jne sand	
		
		mov dx, [RNG]
		; call spawnBricks
		; jmp WrapperPop
		
		; cmp dx, 0
		; je DefaultBrickPattern
		
		; cmp dx, 1
		; je InvertedBrickPattern
		
		; cmp dx, 2
		; je BreakBrickPattern
		
		; cmp dx, 3
		; je BreakBrickInvertedPattern
		
		; jmp ChangeBrickColor
		
		; DefaultBrickPattern:
			; mov cx, [tickcount]
			; xor ax, ax
			
			; cmp cx, 4
			; jle assignAXDef
			
			; cmp cx, 7
			; jge assignAXInv
			
			; jmp assignAXSpec
			
; brickShit:			
			; push bx
			; call PrintTopBrick	
		
			; push ax
			; call PrintMovingBrick
				
			; push ax
			; call PrintBottomBrick
			
			; jmp RNGAssign
			
			; assignAXDef:
				; mov ax, 0xEE20
				; mov bx, 0x4020
				; jmp brickShit
				
			; assignAXInv:
				; mov ax, 0x4020
				; mov bx, 0xEE20
				; jmp brickShit
				
			; assignAXSpec:
				; mov ax, 0xEE20
				; mov bx, 0x2F20
				; jmp brickShit

; RNGAssign:
			; mov word [RNG], 1
			
			; jmp WrapperPop
			
		
; InvertedBrickPattern:
			; mov cx, [tickcount]
			; xor ax, ax
			
			; cmp cx, 4
			; jle assignAXDef2
			
			; cmp cx, 7
			; jge assignAXInv2
			
			; jmp assignAXSpec2
			
; brickShit2:
			; push bx
			; call PrintTopBrick	
		
			; push ax
			; call PrintMovingBrick
				
			; push ax
			; call PrintBottomBrick
			
			; jmp RNGAssignZero
			
			; assignAXDef2:
				; mov ax, 0xEE20
				; mov bx, 0x4020
				; jmp brickShit2
				
			; assignAXInv2:
				; mov ax, 0x4020
				; mov bx, 0xEE20
				; jmp brickShit2
				
			; assignAXSpec2:
				; mov ax, 0xEE20
				; mov bx, 0x2F20
				; jmp brickShit2
			
		
			; cmp word [tickcount], 2
			; jle DefaultBrickPattern
			
			; cmp word [tickcount], 5
			; jle InvertedBrickPattern
			
			; cmp word [tickcount], 7
			; jle DefaultBrickPattern2
			
			; cmp word [tickcount], 9
			; jle InvertedBrickPattern2
			
			
	DefaultBrickPattern:
		
		push GREEN_COLOR
		call PrintTopBrick	
		
		push ORANGE_COLOR
		call PrintMovingBrick
		
		push RED_COLOR
		call PrintBottomBrick
		
		mov word [RNG], 1
	

	InvertedBrickPattern:
		push ORANGE_COLOR
		call PrintTopBrick
		
		push RED_COLOR
		call PrintMovingBrick
		
		push GREEN_COLOR
		call PrintBottomBrick

		mov word [RNG], 0

			jmp WrapperPop
			
			
		; BreakBrickPattern:
			; push 0x2F20
			; call PrintTopBrick
			
			; push 0xEE20
			; call PrintMovingBrick
			
			; push 0x4020
			; call PrintBottomBrick
			
			; mov word [RNG], 3
			
			; jmp WrapperPop		
			
			
		; BreakBrickInvertedPattern:
			; push 0x2F20
			; call PrintTopBrick
			
			; push 0xEE20
			; call PrintMovingBrick
			
			; push 0x4020
			; call PrintBottomBrick
			
			; mov word [RNG], 2
			
			; jmp WrapperPop
			
			
		; ChangeBrickColor:
			; ; Change the color of the bricks based on the current color
			; cmp word [currentColor], GREEN_COLOR
			; je  SetNextColorRed
			; cmp word [currentColor], RED_COLOR
			; je  SetNextColorOrange
			; cmp word [currentColor], ORANGE_COLOR
			; je  SetNextColorGreen

		; SetNextColorRed:
			; mov word [currentColor], RED_COLOR
			; jmp WrapperPop

		; SetNextColorOrange:
			; mov word [currentColor], ORANGE_COLOR
			; jmp WrapperPop

		; SetNextColorGreen:
			; mov word [currentColor], GREEN_COLOR
			; jmp WrapperPop
		
		
		WrapperPop:
			jmp Pop3rdSegment

; CHANGERNG3:
	; mov word [RNG], 3
	
	; jmp WrapperPop
	
; CHANGERNG2:
	; mov word [RNG], 2
	
	; jmp WrapperPop
	

PrintMovingBrick:
	push bp
	mov bp, sp
	
	push di
	push ax
	push dx
	push bx
	
	mov di, [rabbitStart]
	sub di, 320
	mov cx, [tickcount]
	add cx, cx
	sub di, cx ;26
	mov ax, [bp + 4]
	xor bx, bx
	xor dx, dx

	loopbrick3:
		mov word [es:di], ax
		
		add di, 2
		add bx, 1
		cmp bx, 16
		jne loopbrick3
	
	pop bx
	pop dx
	pop ax
	pop di
	pop bp
	
	ret 2
	

PrintTopBrick:
	push bp
	mov bp, sp
	
	push di
	push ax
	push dx
	push bx
	push cx
	
	xor bx, bx
	mov ax, [bp + 4]   ;brick color
	mov di, [rabbithead]
	sub di, 640
	mov cx, [tickcount]
	add cx, cx
	sub di, cx ;26

	loopbrick4:
		mov word [es:di], ax
		
		add di, 2
		add bx, 1
		cmp bx, 16
		jne loopbrick4
	
	pop cx
	pop bx
	pop dx
	pop ax
	pop di
	pop bp
	
	ret 2
	
	
PrintBottomBrick:
	push bp
	mov bp, sp
	
	push di
	push ax
	push dx
	push bx
	
	mov di, [rabbitStart]
	add di, 160
	sub di, 16
	mov ax, [bp + 4]
	xor bx, bx
	xor dx, dx

	loopbrick5:
		mov word [es:di], ax
		
		add di, 2
		add bx, 1
		cmp bx, 16
		jne loopbrick5
	
	pop bx
	pop dx
	pop ax
	pop di
	pop bp
	
	ret 2
	
	
	Pop3rdSegment:
		pop dx
		pop si
		pop di
		pop bx
		pop ax
		pop es
		pop bp
		
		ret
		

PrintCarrot:
	push bp
	mov bp, sp
	
	push es 
	push dx
	push si
	push di
	push ax
	
	xor dx, dx
	mov dx, [tickcount]
	cmp dx, 5
	jle carrotEnd
	
	xor di, di
	mov di, [rabbithead]
	sub di, 480
	
	mov ax, 0xb800
	mov es, ax
	
	Leaf:
		mov ah, 0x92
		mov al, '*'
		
		;1001 0010
	
		mov word [es:di], ax
	
	Body:
		add di, 160
		mov ah, 0x96
		mov al, 0x21
		
		;1001 0110
		
		mov word [es:di], ax
		
carrotEnd:	
	pop ax
	pop di
	pop si
	pop dx
	pop es
	pop bp
	
	ret 

			
PrintScore:
	push bp
	mov bp, sp
	
	push di
	push dx
	push es
	push si
	push cx
	push ax
	
	mov di, [SkyStart]
	
	mov dx, 0xb800
	mov es, dx

	mov si, Score
	mov cx, [ScoreLength]
	
	mov ah, 0x10
	nextchar:
		mov al, byte[si]
		
		mov word[es:di], ax
		add di, 2
		add si, 1
		
		loop nextchar
		
	
	mov ax, [SkyStart]
	add ax, 14
	push ax   ;pos
	mov ax, [bp + 4]
	push ax   ;number
	call printnum2
	

	pop ax
	pop cx
	pop si
	pop es
	pop dx
	pop di
	pop bp
	
	ret 2


clouds:
	push bp
	mov bp, sp
	
	pusha

	mov di, [bp + 4]
	mov cx, 14
	clouds1:
		mov ax, 0xFF20
		mov word [es:di], ax
		
		add di, 2
		loop clouds1
	
	sub di, 160
	sub di, 22
	mov cx, 8
	clouds2:
		mov ax, 0xFF20
		mov word [es:di], ax
		
		add di, 2
		loop clouds2
		
	sub di, 160
	sub di, 12
	mov cx, 4
	clouds3:
		mov ax, 0xFF20
		mov word [es:di], ax
		
		add di, 2
		loop clouds3
		
	popa
	
	pop bp
	ret 2
	

topBrickOrange:
	push ORANGE_COLOR
	call PrintTopBrick
	ret
	
topBrickRed:
	push RED_COLOR
	call PrintTopBrick	
	ret
	
topBrickGreen:
	push GREEN_COLOR
	call PrintTopBrick
	ret
	
topBrickCyan:
	push CYAN_COLOR
	call PrintTopBrick
	ret
	
	
MidBrickOrange:
	push ORANGE_COLOR
	call PrintMovingBrick
	ret
	
MidBrickRed:
	push RED_COLOR
	call PrintMovingBrick
	ret
	
MidBrickGreen:
	push GREEN_COLOR
	call PrintMovingBrick
	ret
	
MidBrickCyan:
	push CYAN_COLOR
	call PrintMovingBrick
	ret
	
	
BottomBrickOrange:
	push ORANGE_COLOR
	call PrintBottomBrick
	ret
	
BottomBrickRed:
	push RED_COLOR
	call PrintBottomBrick
	ret
	
BottomBrickGreen:
	push GREEN_COLOR
	call PrintBottomBrick
	ret
	
BottomBrickCyan:
	push CYAN_COLOR
	call PrintBottomBrick
	ret
	
spawnBricks:
	cmp byte [tickcount], 3
	jle topBrickCyan
	
	cmp byte [tickcount], 5
	jle topBrickRed
	
	cmp byte [tickcount], 7
	jle topBrickGreen
	
	cmp byte [tickcount], 8
	jge topBrickOrange
	
	ret
	
spawnMiddleBricks:
	cmp byte [tickcount], 3
	jle MidBrickOrange
	
	cmp byte [tickcount], 5
	jle MidBrickRed
	
	cmp byte [tickcount], 7
	jle MidBrickGreen
	
	cmp byte [tickcount], 8
	jge MidBrickCyan
	
	ret
	
spawnBottomBricks:
	cmp byte [tickcount], 3
	jle BottomBrickOrange
	
	cmp byte [tickcount], 5
	jle BottomBrickRed
	
	cmp byte [tickcount], 7
	jle BottomBrickGreen
	
	cmp byte [tickcount], 8
	jge BottomBrickCyan
	
	ret	
	
; DefaultBrickPatternWrapper:
	; DefaultBrickPattern:
		
		; push GREEN_COLOR
		; call PrintTopBrick	
		
		; push ORANGE_COLOR
		; call PrintMovingBrick
		
		; push RED_COLOR
		; call PrintBottomBrick
		
		; mov word [RNG], 1
		
		; ret
	
	; jmp WrapperPop

; InvertedBrickPatternWrapper:
	; InvertedBrickPattern:
		; push ORANGE_COLOR
		; call PrintTopBrick
		
		; push RED_COLOR
		; call PrintMovingBrick
		
		; push CYAN_COLOR
		; call PrintBottomBrick

		; mov word [RNG], 0
		
		; ret
		
	; jmp WrapperPop

; DefaultBrickPattern2Wrapper:
	; DefaultBrickPattern2:
		
		; push RED_COLOR
		; call PrintTopBrick	
		
		; push ORANGE_COLOR
		; call PrintMovingBrick
		
		; push GREEN_COLOR
		; call PrintBottomBrick
		
		; mov word [RNG], 1
		
		; ret
	
	; jmp WrapperPop

; InvertedBrickPattern2Wrapper:
	; InvertedBrickPattern2:
		; push ORANGE_COLOR
		; call PrintTopBrick
		
		; push GREEN_COLOR
		; call PrintMovingBrick
		
		; push CYAN_COLOR
		; call PrintBottomBrick

		; mov word [RNG], 0
		
		; ret
	
	; jmp WrapperPop

	
ScoreUpdateWrapper:	
	UpdateScore:
		push bp
		mov bp, sp
		pusha
		
		mov di, [bp + 4]
		sub di, 160
		mov bx, [carrot]
		
		cmp word [es:di], bx
		jne exitScore
		
		mov ax, [ScoreToDisplay]
		add ax, 10
		mov [ScoreToDisplay], ax
		call sound
	exitScore:
		popa
		pop bp
		ret 2
	
carrot: dw 0x9621

AssignBunnyWhite:
	mov bh, 0x7F
	mov byte [RabbitColor], 0x7F
	mov byte [RabbitEyeColor], 0xFF
	
	jmp proceed

assignBunnyYellow:
	mov bh, 0x60
	mov byte [RabbitColor], 0x60
	mov byte [RabbitEyeColor], 0xEF
	
	jmp proceed

print333:
	push 3996
	mov dx, [BlueBrickCount]
	push dx
	call printnum2
	
	ret
		
KeyboardInterrupt:
	push ax
	push es
	push di
	push dx
	push ds
	
	push cs
	pop ds

	; cmp byte[endCondition], 0x01
	; je KeyboardInterrupt_Exit

	mov ax, 0xB800
	mov es, ax

	in al, 0x60
	
	cmp byte al, 1
	je KeyboardInterrupt_Exit
	
	nnextcmp:
	cmp byte al, 10000001b ;esc key up
	jne nextcmp

	badscene:
	cmp byte[BusyMode], 0x01 ; if in busy (pause) mode
	je RIPGame
	
	call ConfirmationScreen

	jmp KeyboardInterrupt_Exit
	RIPGame:
		call EndScreen
		jmp KeyboardInterrupt_Exit

nextcmp:
	cmp byte al, [JumpKey]
	jne KeyboardInterrupt_NoMatch
	
MovingRabbit:
	cmp word [ScoreToDisplay], 10
	jge assignBunnyYellow
	
	; cmp word [ScoreToDisplay], 10
	; jl AssignBunnyWhite
	
proceed:
	push word [RabbitColor]
	call MoveRabbit
	
	jmp KeyboardInterrupt_Exit
	
	KeyboardInterrupt_NoMatch:
		cmp byte[BusyMode], 0x01
		jne KeyboardInterrupt_Exit
		
		KeyboardInterrupt_NoMatch_RestoreScreen:
			call restoreScreen
			mov byte[BusyMode], 0x00
			
	
	KeyboardInterrupt_Exit:
	
		mov al, 0x20
		out 0x20, al
		
		pop ds
		pop dx
		pop di
		pop es
		pop ax
		iret
	
RegisterJumpInterrupt:	
	; Save old keyboard interrupt
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:9 * 4]
	mov [OldKeyboardInterrupt], ax ; save offset of old routine
	mov ax, [es:9 * 4 + 2]
	mov [OldKeyboardInterrupt + 2], ax ; save segment of old routine

	cli ; disable interrupts
	mov word[es:9 * 4], KeyboardInterrupt ; store offset at n * 4
	mov [es:9 * 4 + 2], cs ; store segment at n * 4 + 2
	sti ; enable interrupts
	
	ret
	
RegisterInterrupts:
	call RegisterJumpInterrupt
	
	ret
	
	
timer:
	pusha
	push ds
	push cs
	pop ds
	
	mov ah, 02h
	int 0x1a
	
	cmp byte[endCondition], 1
	je ignore
	
	cmp byte[BusyMode], 1
	je ignore
	
	mov ax, 0xB800
	mov es, ax
		
		; cmp bx, [DaySkyColor]
		; je EndScreen
	push dx
	push 9
	call mod   ;gives random num from 1 to 9 in global variable (tickcount)
	
	push 3992
	push word[BlueBrickCount]
	call printnum2
	
	call PlaceToMove
	call PlaceToMoveCar
	call BrickToMove
	
	mov di, [rabbitStart]
	add di, 160
	
	cmp word [es:di], CYAN_COLOR
	jne assignZeroToBlueBrickCount   ;jackpot (random bug)
	
	cmp word [es:di], CYAN_COLOR
	jne ignore

	inc word [cs:BlueBrickCount]
	
	;debugging
debug:
	; mov cx, [SkyStart]
	; add cx, 20
	; push cx
	; mov cx, [BlueBrickCount]
	; push cx
	; call printnum2
	
	cmp word [BlueBrickCount], 70
	je removeBlueBrick
	
ignore:
	
	; inc word [cs:tickcount]; increment tick count
	; push word [cs:tickcount]

	; call printnum ; print tick count
	
	mov al, 0x20
	out 0x20, al ; end of interrupt
	
	pop ds
	popa
	
	iret ; return from interrupt
	
	
assignZeroToBlueBrickCount:
	mov word[BlueBrickCount], 0x00
	
	jmp debug
	
removeBlueBrick:
	xor di, di
	mov di, 3576
	mov bx, [DaySkyColor]
	mov cx, 30
	
	brickRemoval:	
		mov word [es:di], bx
		add di, 2
		loop brickRemoval
		
		
	call miniDelay
	call EndScreen
	jmp ignore
	
TimerInterrupt:
	xor ax, ax
	mov es, ax ; point es to IVT base

	mov ax, [es:8 * 4]
	mov [OldTimerInterrupt], ax ; save offset of old routine
	mov ax, [es:8 * 4 + 2]
	mov [OldTimerInterrupt + 2], ax ; save segment of old routine

	cli ; disable interrupts
	mov word [es:8*4], timer; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts

	mov dx, TimerInterrupt ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras
	
	ret
	

TimerInterruptWrapper:
	call TimerInterrupt
	
	ret
		
		
longDelay:
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay	
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay	
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay	
	call delay
	call delay
	call delay	
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	jmp IntroScreen
		
		
;Intro Screen using BIOS/DOS

IntroScreen:
	push bp
	mov bp, sp
	
	pusha
	
	mov ah, 0x13  ;write string in video mode
	mov al, 0x01
	
	mov bh, 0
	mov bl, 01000111b
	
	;es:bp = ds:message
	push ds
	pop es  ;es = ds
	
	mov cx, 54   ;length of title
	mov dx, 0x010B		;row & col
	mov bp, Title0
	int 0x10   ;BIOS Vid Service

	mov cx, 13   ;length of string0
	mov dx, 0x0620		;row & col
	mov bp, Intro0
	int 0x10   ;BIOS Vid Service
	
	mov cx, 24   ;length of string1
	mov dx, 0x0819   ;row & col
	mov bp, Intro1
	int 0x10   ;BIOS Vid Service
	
	mov cx, 24   ;length of string2
	mov bp, Intro2
	mov dx, 0x0919
	int 0x10   ;BIOS Vid Service	
	
	mov cx, 12   ;length of string3
	mov bp, Instruction00
	mov dx, 0x1121
	int 0x10   ;BIOS Vid Service
	
	mov cx, 52   ;length of string4
	mov bp, Instruction0
	mov dx, 0x1405
	int 0x10   ;BIOS Vid Service	
	
	mov cx, 70   ;length of string5
	mov bp, Instruction1
	mov dx, 0x1505
	int 0x10   ;BIOS Vid Service
	
	popa
	pop bp
	
	ret
	

;End Screen using BIOS/DOS

EndScreen:
	push bp
	mov bp, sp
	
	pusha
	
	mov byte[endCondition], 1
	mov ah, 0x13  ;write string in video mode
	mov al, 0x01
	
	mov bh, 0
	mov bl, 01000111b
	
	;es:bp = ds:message
	push ds
	pop es  ;es = ds
	
	call clrscr
	
	mov cx, 9   ;length of title
	mov dx, 0x051B		;row & col
	mov bp, Conclusion0
	int 0x10   ;BIOS Vid Service

	mov cx, 12   ;length of string0
	mov dx, 0x071B		;row & col
	mov bp, Conclusion1
	int 0x10   ;BIOS Vid Service
	
	mov ax, 0xB800
	mov es, ax
	
	push 1198
	mov ax, [ScoreToDisplay]
	push ax
	call printnum2
	
	popa
	pop bp
	
	ret
	
	
;Confirmation Screen using BIOS/DOS Services	
ConfirmationScreen:
	push bp
	mov bp, sp
	
	pusha
	push ds
	push es
	
	call saveScreen
	call clrscr
	
	mov ah, 0x13  ;write string in video mode
	mov al, 0x01
	
	mov bh, 0
	mov bl, 01000111b
	
	;es:bp = ds:message
	push ds
	pop es  ;es = ds
	
	mov cx, 30   ;length of title
	mov dx, 0x091B		;row & col
	mov bp, Confirmation0
	int 0x10   ;BIOS Vid Service

	mov cx, 47   ;length of string0
	mov dx, 0x0A15		;row & col
	mov bp, Confirmation1
	int 0x10   ;BIOS Vid Service	 

	mov byte[BusyMode], 0x01
	
popThis:
	pop es
	pop ds
	popa
	pop bp
	
	ret
	

printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 4000 ; point di to 70th column
	nextpos: pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
	ret 2


printnum2:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit2: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit2 ; if no divide it again
	mov di, [bp + 6] ; point di to 70th column
	nextpos2: pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos2; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
	ret 4
	

mod:
	push bp
	mov bp, sp
	pusha
	
	mov ax, [bp + 6]
	
	mov bx, [bp + 4]
	cmp bx, 0
	je end4
	
loop:
	sub ax, bx
	
	cmp ax, bx
	jae loop
	mov [cs:tickcount], ax
	; push ax
	; call printnum
	
end4:
	popa
	pop bp
	ret 4
	
	
mod2:
	push bp
	mov bp, sp
	pusha
	
	mov ax, [bp + 6]
	
	mov bx, [bp + 4]
	cmp bx, 0
	je end42
	
loop222:
	sub ax, bx
	
	cmp ax, bx
	jae loop222
	mov [cs:BlueBrickTemp], ax
	push ax
	call printnum
	
end42:
	popa
	pop bp
	ret 4
	
sound:
		pusha
		mov cx, 5
loopForMusic1:         mov al, 0b6h
	out 43h, al

	;load the counter 2 value for d3
	mov ax, 1fb4h
	out 42h, al
	mov al, ah
	out 42h, al

	;turn the speaker on
	in al, 61h
	mov ah,al
	or al, 3h
	out 61h, al
	mov al, ah
	out 61h, al


	;load the counter 2 value for a3
	mov ax, 152fh
	out 42h, al
	mov al, ah
	out 42h, al

	;turn the speaker on
	in al, 61h
	mov ah,al
	or al, 3h
	out 61h, al
	mov al, ah
	out 61h, al

		
	;load the counter 2 value for a4
	mov ax, 0A97h
	out 42h, al
	mov al, ah
	out 42h, al
		
	;turn the speaker on
	in al, 61h
	mov ah,al
	or al, 3h
	out 61h, al
	call delay
	mov al, ah
	out 61h, al

	 
	 loop loopForMusic1
	 
	 popa
	 ret
checkBlueBrick:
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xB800
	mov es, ax
	
	mov di, [rabbitStart]
	add di, 160
	mov ax, CYAN_COLOR
	
	cmp word [es:di], ax
	jne popthisShit
	
	
	; cmp word [BlueBrickCount], 80
	; call EndScreen
	
	
popthisShit:	
	popa
	pop bp
	ret
	
unhook_KeyBoardInterrupt:
	push ax
	push es
	
	xor ax, ax
	mov es, ax
	
	mov ax, [OldKeyboardInterrupt]								; read old offset in ax
	mov bx, [OldKeyboardInterrupt+2]								; read old segment in bx
	
	cli												; disable interrupts
	mov [es:9*4], ax								; restore old offset from ax
	mov [es:9*4+2], bx								; restore old segment from bx
	sti		
	
	pop es
	pop ax
	ret
	
unhook_TimerInterrupt:
	push ax
	push es
	
	xor ax, ax
	mov es, ax
	
	mov ax, [OldTimerInterrupt]								; read old offset in ax
	mov bx, [OldTimerInterrupt+2]								; read old segment in bx
	
	cli												; disable interrupts
	mov [es:8*4], ax								; restore old offset from ax
	mov [es:8*4+2], bx								; restore old segment from bx
	sti		
	
	pop es
	pop ax
	ret

savs:
	saveScreen:
		push es
		push ax
		push di
		push ds
		push si
		push cx
		
		mov di, screenBuffer
		xor si, si
		mov ax, cs
		mov es, ax
		mov ax, 0xb800 					; load video base in ax
		mov ds, ax
		
		; [es:di] = [ds:si]
		mov cx, (80*25*2)
		cld
		rep movsb
			
		pop cx
		pop si
		pop ds
		pop di
		pop ax
		pop es
		ret
	
	ret
	
restoreScreen:
	push es
	push ax
	push di
	push ds
	push si
	push cx
	
	xor di, di
	mov si, screenBuffer
	mov ax, cs
	mov ds, ax
	mov ax, 0xb800 					; load video base in ax
	mov es, ax
	
	; [es:di] = [ds:si]
	mov cx, (80*25*2)
	cld
	rep movsb
		
	pop cx
	pop si
	pop ds
	pop di
	pop ax
	pop es
	ret
	
	
IntroScreenWrapper:
	call IntroScreen
	call longDelay
	call longDelay
	; mov ah, 0 ; service 0 – get keystroke
	; int 0x16 ; call BIOS keyboard service
	
	ret
	
InterruptWrapper:
	call RegisterInterrupts
	call TimerInterruptWrapper	

	ret	
	
				
	MainDraw:	
				call printBackground
				call printroad
				call printCar
				call rabbitAndArea
				
				; call move   ;function that makes animations
				pangeLeteRaho:
					cmp byte[endCondition], 0
					je pangeLeteRaho
				
				call EndScreen
				ret
				
	Game:
		call IntroScreenWrapper
		call InterruptWrapper
		call MainDraw
					
		ret
		
start:
		call clrscr ; call the clrscr subroutine
		
		call Game
		
mainEnd:
		call unhook_KeyBoardInterrupt
		call unhook_TimerInterrupt
		
		mov ax, 0x4c00 ; terminate program
		int 0x21
