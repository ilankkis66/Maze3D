.486
IDEAL
;input: x,y of the man in the maze
;output: in bx, the index of the current place of the man in the maze
Macro CalcIndex
	push ax
	mov al,mazecol
	mul [Yman]
	add al,[Xman]
	mov bx,ax
	pop ax
endm CalcIndex

;input: cx,dx - x,y of the pixel in screen
;output: in bx, the index of this pixel in es 
Macro CalcIndexInScreen
	mov bx,dx
	shl dx,6
	shl bx,8
	add bx,dx
	add bx,cx
	shr dx,6
endm CalcIndexInScreen
;---------------------------------------------;
; case: DeltaY is bigger than DeltaX		  ;
; input: p1X p1Y,		            		  ;
; 		 p2X p2Y,		           		      ;
;		 Color                                ;
; output: line on the screen                  ;
;---------------------------------------------;

Macro DrawLine2DDY p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1X]
	cmp ax, [p2X]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2Y]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2Y]
	sub bx, [p1Y]
	absolute bx
	mov cx, [p2X]
	sub cx, [p1X]
	absolute cx
	mov ax, [p2Y]
lp:
	pusha
	call PIXEL
	popa
	inc [pointY]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = (p2Y - p1Y) = deltay
	add [pointX], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2X - p1X) = daltax
	cmp [pointY], ax ; ax = p2Y
	jne lp
	call PIXEL
ENDM DrawLine2DDY
;---------------------------------------------;
; case: DeltaX is bigger than DeltaY		  ;
; input: p1X p1Y,		            		  ;
; 		 p2X p2Y,		           		      ;
;		 Color -> variable                    ;
; output: line on the screen                  ;
;---------------------------------------------;
Macro DrawLine2DDX p1X, p1Y, p2X, p2Y
	local l1, lp, nxt
	mov dx, 1
	mov ax, [p1Y]
	cmp ax, [p2Y]
	jbe l1
	neg dx ; turn delta to -1
l1:
	mov ax, [p2X]
	shr ax, 1 ; div by 2
	mov [TempW], ax
	mov ax, [p1X]
	mov [pointX], ax
	mov ax, [p1Y]
	mov [pointY], ax
	mov bx, [p2X]
	sub bx, [p1X]
	absolute bx
	mov cx, [p2Y]
	sub cx, [p1Y]
	absolute cx
	mov ax, [p2X]
lp:
	pusha
	call PIXEL
	popa
	inc [pointX]
	cmp [TempW], 0
	jge nxt
	add [TempW], bx ; bx = abs(p2X - p1X) = deltax
	add [pointY], dx ; dx = delta
nxt:
	sub [TempW], cx ; cx = abs(p2Y - p1Y) = deltay
	cmp [pointX], ax ; ax = p2X
	jne lp
	call PIXEL
ENDM DrawLine2DDX


Macro absolute a
	local l1
	cmp a, 0
	jge l1
	neg a
l1:
Endm

MODEL small
STACK 256

DATASEG

;						│  			 				    │
;	0				4	│  			8	___			12	└── 	

;		  │		      	│  │	  	       │    		│ │
;	1	  │			5	│  │		9   ___│	    13 	└─┘	

;		───             ┌──             ─── 		    ┌──
;   2               6   │           10	___	        14	└──

;		──┐             ┌──┐            ──┐             ┌─┐
;   3 	  │			7   │  │ 	 	11	__│		    15	└─┘

	colormaze equ 15
	wincolor equ 12
	
	;=============================
	; first maze
	;=============================
	mazecol equ 6
	mazerow equ 6
	maxsize equ mazecol
	maze db 6,11,6,3,6,3		
		 db 5,6,9,13,5,5
		 db 4,8,10,10,9,5
		 db 5,6,10,11,15,5
		 db 5,5,6,11,14,1
		 db 12,8,8,10,11,13
	Xman db 4
	Yman db 5
	DirMan db 3
	Xwin equ 5
	ywin equ 5
		 
	; =============================
	; second maze
	; =============================
	; mazecol equ 6
	; mazerow equ 6
	; maxsize equ mazecol		 
	; maze db 6,10,10,2,11,7		
		 ; db 4,10,11,5,14,1
		 ; db 5,14,10,0,11,5
		 ; db 4,11,14,0,11,5
		 ; db 5,14,10,9,6,1
		 ; db 12,10,10,10,9,13	
	; Xman db 0
	; Yman db 0
	; DirMan db 1
	; Xwin equ 5
	; ywin equ 5

	; =============================
	 ; third maze
	; =============================
	; mazecol equ 7
	; mazerow equ 4
	; maxsize equ mazecol	 
	; maze db 14,2,10,2,10,10,3		
		 ; db 7,13,7,5,14,3,5
		 ; db 4,2,9,4,11,5,13
		 ; db 12,8,10,8,10,8,11
	; Xman db 0
	; Yman db 0
	; DirMan db 1
	; Xwin equ 6
	; ywin equ 3
		 
	ArrX dw  0,22,55,79,97,110,120,127,132
	ArrY dw 0,13,34,49,60,69,74,79,82
	ArrLen dw mazecol+1 dup (?)		
	
	propleft equ 10
	proptop equ 0
	
	SizeEachWall equ 6 ;when draw the map, each wall is not one pixel
	leftmap equ 320-mazecol*SizeEachWall-150
	topmap equ 200-mazerow*SizeEachWall-5
	colormap equ 6
	
	EastMask equ 1
	NorthMask equ 2
	WestMask equ 4
	SouthMask equ 8
	
	printx db " x: $"
	printy db " y: $"
	printdir db " dir: $"
	newline db 10,13,'$'
	WinMsg db "Victory Royale       ",'$'
	AnyKey db "press any key to continue",'$'
	insturction db "Welcome to Maze3D game",10,13
				db "You have some option while the game:",10,13
				db "> for turn right",10,13
				db "< for turn left",10,13
				db "^ for go forward",10,13
				db "v for do back",10,13
				db "P for print your x,y in the maze and your direction" ,10,13
				db "The directions are:  0 up, 1 right, 2 down, 3 left",10,13
				db "M for draw map of the maze, in the maze you will show:",10,13
				db "In purple your location and with an arrow your dir",10,13 
				db "In red the location that you have to go there for win",10,13
				db "Esc for exit the game",10,13
				db "If you show red frame around white full rect, that the winnig place",10,13,'$'
	
	VectorLeft db maxsize dup(?)
	VectorRight db maxsize dup(?)
	
	TempW dw ?
    pointX dw ? 
	pointY dw ?
    point1X dw ? 
	point1Y dw ?
    point2X dw ? 
	point2Y dw ?
	Color db ?
	
	Deep db 0
	cntdeep db 0
	ifwin db 0
	
	DirforVector dw CalcVector_dir0,CalcVector_dir1,CalcVector_dir2,CalcVector_dir3
	DirforDeep dw Deep_Dir0,Deep_Dir1,Deep_Dir2,Deep_Dir3	
	MoveBack dw Goback_dir0,Goback_dir1,Goback_dir2,Goback_dir3
	DrawDir dw DrawDir0,DrawDir1,DrawDir2,DrawDir3
	
	ifProp db 0 ;if is odd print, even not print
	ppressed db ?
	mpressed db ?
	ifmap db 0  ;if is odd draw the map, even not draw
	notdraw db 0
	
	cantmove db 0
	move db 1
	outside db 0
	
	tmpw dw ?
	limit db ?

	mapleft dw ?
	maptop dw ?	
	
	showwin1 db 0

CODESEG
start:
    mov ax, @data
    mov ds, ax
	
	mov ax,3h
	int 10h ; set text mode
	; set text mode for clear screen
	
	mov dx,offset insturction
	mov ah,9
	int 21h
	
	mov dx,offset AnyKey
	mov ah,9
	int 21h
	
	mov ah,0
	int 16h 
	
	mov ax,13h
	int 10h ; set graphic mode
	
	mov ax,0a000h
	mov es,ax ; set graphic mode

Game:	
	cmp [notdraw],1 ;check if don't need to draw the maze
	je keyinput
	call CalcDeep
	call CalcVector
	mov al,colormaze ;color
	call Drawmaze
keyinput:
	cmp [ifwin],1
	je GotPrize
	mov ah,0
	int 16h
	cmp ah,1  ;scan code of esc, for exit the game
	je exit
	cmp ah,19h ;scan code of P, for print x,y,dir
	je Pclicked
	cmp ah,32h ;scan code of m, for draw the map
	je Mclicked
	cmp ah,48h ;scan code of up arrow
	je onlymove
	cmp ah,50h ;scan code of down arrow
	je onlymove
	cmp ah,4bh   ;scan code of left arrow
	je turn
	cmp ah,4dh   ;scan code of right arrow
	je turn
	jmp keyinput
turn:	
	call ChangeLook
	jmp aftermove
onlymove:
	mov [move],0
	mov [cantmove],0
	Call MoveInmaze
	cmp [move],0 ;if the man couldn't move, return to the key input
	je keyinput	
	
	;check if the man arrive the winning place
	cmp [Xman],Xwin
	jne aftermove
	cmp [Yman],Ywin
	jne aftermove
	
	mov [ifwin],1
aftermove:
	call ClearScreen
	mov [ppressed],0
	mov [mpressed],0
	jmp CheckPropAfterClearScreen
NextTime:	
	mov [notdraw],0
	jmp CheckMapAfterClearScreen
	;jmp Game

GotPrize:
	;set cursor position
	mov ah,2
	mov bh,0
	mov dh,proptop
	mov dl,propleft
	int 10h
	mov dx,offset WinMsg
	mov ah,9
	int 21h
	;cursor one line lower, for print press any key to continue
	mov ah,2
	mov bh,0
	mov dh,proptop+1
	mov dl,propleft
	int 10h
	mov dx,offset AnyKey
	mov ah,9
	int 21h
	
	mov ah,0
	int 16h
	
exit:
	mov ax,03h
	int 10h ; set text mode

    mov ax, 4C00h
    int 21h
	
Pclicked:	
	mov [ppressed],1
	inc [ifProp]
CheckPropAfterClearScreen:	
	and [ifProp],1 ;check if need to print the prop
	jz return
	call PrintProp
return:	
	cmp [ppressed],1  ;if print beause P pressed, so return to the key input
	je keyinput
	jmp NextTime
	
Mclicked:
	mov [mpressed],1
	inc [ifmap]
CheckMapAfterClearScreen:	
	and [ifmap],1  ;check if need to draw the map
	jz return2
	cmp [mpressed],1
	je drawthemap
	;draw the maze, for the maze will be the background
	call CalcDeep
	call CalcVector
	mov al,colormaze ;color
	call Drawmaze
	mov [notdraw],1 ;for no draw the maze in the main
drawthemap:	
	mov cx,leftmap
	mov dx,topmap
	mov al,colormap
	call DrawMap
return2:	
	cmp [mpressed],1    ;if draw beause M pressed, so return to the key input
	je keyinput
	jmp Game		
	
;===========================
; procedures
;===========================
;================================================
; Description - if show the win point showwin1=1
;               else showwin1=0
; INPUT: Xman,Yman,deep,DirMan
; OUTPUT: showwin1 
; Register Usage: AX  
;================================================
proc showwin
	push ax
	;dir 0 is Xman=Xwin
	cmp [DirMan],0
	jne @@check1
	cmp [Xman],Xwin
	jne @@check1
	;if Yman-deep=Yman so see thw winning place
	mov al,[Yman]
	sub al,[deep]
	cmp al,Ywin
	jne @@check1
	mov [showwin1],1
	jmp @@ret
@@check1:
	;dir 1 is Yman=Ywin	
	cmp [DirMan],1
	jne @@check2
	cmp [Yman],Ywin
	jne @@check2
	;if Yman+deep=Yman so see thw winning place
	mov al,[Xman]
	add al,[deep]
	cmp al,Xwin
	jne @@check2
	mov [showwin1],1
	jmp @@ret
@@check2:	
	;dir 2 is Xman=Xwin
	cmp [DirMan],2
	jne @@check3
	cmp [Xman],Xwin
	jne @@check3
	;if Yman+deep=Yman so see thw winning place
	mov al,[Yman]
	add al,[deep]
	cmp al,Ywin
	jne @@check3
	mov [showwin1],1
	jmp @@ret
@@check3:
	;dir 3 is Yman=Ywin
	cmp [DirMan],3
	jne @@ret
	cmp [Yman],Ywin
	jne @@ret
	;if Xman+deep=Xman so see thw winning place
	mov al,[Xman]
	sub al,[deep]
	cmp al,Xwin
	jne @@ret
	mov [showwin1],1
@@ret:	
	pop ax
	ret
endp showwin	
;================================================
; Description - print to the screen map of the maze in 2D
; INPUT: cx - x left
;        dx - y top 
;        al - color
; OUTPUT: screen 
; Register Usage: none beacuse all saved, there are pusha,popa  
;================================================
proc DrawMap
	pusha 
	mov [mapleft],cx
	mov [maptop],dx
	xor bx,bx
	xor di,di
@@checkwall:	
	test [maze+di],NorthMask ;check if there is wall up
	jz @@checkleft
	mov si,SizeEachWall
	call DrawHorizontalLine ;draw the up wall
@@checkleft:
	test [maze+di],WestMask ;check if there is wall left
	jz @@nexttime
	mov si,SizeEachWall+1
	call DrawVerticalLine ;draw the left wall
@@nexttime:
	add cx,SizeEachWall
	inc di
	push ax
	push dx
	push di
	mov ax,di
	mov di,mazecol
	xor dx,dx ;when div words the result is dx:ax/register
	div di   
	cmp dx,0   ;check if di mod mazecol is 0, for go next line
	pop di
	pop dx
	pop ax
	jne @@checkwall
	
	mov si,SizeEachWall+1 ; if it is the end of the line draw wall in right
	call DrawVerticalLine

	add cx,320-SizeEachWall*mazecol ;the start of next line
	add dx,SizeEachWall-1           ;the top of next line
	
	inc bx
	cmp bx,mazerow ;check if it is the last line
	jne @@iffinsh
	mov si,SizeEachWall*mazecol ; if it is the last line draw wall down
	call DrawHorizontalLine     ;draw the whole lower line
@@iffinsh:	
	cmp di,mazecol*mazerow ;check if all the array checked
	jb @@checkwall
	
	push cx
	mov cx,mazerow
	sub cl,[Yman]
	;find the y of the place that the man is
	;dx is the y of the last line
@@subdx:
	cmp cl,0
	je @@changeX
	sub dx,SizeEachWall
	dec cl
	jmp @@subdx
	
@@changeX:	
	pop cx
	push dx
	mov dl,[Xman]
	;find the x of the place that the man is
	;cx is the start of the last line
@@addcx:	
	cmp dl,0
	je @@DrawPoint
	add cx,SizeEachWall
	dec dl
	jmp @@addcx
@@DrawPoint:	
	pop dx
	
	;draw purple rect in the place that the man is
	inc cx ;for not draw on the wall
	inc dx
	mov ax,SizeEachWall-1 ;number of rowes
@@nextline:
	push ax	
	mov al,9 ;purple color
	mov si,SizeEachWall-1 ;length of the line (number of colums)
	call DrawHorizontalLine
	inc dx
	pop ax
	dec ax
	cmp ax,0
	jne @@nextline
	
	
	sub dx,SizeEachWall-1 ;return to the start of the line
	mov bl,[DirMan]
	xor bh,bh
	shl bx,1  ;offset is word
	call [DrawDir+bx] ;draw an arrow that fits the direction
	
	;find the x of the winning point
	push [mapleft]
	mov cx,Xwin
@@addforX:
	cmp cx,0
	je @@addforY	
	add [mapleft],SizeEachWall
	dec cx
	jmp @@addforX
@@addforY:
	push [maptop]
	mov cx,Ywin
	;find the y of the winning point
@@changey:	
	cmp cx,0
	je @@DrawWin	
	add [maptop],SizeEachWall
	dec cx
	jmp @@changey
@@DrawWin:
	mov cx,[mapleft]
	mov dx,[maptop]
	
	pop [maptop]
	pop [mapleft]
	
	;draw red rect in the winning place 
	inc cx
	inc dx
	mov ax,SizeEachWall-1
@@nextline2:
	push ax	
	mov al,wincolor
	mov si,SizeEachWall-1
	call DrawHorizontalLine
	inc dx
	pop ax
	dec ax
	cmp ax,0
	jne @@nextline2
	
	popa
	ret 
endp DrawMap	

;================================================
;this description is for the next 4 procedures
; Description - print to the screen an arrow that showes the dir
; INPUT: cx - x left
;        dx - y top 
;        al - color 
; OUTPUT: screen 
; Register Usage: cx,dx,bx
;================================================
proc DrawDir0
	push cx
	push dx
	push bx
	;draw the vertical line in the arrow
	add cx,SizeEachWall/2-1
	mov al,0
	mov si,SizeEachWall-1
	call DrawVerticalLine
	CalcIndexInScreen
	sub bx,321
	mov cx,SizeEachWall/2
	;draw the right line in the arrow
@@drawright:
	add bx,321	
	mov [byte ptr es:bx],0
	loop @@drawright
	sub bx,SizeEachWall-2
	mov [byte ptr es:bx],0
	mov cx,SizeEachWall/2-1
	;draw the left line in the arrow
@@drawleft:	
	sub bx,319
	mov [byte ptr es:bx],0
	loop @@drawleft
	pop bx
	pop dx
	pop cx
	ret
endp DrawDir0	

proc DrawDir1
	push cx
	push dx
	push bx
	;draw the horizontal line in the arrow
	add dx,SizeEachWall/2-1
	mov al,0
	mov si,SizeEachWall-1
	call DrawHorizontalLine
	add cx,SizeEachWall-2
	CalcIndexInScreen
	sub bx,319
	mov cx,SizeEachWall/2
	;draw the right line in the arrow
@@drawright:
	add bx,319	
	mov [byte ptr es:bx],0
	loop @@drawright
	sub bx,(SizeEachWall-2)*320
	mov [byte ptr es:bx],0
	mov cx,SizeEachWall/2-1
	;draw the left line in the arrow
@@drawleft:	
	add bx,321
	mov [byte ptr es:bx],0
	loop @@drawleft
	pop bx
	pop dx
	pop cx
	ret
endp DrawDir1

proc DrawDir2
	push cx
	push dx
	push bx
	;draw the vertical line in the arrow
	add cx,SizeEachWall/2-1
	mov al,0
	mov si,SizeEachWall-1
	call DrawVerticalLine
	add dx,SizeEachWall-2
	CalcIndexInScreen
	add bx,321
	mov cx,SizeEachWall/2
	;draw the left line in the arrow
@@drawleft:	
	sub bx,321
	mov [byte ptr es:bx],0
	loop @@drawleft
	add bx,SizeEachWall-2
	mov [byte ptr es:bx],0
	mov cx,SizeEachWall/2-1
	;draw the right line in the arrow
@@drawright:	
	add bx,319
	mov [byte ptr es:bx],0
	loop @@drawright
	pop bx
	pop dx
	pop cx
	ret
endp DrawDir2	

proc DrawDir3
	push cx
	push dx
	push bx
	;draw the horizontal line in the arrow
	add dx,SizeEachWall/2-1
	mov al,0
	mov si,SizeEachWall-1
	call DrawHorizontalLine
	CalcIndexInScreen
	sub bx,321
	mov cx,SizeEachWall/2
	;draw the left line in the arrow
@@drawleft:	
	add bx,321
	mov [byte ptr es:bx],0
	loop @@drawleft
	sub bx,(SizeEachWall-2)*320
	mov [byte ptr es:bx],0
	mov cx,SizeEachWall/2-1
	;draw the right line in the arrow
@@drawright:	
	add bx,319
	mov [byte ptr es:bx],0
	loop @@drawright
	pop bx
	pop dx
	pop cx
	ret
endp DrawDir3

;================================================
; Description - clear the whole scren (draw a 320*200 black rect) 
; INPUT:none 
; OUTPUT:screen
; Register Usage: cx,bx
;================================================
proc ClearScreen
	push cx
	push bx
	
	mov cx,320*100
	xor bx,bx
@@clearscreen:
	mov [word ptr es:bx],0
	add bx,2 ;every time draw word
	loop @@clearscreen 
	
	pop bx
	pop cx
	ret
endp ClearScreen	


;================================================
; Description - print the properties of the man: x,y,dir 
; INPUT:Xman,Yman,DirMan 
; OUTPUT:screen
; Register Usage: none beacuse all saved, there are pusha,popa  
;================================================
proc PrintProp
	pusha
	;check if need to print the prop
	mov ah,2
	mov bh,0
	mov dh,proptop
	mov dl,propleft
	int 10h
	
	;print x
	mov dx,offset printx
	mov ah,9 
	int 21h
	xor ah,ah
	mov dl,[Xman]
	add dl,'0' ;add the y the ascii value of 0
	mov ah,2
	int 21h
	mov ah,2
	;print y
	mov dx,offset printy
	mov ah,9 
	int 21h
	xor ah,ah
	mov dl,[Yman]
	add dl,'0' ;add the y the ascii value of 0
	mov ah,2
	int 21h
	;print direction
	mov dx,offset printdir
	mov ah,9 
	int 21h
	mov dl,[DirMan]
	add dl,'0' ;add the y the ascii value of 0
	mov ah,2
	int 21h
	
	popa
	ret 
endp PrintProp	

;================================================
; Description - draw the maze
; INPUT:Xman,Yman,DirMan,deep,VectorLeft,VectorRight 
; OUTPUT:screen
; Register Usage: none beacuse all saved, there are pusha,popa  
;================================================
proc Drawmaze
	pusha
	
	xor bh,bh
	mov bl,[deep]
	inc bl
	shl bx,1
	mov cx,[ArrX+bx]
	mov dx,[ArrY+bx]
	mov [tmpw],dx
	shl [tmpw],1
	mov si,200
	sub si,[tmpw] ; height = 200-2y
	mov [tmpw],cx
	shl [tmpw],1
	mov di,320
	sub di,[tmpw] ; width = 320-2x
	mov [showwin1],0
	call showwin ;this proc put 1 in showwin1 if the man sees the winning place	
	call FullRect
@@drawdiag: 		
	pusha ;for save the x,y,width and height of the rect for draw the red frame
	mov [color],al
	mov [point1X],0
	mov [point1Y],0
	mov [point2X],318
	mov [point2Y],198
	call DrawLine2D ;draw diag from 0,0 to 318,198
	
	mov [point1X],318
	mov [point1Y],0
	mov [point2X],0
	mov [point2Y],198
	call DrawLine2D ;draw diag from 318,0 to 0,198
	popa
	
	;cmp [showwin1],1 
	;jne @@finishrect
	;mov al,wincolor ;if show the winning place, the rect has to be in wincolor,red
	;call FullRect 
	cmp [showwin1],1 ;if show the winning place, showwin1=1
	jne @@finishrect ;showwin1=0, dont show the red frame
	;draw red frame around the full rect
	dec cx 
	dec dx
	add si,2
	add di,2
	mov al,wincolor
	call EmptyRect
@@finishrect:	
	mov al,colormaze
	xor di,di
@@Nextline:
	;vertical line in the left side
	add di,2
	mov al,[color]
	mov cx,[ArrX+di]
	mov dx,[ArrY+di]
	mov [tmpw],dx
	shl [tmpw],1
	mov si,200
	sub si,[tmpw] ;len = 200-2y
	mov [ArrLen+di],si ;save the length for draw right
	call DrawVerticalLine
	
	mov bx,di
	shr bx,1
	dec bx
	cmp [VectorLeft+bx],0 ;check if there is a wall in left
	jne @@Draw&CheckRight
@@thereisTurn:
	pusha
	
	;draw the turn(2 horizontal line)
	mov cx,[ArrX+di-2]
	mov dx,[ArrY+di]
	mov si,[ArrX+di]
	sub si,cx
	call DrawHorizontalLine
	add dx,[ArrLen+di]
	dec dx
	call DrawHorizontalLine
	
	;erasure the diag line (black rect)
	mov cx,[ArrX+di-2]
	inc cx
	mov dx,[ArrY+di-2]
	dec dx
	mov si,[ArrY+di]
	sub si,dx
	mov bx,di
	mov di,[ArrX+bx]
	sub di,cx
	add di,2
	mov al,0
	call FullRect ;the upper rect
	mov dx,[ArrY+bx]
	add dx,[ArrLen+bx]
	inc di
	call FullRect ;the lower rect
	
	popa
	
@@Draw&CheckRight:
	;vertical line in the right side
	mov [tmpw],320
	sub [tmpw],cx
	mov cx,[tmpw]
	mov dx,[ArrY+di]
	mov si,[ArrLen+di]
	call DrawVerticalLine ;check if there is a wall in left
	
	cmp [VectorRight+bx],0 ;check if there is a wall in left
	jne @@NextTime
	
	pusha
	;draw the turn(2 horizontal line)
	mov cx,320
	sub cx,[ArrX+di]
	mov dx,[ArrY+di]
	mov si,[ArrX+di]
	sub si,[ArrX+di-2]
	call DrawHorizontalLine
	add dx,[ArrLen+di]
	dec dx
	call DrawHorizontalLine
	
	;erasure the diag line (black rect)
	mov cx,320
	sub cx,[ArrX+di]
	dec cx
	mov dx,[ArrY+di-2]
	dec dx
	mov si,[ArrY+di]
	sub si,dx
	mov bx,di
	mov di,[ArrX+bx]
	sub di,[ArrX+bx-2]
	inc di
	mov al,0
	call FullRect ;the upper rect
	mov dx,[ArrY+bx]
	add dx,[ArrLen+bx]
	call FullRect ;the lower rect

	popa

@@NextTime:	
	;the loop has to run deep times
	mov ax,di
	shr ax,1
	dec ax
	cmp al,[deep]
	jne @@Nextline
	popa
	ret
endp Drawmaze	




;================================================
; Description - call for another procedure that calc the vector
; INPUT:DirMan 
; OUTPUT:VectorLeft,VectorRight
; Register Usage:bx
;================================================
proc CalcVector
	push bx
	
	xor bh,bh
	mov bl,[DirMan]
	shl bl,1 ;mul bl in 2 beacuse offset is word
	call [DirforVector+bx]

	pop bx
	ret
endp CalcVector	

;================================================
;this description is for the next 4 procedures
; Description - calc the vector, accordingly the DirMan 
; INPUT:Xman,Yman,DirMan 
; OUTPUT:VectorLeft,VectorRight
; Register Usage:none beacuse all saved, there are pusha,popa 
;================================================
proc CalcVector_dir0
	pusha
	CalcIndex 
	xor si,si
	xor cx,cx
	mov cl,[deep]
	inc cl
@@checkvector:
	mov al,[maze+bx]
	mov [VectorLeft+si],al
	and [VectorLeft+si],WestMask ;left side of dir 0 is west wall
	jz @@Rightside
	mov [VectorLeft+si],1
@@Rightside:
	mov [VectorRight+si],al
	and [VectorRight+si],EastMask ;right side of dir 0 is east wall
	jz @@NextLoop
	mov [VectorRight+si],1
@@NextLoop:	
	inc si
	sub bx,mazecol
	loop @@checkvector
	
	popa
	ret
endp CalcVector_dir0

proc CalcVector_dir1
	pusha
	CalcIndex
	xor si,si
	xor cx,cx
	mov cl,[deep]
	inc cl
@@checkvector:
	mov al,[maze+bx]
	mov [VectorLeft+si],al
	and [VectorLeft+si],NorthMask ;left side of dir 1 is north wall
	jz @@Rightside
	mov [VectorLeft+si],1
@@Rightside:
	mov [VectorRight+si],al
	and [VectorRight+si],SouthMask ;right side of dir 1 is south wall
	jz @@NextLoop
	mov [VectorRight+si],1
@@NextLoop:	
	inc si
	inc bx
	loop @@checkvector
	
	popa
	ret
endp CalcVector_dir1

proc CalcVector_dir2
	pusha
	CalcIndex
	xor si,si
	xor cx,cx
	mov cl,[deep]
	inc cl
@@checkvector:
	mov al,[maze+bx]
	mov [VectorLeft+si],al
	and [VectorLeft+si],EastMask ;left side of dir 2 is east wall
	jz @@Rightside
	mov [VectorLeft+si],1
@@Rightside:
	mov [VectorRight+si],al
	and [VectorRight+si],WestMask ;right side of dir 2 is west wall
	jz @@NextLoop
	mov [VectorRight+si],1
@@NextLoop:	
	inc si
	add bx,mazecol
	loop @@checkvector
	
	popa
	ret
endp CalcVector_dir2

proc CalcVector_dir3
	pusha
	CalcIndex
	xor si,si
	xor cx,cx
	mov cl,[deep]
	inc cl
@@checkvector:
	mov al,[maze+bx]
	mov [VectorLeft+si],al
	and [VectorLeft+si],SouthMask ;right side of dir 1 is south wall
	jz @@Rightside
	mov [VectorLeft+si],1
@@Rightside:
	mov [VectorRight+si],al
	and [VectorRight+si],NorthMask ;right side of dir 3 is north wall
	jz @@NextLoop
	mov [VectorRight+si],1
@@NextLoop:	
	inc si
	dec bx
	loop @@checkvector
	
	popa
	ret
endp CalcVector_dir3



;================================================
; Description - call for another procedure that calc the deep
; INPUT:DirMan 
; OUTPUT:deep
; Register Usage:bx
;================================================
proc CalcDeep
	push bx
	
	xor bh,bh
	mov bl,[DirMan]
	shl bl,1 ;mul bl in 2 beacuse offset is word
	mov [cntdeep],0
	call [DirforDeep+bx]

	pop bx
	ret
endp CalcDeep										

;================================================
;this description is for the next 4 procedures
; Description - calc the depp, accordingly the DirMan 
; INPUT:Xman,Yman,DirMan 
; OUTPUT:deep
; Register Usage:none beacuse all saved, there are pusha,popa 
;================================================
proc Deep_Dir0
	pusha
	
	CalcIndex
	
	xor cx,cx
	mov cl,[Yman]
@@checkwall:
	test [maze+bx],NorthMask ;00000010b ;the wall that you can't pass is north wall
	jnz @@ret 
	inc [cntdeep]
	sub bx,mazecol
	loop @@checkwall
@@ret:
	mov al,[cntdeep]
	mov [deep],al
	popa
	ret
endp Deep_Dir0	

proc Deep_Dir1
	pusha
	mov [limit],mazecol
	
	CalcIndex
	xor cx,cx
	mov cl,[limit]
	sub cl,[Xman]
@@checkwall:	
	test [maze+bx],EastMask ;00000001b the wall that you can't pass is east wall
	jnz @@ret 
	inc [cntdeep]
	inc bx
	loop @@checkwall
@@ret:
	mov al,[cntdeep]
	mov [deep],al
	popa
	ret
endp Deep_Dir1

proc Deep_Dir2
	pusha
	mov [limit],mazerow
	
	CalcIndex
	
	xor cx,cx
	mov cl,[limit]
	sub cl,[Yman]
@@checkwall:	
	test [maze+bx],SouthMask ;00001000b the wall that you can't pass is south wall
	jnz @@ret 
	inc [cntdeep]
	add bx,mazecol
	loop @@checkwall
@@ret:
	mov al,[cntdeep]
	mov [deep],al
	popa
	ret
endp Deep_Dir2

proc Deep_Dir3
	pusha
	
	CalcIndex
	
	xor cx,cx
	mov cl,[Xman]
@@checkwall:
	test [maze+bx],WestMask ;00000100b the wall that you can't pass is west wall
	jnz @@ret 
	inc [cntdeep]
	dec bx
	loop @@checkwall
@@ret:
	mov al,[cntdeep]
	mov [deep],al
	popa
	ret
endp Deep_Dir3

;================================================
; Description - change the Xman,Yman accordingly the DirMan and the key
; INPUT:DirMan, ah - scan code of key
; OUTPUT:Xman,Yman of the man after a movement
; Register Usage:ax,bx 
;================================================
proc MoveInmaze
	push ax
	push bx
	cmp [deep],0
	jne @@WallBack
	cmp ah,48h ;scan code of up arrow
	je @@ret
@@WallBack:	
	cmp ah,50h ;scan code of down arrow
	jne @@canmove
	mov bl,[DirMan]
	shl bl,1 ;mul by 2, offset is word
	xor bh,bh
	call [MoveBack+bx]
	cmp [cantmove],1
	je @@ret
@@canmove:
	mov [move],1
	mov al,[DirMan]
	shr al,1 ;check if the direction of the man is even or odd
	jc @@OddDir
@@EvenDir:

	cmp [DirMan],0
	ja @@not0
	cmp ah,48h ;scan code of up arrow
	je @@DecY
	jmp @@IncY
	
@@not0:
	cmp ah,48h ;scan code of up arrow
	ja @@DecY	
@@IncY:	
	inc [Yman]
	jmp @@ret
@@DecY:
	dec [Yman]
	jmp @@ret
@@OddDir:

	cmp [DirMan],1
	ja @@not1
	cmp ah,48h ;scan code of up arrow
	je @@IncX
	jmp @@DecX
	
@@not1:
	cmp ah,48h ;scan code of up arrow
	je @@DecX	
@@IncX:	
	inc [Xman]
	jmp @@ret
@@DecX:
	dec [Xman]
	


@@ret:
	pop bx
	pop ax
	ret
endp MoveInmaze

;================================================
;this description is for the next 4 procedures
; Description - check if after the move you will pass wall from back
;               put 0/1 in cantmove that fits for the situation
; INPUT:Xman,Yman,DirMan 
; OUTPUT:deep
; Register Usage:none beacuse all saved, there are pusha,popa 
;================================================
proc Goback_dir0
	push bx
	CalcIndex
	test [maze+bx],SouthMask ;the wall behind you in dir 0 is south wall
	jz @@ret
	mov [cantmove],1
@@ret:
	pop bx
	ret
endp Goback_dir0
	
proc Goback_dir1
	push bx
	CalcIndex
	test [maze+bx],WestMask  ;the wall behind you in dir 1 is west wall
	jz @@ret
	mov [cantmove],1
@@ret:
	pop bx
	ret
endp Goback_dir1

proc Goback_dir2
	push bx
	CalcIndex
	test [maze+bx],NorthMask ;the wall behind you in dir 2 is north wall
	jz @@ret
	mov [cantmove],1
@@ret:
	pop bx
	ret
endp Goback_dir2

proc Goback_dir3
	push bx
	CalcIndex
	test [maze+bx],EastMask ;the wall behind you in dir 3 is east wall
	jz @@ret
	mov [cantmove],1
@@ret:
	pop bx
	ret
endp Goback_dir3

;================================================
; Description - change the DirMan accordingly the key 
; INPUT:DirMan, AH - scan code of the key
; OUTPUT:DirMan
; Register Usage:ax 
;================================================
proc ChangeLook
	push ax
	
	cmp ah,4dh ;scan code of right arrow
	je @@rightarrow
	cmp ah,4bh ;scan code of left arrow
	jne @@ret
@@leftarrow:
	dec [DirMan]
	jmp @@ret
@@rightarrow:
	inc [DirMan]
@@ret:
	;DirMan mod 4
	;dir 4 is like 0, 5 is like 1 
	and [DirMan],3 ;00000011b
	
	pop ax
	ret
endp ChangeLook
;================================================
; Description - draw full rect in the screen
; INPUT: cx = left col 
;        dx = top row 
;        al = color 
;        si = height 
;        di = width 
; OUTPUT:screen
; Register Usage: none beacuse all saved, there are pusha,popa 
;================================================
proc FullRect
	cmp si,0
	je @@ret
	cmp di,0
	je @@ret
	pusha

	CalcIndexInScreen
	
	mov cx,si
@@height:
	push cx
	mov cx,di
	push bx
@@width:
	mov [byte ptr es:bx],al
	inc bx
	loop @@width
	pop bx
	add bx,320 ;next line
	pop cx
	loop @@height

	popa
@@ret:	
	ret
endp FullRect

;================================================
; Description - draw empty rect in the screen
; INPUT: cx = left col 
;        dx = top row 
;        al = color 
;        si = height 
;        di = width 
; OUTPUT:screen
; Register Usage: none beacuse all saved, there are pusha,popa 
;================================================
proc EmptyRect
	;pusha
	call DrawVerticalLine ;draw the left vertical line
	add cx,di 
	call DrawVerticalLine ;draw the right vertical line
	sub cx,di
	push si
	mov si,di
	call DrawHorizontalLine ;draw the up horizontal line
	pop si
	add dx,si
	dec dx
	mov si,di
	call DrawHorizontalLine ;draw the down horizontal line
@@ret:	
	;popa
	ret
endp EmptyRect

;================================================
; Description - draw horizontal line in the screen
; INPUT: cx = left col 
;        dx = row 
;        al = color 
;        si = length 
; OUTPUT:screen
; Register Usage: bx,dx,si,cx 
;================================================
;cx- x left
;si- length
proc DrawHorizontalLine	
	cmp si,0
	je @@ret
	push bx
	push dx 
	push si
	push cx
	
	CalcIndexInScreen
	mov cx,si
	
@@drawpixel:
	mov [byte ptr es:bx],al 
	inc bx ;next pixel
	loop @@drawpixel
	
	pop cx
	pop si
	pop dx
	pop bx
	
@@ret:	
	ret
endp DrawHorizontalLine

;================================================
; Description - draw horizontal line in the screen
; INPUT: cx = col 
;        dx = top row 
;        al = color 
;        si = length 
; OUTPUT:screen
; Register Usage: bx,dx,si,cx 
;================================================
proc DrawVerticalLine
	cmp si,0
	je @@ret	
	push bx
	push dx 
	push si
	push cx

	CalcIndexInScreen
	mov cx,si
	
@@drawpixel:
	mov [byte ptr es:bx],al
	add bx,320 ;next pixel
	loop @@drawpixel
	
	pop cx
	pop si
	pop dx
	pop bx
@@ret:
	ret
endp DrawVerticalLine
	

;---------------------------------------------;
; input: point1X point1Y,         ;
; 		 point2X point2Y,         ;
;		 Color                                ;
; output: line on the screen                  ;
;---------------------------------------------;
PROC DrawLine2D
	mov cx, [point1X]
	sub cx, [point2X]
	absolute cx
	mov bx, [point1Y]
	sub bx, [point2Y]
	absolute bx
	cmp cx, bx
	jae DrawLine2Dp1 ; deltaX > deltaY
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp cx, dx
	jbe DrawLine2DpNxt1 ; point1Y <= point2Y
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt1:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDY point1X, point1Y, point2X, point2Y
	ret
DrawLine2Dp1:
	mov ax, [point1X]
	mov bx, [point2X]
	mov cx, [point1Y]
	mov dx, [point2Y]
	cmp ax, bx
	jbe DrawLine2DpNxt2 ; point1X <= point2X
	xchg ax, bx
	xchg cx, dx
DrawLine2DpNxt2:
	mov [point1X], ax
	mov [point2X], bx
	mov [point1Y], cx
	mov [point2Y], dx
	DrawLine2DDX point1X, point1Y, point2X, point2Y
	ret
ENDP DrawLine2D

;-----------------------------------------------;
; input: pointX pointY,      					;
;           Color								;
; output: point on the screen					;
;-----------------------------------------------;
proc PIXEL
	mov cx,[pointX]
	mov dx,[pointY]
	mov al,[Color]
	CalcIndexInScreen
	mov [byte ptr es:bx],al
	;mov bh,0h
	;mov ah,0Ch
	;int 10h
	ret
endp PIXEL

END start
	