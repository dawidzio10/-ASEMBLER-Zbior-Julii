assume cs:code1, ds:data1, ss:stack1

data1 segment
	argslength			db 16 dup(0)																			;OK Ograniczenie do 16 argumentow
	argscounter			dw 0																					;OK
	args 				db 128 dup('$')																			;OK Maksymalnie moze byc 127 bajtów argumentow
	usage				db "Nalezy wprowadzic 6 liczb w formacie 1234.567$"										;OK
	xmin 				dq ?																					;OK
	xmax 				dq ?																					;OK
	ymin 				dq ?																					;OK
	ymax 				dq ?																					;OK
	cr 					dq ?																					;OK
	ci 					dq ?																					;OK
	isnegative 			db ?																					;OK
	tmpW				dw ?																					;OK
	dziesiec			dq 10.0																					;OK
	zerojeden			dq 0.1																					;OK
	minusjeden			dq -1.0																					;OK
	jeden				dq 1.0																					;OK

	zr 					dq ?
	zi 					dq ?
	dwa					dq 2.0
	cztery				dq 4.0
	resx				dw 320
	resy 				dw 200
	x   				dw 0
	y 					dw 0
	kol 				db ?
data1 ends																									

code1 segment
start:
		.387
		.386
		mov ax, seg data1
		mov ds, ax
		mov ax, seg stack1
		mov ss, ax
		mov sp, offset wsk
		mov ah,62h
		int 21h
		mov es,bx
		finit

		call createargs
		call checkargs
		call convert
		call graphmode
		call draw
		call textmode
	
	quit:	
		mov ah,4ch
		int 21h


	;****************************************
	;******wyszukiwanie niebialego znaku*****
	;****************************************
	;input bl - adres od ktorego szuka 
	;output bl - adres pierwszego niebialego znaku
	findnotwhite proc
		push dx
		xor bh,bh
	comp1:
		mov dl,byte ptr es:[bx]
		cmp dl,32d ;znak spacji
		je white
		cmp dl,9d ;znak tabulatora
		je white
		jmp foundnotwhite

	white:
		inc bl
		jmp comp1

	foundnotwhite:
		pop dx
		ret
	findnotwhite endp
	;****************************************
	;******kopiowanie do białego znkau*******
	;****************************************
	;input bl - adres od ktorego kopiuje 
	;output bl - adres nastepnego bialego znaku, di do którego miejsca uzyte args
	copytowhite proc
		push dx
		push si
		
		xor bh,bh
		mov si,argscounter

	copy:
		mov dl,byte ptr es:[bx]
		cmp dl,32d ;znak spacji
		je brcopy
		cmp dl,9d ;znak tabulatora
		je brcopy
		cmp dl,13d ;znak entera
		je brcopy

		mov byte ptr ds:[args+di],dl
		inc argslength[si]
		inc di
		inc bl
		jmp copy
	brcopy:
		mov byte ptr ds:[args+di],'/'
		inc di
		inc argscounter
		pop si
		pop dx
		ret
	copytowhite endp
	;****************************************
	;******tworzenie tablicy argumentow******
	;****************************************
	createargs proc
	push di
			xor di,di
			mov bl,81h; od znaku spacji szukamy niebiałych znakow
		petla:
			call findnotwhite ;zwraca w bl niebiały znak
			call copytowhite  ;pobiera z bl niebiały znak, zwraca w di - ostatnie miejsce w args do ktorego skopiowano
		cmp byte ptr es:[bx],13d ; jesli enter to koncyzmy
		jne petla
	pop di
	createargs endp
	;****************************************
	;******sprawdzanie poprawnosci arg*******
	;****************************************
	checkargs proc
		push ax
		push di
		
		cmp argscounter,6
		jne wrong
		mov di,0
		xor al,al

		mov dl,byte ptr ds:[args]
		cmp dl,'-'
		je checkn
		cmp dl,'0'
		jb wrong
		cmp dl,'9'
		ja wrong
		inc di
check:
		mov dl,byte ptr ds:[args+di]
		cmp dl,'$'
		je wyskocz
		cmp dl,'.'
		jb wrong
		cmp dl,'9'
		ja wrong
		cmp dl,'.'
		je zliczkropki
		cmp dl,'/'
		je checkn
		inc di
jmp check

	zliczkropki:
		inc al
		jmp checkk

	wyskocz:
		cmp al,6
		jne wrong
		pop di
		pop ax
		ret

	wrong:
		mov dx, offset usage
		mov ah,9
		int 21h
		pop di
		pop ax
		jmp quit

	checkn:
		inc di
		mov dl,byte ptr ds:[args+di]
		cmp dl,'-'
		je checkn
		cmp dl,'$'
		je wyskocz
		cmp dl,'0'
		jb wrong
		cmp dl,'9'
		ja wrong
		inc di
	jmp check


	checkk:
		inc di
		mov dl,byte ptr ds:[args+di]
		cmp dl,'0'
		jb wrong
		cmp dl,'9'
		ja wrong
		inc di
	jmp check
			
	checkargs endp
	;****************************************
	;******konwersja argumentow**************
	;****************************************
	convert proc
	push di

	xor di,di

	call przetworz
	fstp qword ptr [xmin]

	call przetworz
	fstp qword ptr [xmax]

	call przetworz
	fstp qword ptr [ymin]

	call przetworz
	fstp qword ptr [ymax]

	call przetworz
	fstp qword ptr [cr]

	call przetworz
	fstp qword ptr [ci]

	pop di
	ret
	convert endp
	;****************************************
	;******przetwarzanie argumentow**********
	;****************************************
	;INPUT di - pierwszy znak liczby
	przetworz proc
	
	push ax
	push dx
	push si

	xor dx,dx
	mov tmpW,0
	mov isnegative,0

	mov dl, byte ptr ds:[args+di]
	cmp dl,'-'
	je ujemna


przetworzprzedkropka:
	mov dl, byte ptr ds:[args+di]
	sub dl,'0'
	mov tmpW,dx
	fild word ptr [tmpW]
petla1:
	inc di
	mov dl, byte ptr ds:[args+di]
	cmp dl,'.'
	je przetworzpokropce
	fld dziesiec
	fmulp st(1),st(0)
	sub dl,'0'
	mov tmpW,dx
	fild word ptr [tmpW]
	faddp st(1),st(0)
	jmp petla1


przetworzpokropce:
	inc di
	mov dl, byte ptr ds:[args+di]
	sub dl,'0'
	mov tmpW,dx
	fild word ptr [tmpW]
petla2:
	inc di
	mov dl, byte ptr ds:[args+di]
	cmp dl,'/'
	je zakoncz
	fld dziesiec
	fmulp st(1),st(0)
	sub dl,'0'
	mov tmpW,dx
	fild word ptr [tmpW]
	faddp st(1),st(0)
	jmp petla2
	

ujemna:
	mov isnegative,1
	inc di
	jmp przetworzprzedkropka

zakoncz:
	fld jeden
	fxch st(2)
	fxch st(1)

zmniejsz:
	fcom st(2) 
	fstsw ax
	sahf 
	jb porown
	fld zerojeden
	fmulp st(1),st(0)
jmp zmniejsz

porown:
	faddp st(1),st(0)
	cmp isnegative,0
	je wyjdz

	fld minusjeden
	fmulp st(1),st(0)

wyjdz:
	inc di
	pop si
	pop dx
	pop ax
	ret
	przetworz endp
	
	;****************************************
	;******przejscie do trybu graficznego****
	;****************************************
	graphmode proc
		push ax
		mov	al,13h ;320x*x00 punktow 256 kolorow
		xor ah,ah
		int	10h
		pop ax
		ret
	graphmode endp	
	;****************************************
	;******przejscie do trybu tekstowego*****
	;****************************************
	textmode proc
		push ax
		xor	ax,ax
		int	16h  ;oczekiwanie na dowolny klawisz
		mov	al,3h  ;tryb tekstowy
		xor ah,ah
		int	10h
		pop ax
		ret
	textmode endp

	;****************************************
	;******rysowanie*************************
	;****************************************
	draw proc
	push cx
	push dx
		mov cx,resy ; licznik do petli y

		petlay:
		mov x,0
		mov dx,resx ; licznik do petli x
			petlax:
			call tysiac
			call pomaluj
			inc x
			dec dx
			cmp dx,0
			jne petlax
			inc y
		loop petlay
	pop dx
	pop cx
	ret
	draw endp

	
	pomaluj proc
	pusha
		mov	ax,0a000h   ;seg pamieci VGA 
		mov	es,ax
		mov	ax,y
		mov	bp,320
		mul	bp     ; dx:ax=ax*bp=y*320 
		add	ax,x  ; ax=320*y+x
		mov bx,ax
		mov	al,kol
		mov	byte ptr es:[bx],al
	popa
	ret
	pomaluj endp

	tysiac proc 
		push cx

		fld qword ptr [cztery]	; 4 
		fld qword ptr [ci]		; ci | 4 
		fld qword ptr [cr]		; cr | ci | 4 
		fld qword ptr [dwa] 	; 2 | cr | ci | 4

		;zi
		fld qword ptr [ymax]	; ymax | 2 | cr | ci | 4
		fld qword ptr [ymin]	; ymin | ymax | 2 | cr | ci | 4
		fsubp st(1), st			; ymax-ymin | 2 | cr | ci | 4
		fild word ptr [resy]	; 200 | ymax-ymin | 2 | cr | ci | 4
		fdivp st(1), st			; (ymax-ymin)/200 | 2 | cr | ci | 4
		fild word ptr [y]		; y | (ymax-ymin)/200 | 2 | cr | ci | 4
		fmulp st(1), st			; y*(ymax-ymin)/200 | 2 | cr | ci | 4
		fld qword ptr [ymin]	; ymin | y*(ymax-ymin)/200 | 2 | cr | ci | 4
		faddp st(1), st			; zi(y) | 2 | cr | ci | 4

		;zr
		fld qword ptr [xmax]	; xmax | y | 2 | cr | ci | 4
		fld qword ptr [xmin]	; xmin | xmax | y | 2 | cr | ci | 4
		fsubp st(1), st			; xmax-xmin | y | 2 | cr | ci | 4
		fild word ptr [resx]	; 320 | xmax-xmin | y | 2 | cr | ci | 4
		fdivp st(1), st			; (xmax-xmin)/320 | y | 2 | cr | ci | 4
		fild word ptr [x]		; x | (xmax-xmin)/320 | y | 2 | cr | ci | 4
		fmulp st(1), st			; x*(xmax-xmin)/320 | y | 2 | cr | ci | 4
		fld qword ptr [xmin]	; xmin | x*(xmax-xmin)/320 | y | 2 | cr | ci | 4 
		faddp st(1), st			; zr(x)| y | 2 | cr | ci | 4



		mov cx, 1000
		licz:
			fld st 				; x | x | y | 2 | cr | ci | 4 
			fmul st,st(1)		; x*x | x | y | 2 | cr | ci | 4 
			fld st(2)			; y | x*x | x | y | 2 | cr | ci | 4 
			fmul st,st(3) 		; y*y | x*x | x | y | 2 | cr | ci | 4
			fsubp st(1), st		; x*x-y*y | x | y | 2 | cr | ci | 4 
			fadd st, st(4)		; x*x-y*y+cr | x | y | 2 | cr | ci | 4 
			fxch st(2)			; y | x | x*x-y*y+cr | 2 | cr | ci | 4 
			fmulp st(1),st      ; y*x | x*x-y*y+cr | 2 | cr | ci | 4
			fmul st,st(2)		; 2xy | x*x-y*y+cr | 2 | cr | ci | 4 
			fadd st,st(4)		; 2xy+ci | x*x-y*y+cr | 2 | cr | ci | 4 
			fxch 				; x*x-y*y+cr | 2xy+ci | 2 | cr | ci | 4 
			fld st  			; new_x | new_x | new_y | 2 | cr | ci | 4 
			fmul st, st(1)		; x*x | x | y | 2 | cr | ci | 4 
			fld st(2)			; y | x*x | x | y | 2 | cr | ci | 4 
			fmul st, st(3)		; y*y | x*x | x | y | 2 | cr | ci | 4 
			faddp st(1), st		; x*x+y*y | x | y | 2 | cr | ci | 4 
			fcomp st(7)			; x | y | 2 | cr | ci | 4
			fstsw ax
			sahf
			jb break
		loop licz
		
		mov kol, 31
		jmp policzone
		
		break:
		mov kol, 0
		
		policzone:
		fcompp ; 2 | cr | ci | 4
		fcompp ; ci | 4
		fcompp ; -
		pop cx
		ret
	tysiac endp

code1 ends



stack1 segment stack
	 	 dw 200 dup(?)
	wsk  dw ?
stack1 ends
end start