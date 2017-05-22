	.387
	assume cs:code1, ds:data1, ss:stack1

	data1 segment
	argslength			db 16 dup(0)																			;OK Ograniczenie do 16 argumentow
	argscounter			dw 0																					;OK
	args 				db 128 dup('$')																			;OK Maksymalnie moze byc 127 bajtów argumentow
	usage				db "Nalezy wprowadzic 6 liczb w formacie 123.4567$"										;OK
	xmin 				dq ?
	xmax 				dq ?
	ymin 				dq ?
	ymax 				dq ?
	cr 					dq ?
	ci 					dq ?
	data1 ends																									

	code1 segment

	start:
		mov ax, seg data1
		mov ds, ax
		mov ax, seg stack1
		mov ss, ax
		mov sp, offset wsk
		finit

		mov ah,62h
		int 21h
		mov es,bx

		xor di,di
		mov bl,81h; od znaku spacji szukamy niebiałych znakow
	petla:
		call findnotwhite ;zwraca w bl niebiały znak
		call copytowhite  ;pobiera z bl niebiały znak, zwraca w di - ostatnie miejsce w args do ktorego skopiowano
	cmp byte ptr es:[bx],13d ; jesli enter to koncyzmy
	jne petla

		call checkargs

		call tryb_graficzny
		;call rysuj
		call tryb_tekstowy
	
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
		mov dx,offset ds:[usage]
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
	;******przejscie do trybu graficznego****
	;****************************************
	tryb_graficzny proc
		push ax
		mov	al,13h ;320x200 punktow 256 kolorow
		xor ah,ah
		int	10h
		pop ax
		ret
	tryb_graficzny endp	
	;****************************************
	;******przejscie do trybu tekstowego*****
	;****************************************
	tryb_tekstowy proc
		push ax
		xor	ax,ax
		int	16h  ;oczekiwanie na dowolny klawisz
		mov	al,3h  ;tryb tekstowy
		xor ah,ah
		int	10h
		pop ax
		ret
	tryb_tekstowy endp

	code1 ends

	stack1 segment stack
			dw 200 dup(?)
		wsk dw ?
	stack1 ends
	end start