[org 0x0100] 

jmp start

;__________________________________________________________________________________

B_O:
mov word[inputbase], 2
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],8
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

B_D:
mov word[inputbase], 2
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],10
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

B_H:
mov word[inputbase],2
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],16
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

O_B:
mov word[inputbase],8
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],2
push word [outputbase]
call outputNumber

ret

;__________________________________________________________________________________

O_D:
mov word[inputbase],8
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase] ,10
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

O_H:
mov word[inputbase],8
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],16
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

D_B:
mov word[inputbase],10
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],2
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

D_O:
mov word[inputbase],10
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],8
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

D_H:
mov word[inputbase],10
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],16
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

H_B:
mov word[inputbase],16
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase] ,2
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

H_O:
mov word[inputbase],16
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase],8
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

H_D:
mov word[inputbase],16
push word [inputbase]
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
call input
push ax
mov word[outputbase], 10
push word [outputbase]
call outputNumber
ret

;__________________________________________________________________________________

add_:
	push bp
	mov bp, sp
	pusha
		mov dx, msg8
		mov cx, len8
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input			; taking input
		push ax
		call Line_Break
		mov dx, msg9
		mov cx, len9
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input
		push ax
		call Line_Break
		mov dx, msg10
		mov cx, len10
		mov bx, 1
		mov ax, 4000h
		int 0x21
		pop ax
		pop bx
		add ax, bx
		push ax
		push word [inputbase]
		call outputNumber	
	popa	
	pop bp
	ret

;________________________________________________________________________________________________-

subtract_:
	
	push bp
	mov bp, sp
	pusha
		mov dx, msg8
		mov cx, len8
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input			; taking input
		push ax
		call Line_Break
		mov dx, msg9
		mov cx, len9
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input
		push ax
		call Line_Break
		mov dx, msg10
		mov cx, len10
		mov bx, 1
		mov ax, 4000h
		int 0x21
		pop bx
		pop ax
		sub ax, bx
		push ax
		push word [inputbase]
		call outputNumber	
	popa
	pop bp
ret



;_______________________________________________________________________________________________

multiplication_:
	push bp
	mov bp, sp
	pusha
		mov dx, msg8
		mov cx, len8
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input			; taking input
		push ax
		call Line_Break
		mov dx, msg9
		mov cx, len9
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input
		push ax
		call Line_Break
		mov dx, msg10
		mov cx, len10
		mov bx, 1
		mov ax, 4000h
		int 0x21
		pop ax
		pop bx
		mul  bx
		push ax
		push word [inputbase]
		call outputNumber	
	popa	
	pop bp
	
ret

;___________________________________________________________________________________________________________________

division_:

	push bp
	mov bp, sp
	pusha
		mov dx, msg8
		mov cx, len8
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input			; taking input
		push ax
		call Line_Break
		mov dx, msg9
		mov cx, len9
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input
		push ax
		call Line_Break
		mov dx, msg10
		mov cx, len10
		mov bx, 1
		mov ax, 4000h
		int 0x21
		pop bx
		pop ax
		xor dx, dx
		div bx
		push dx
		push ax
		push word [inputbase]
		call outputNumber
		mov dx, msg12
		mov cx, len12
		mov bx, 1
		mov ax, 4000h
		
		int 0x21
		pop dx
		push dx
		push word [inputbase]
		call outputNumber
	popa	
	pop bp
ret



;____________________________________________________________________________________________________________


sqrt_:
	mov dx, msg8
	mov cx, len8
	mov bx, 1
	mov ax, 4000h
	int 0x21
	call Line_Break
	push word [inputbase]
	call input
	push ax
	call sqrt
	push word [inputbase]
	call outputNumber
ret

sqrt:
	 
	push bp
	mov bp, sp
	fild word [bp+4]
	fsqrt
	fistp word [bp+4]
	pop bp
	

ret

;______________________________________________________________________________________________________________


modulus_:
	push bp
	mov bp, sp
	pusha
		mov dx, msg8
		mov cx, len8
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input			; taking input
		push ax
		call Line_Break
		mov dx, msg9
		mov cx, len9
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call input
		push ax
		call Line_Break
		mov dx, msg10
		mov cx, len10
		mov bx, 1
		mov ax, 4000h
		int 0x21
		pop bx
		pop ax
		xor dx, dx
		div bx
		push dx
		push word [inputbase]
		call outputNumber
		mov dx, msg12
		mov cx, len12
		mov bx, 1
		mov ax, 4000h
		int 0x21
		push word [inputbase]
		call outputNumber
	popa	
	pop bp
ret

;______________________________________________________________________________________________________________________


;_______________________________________________________________________________________________________________________

OFF:
mov ax,0x4c00
int 0x21

;_______________________________________________________________________________________________________________________


Expression:

ret


;________________________________________________________________________________________________________________________

Line_Break:
	pusha
	mov ah, 0x02					;print single character
	mov dl, 0x0a					;line break character
	int 0x21					;call intrrupt subroutine
	popa
ret

;____

input:
					; inputs a multi digit numbers
	push bp
	mov bp, sp
	push bx
	push cx
	push dx
	push si

	xor cx, cx

	mov dx, buffer			;inputted characters stored in buffer
	mov ax, 0x0a00			;taking input in buffer
	int 0x21

	call Line_Break			; breaking line after input
	
	mov ax, 0
	mov bx, [bp+4]				; bx have base and converting it into decimal
	mov cl, [buffer+1]			; number of chafracters inputted
	cmp cl, 0				; if no input enterd
	jz skipConvert			; then skip conversion loop
	mov si, buffer+2			;sarting address of an inputted array characters
	
	convertLoop:				;conversion loop of characters into number
		
		xor dh, dh
		mov dl, [si]
		sub dl, 48 ; '0'
		cmp dl, 9
		jle .nonalpha
		sub dl, 7
		.nonalpha:
		cmp dl, 0
		jl .continue
		cmp dx, bx
		jge .continue
		push dx
		  xor dx, dx
		  mul bx
		pop dx
		add ax, dx
		.continue:
		inc si
	loop convertLoop
	
	skipConvert:
	
	inputNumberEnd:
		pop si
		pop dx
		pop cx
		pop bx
		pop bp

ret 2

;_____________________________________________________




;_____________________________________________________

outputNumber:
  push bp
  mov bp, sp
  pusha

  mov ax, [bp+6]
  cmp ax, 0
  jge .positive
    mov ah, 0x02
    mov dl, '-'
    int 0x21
    mov ax, [bp+6]
    neg ax
  .positive:

  mov bx, [bp+4]
  mov cx, 0

  .extractLoop:
    xor dx, dx
    div bx

    cmp dx, 10
    jl .numeric
    add dx, 0x07   ; 'A'-'9' => 7 if we add more to 9 we get to char A
    .numeric:
    add dx, '0'

    push dx
    
    inc cx
  cmp ax, 0
  jnz .extractLoop

  mov ah, 0x02
  .printLoop:
    pop dx
    int 0x21
  loop .printLoop

  call Line_Break ; use your linebreak function here

  .outputNumberEnd:
  popa
  pop bp
ret 4

;_____________________________________________________

Systems:
		

push ax
	mov ah,01
	int 0x21
		cmp al,97
		jne skipBinary
		mov word[inputbase],2
		jmp skipOFF1
		
	skipBinary:
		cmp al,65
		jne skipBinary1
		mov word[inputbase], 2
		jmp skipOFF1
	skipBinary1:
	
		cmp al,98
		jne skipOctal
		mov word[inputbase],8
		jmp skipOFF1
	skipOctal:
	
		cmp al,66
		jne skipOctal1
		mov word[inputbase],8
		jmp skipOFF1
	skipOctal1:
	
		cmp al,99
		jne skipDecimal
		mov word[inputbase], 10
		jmp skipOFF1
	skipDecimal:
	
		cmp al,67
		jne skipDecimal1
		mov word[inputbase],10
		jmp skipOFF1
	skipDecimal1:
	
		cmp al,100
		jne skipHexa_Decimal
		mov word[inputbase],16
		jmp skipOFF1
	skipHexa_Decimal:
	
		cmp al,68
		jne skipHexa_Decimal1
		mov word[inputbase] ,16
		jmp skipOFF1
	skipHexa_Decimal1:
	
		cmp al,101
		jne skipOFF
		jmp OFF
		
	skipOFF:
		cmp al,69
		jne  skipOFF1
		jmp OFF
		
	skipOFF1:
pop ax

ret

;_______________________________________________________________________________________

	NumberConversion:
		mov dx, msg3
		mov cx, len3
		mov bx, 1
		mov ax, 4000h
		int 0x21
		call Line_Break
		mov ah, 0x01
		int 0x21
			cmp al,97
			jne skipB_O
			call B_O
			jmp next
		skipB_O:
		
			cmp al,65
			jne skipB_O1
			call B_O
			jmp next
		skipB_O1:
		
			cmp al,98
			jne skipB_D
			call B_D
			jmp next
		skipB_D:
		
			cmp al,66
			jne  skipB_D1
			call B_D
			jmp next
		skipB_D1:
		
			cmp al,99
			jne skipB_H
			call B_H
			jmp next
		skipB_H:
		
			cmp al,67
			jne skipB_H1
			call B_H
			jmp next
		skipB_H1:
		
			cmp al,100
			jne skipO_B
			call O_B
			jmp next
		skipO_B:
		
			cmp al,68
			jne skipO_B1
			call O_B
			jmp next
		skipO_B1:
		
			cmp al,101
			jne skipO_D
			call O_D
			jmp next
		skipO_D:
		
			cmp al,69
			jne skipO_D1
			call O_D
			jmp next
		skipO_D1:
		
			cmp al,102
			jne skipO_H
			call O_H
			jmp next
		skipO_H:
		
			cmp al,70
			jne skipO_H1
			call O_H
			jmp next
		skipO_H1:
		
			cmp al,103
			jne skipD_B
			call D_B
			jmp next
		skipD_B:
		
			cmp al,71
			jne skipD_B1
			call D_B
			jmp next
		skipD_B1:
		
			cmp al,104
			jne skipD_O
			call D_O
			jmp next
		skipD_O:
		
			cmp al,72
			jne  skipD_O1
			call D_O
			jmp next
		skipD_O1:
		
			cmp al,105
			jne skipD_H
			call D_H
			jmp next
		skipD_H:
		
			cmp al,73
			jne skipD_H1
			call D_H
			jmp next
		skipD_H1:
		
			cmp al,106
			jne skipH_B
			call H_B
			jmp next
		skipH_B:
		
			cmp al,72
			jne skipH_B1
			call H_B
			jmp next
		skipH_B1:
		
			cmp al,107
			jne skipH_O
			call H_O
			jmp next
		skipH_O:
		
			cmp al,73
			jne  skipH_O1
			call H_O
			jmp next
		skipH_O1:
		
			cmp al,108
			jne skipH_D
			call H_D
			jmp next
		skipH_D:
		
			cmp al,74
			jne skipH_D1
			call H_D
			jmp next
		skipH_D1:
		
			cmp al,108
			jne skipOFF3
			call OFF
			jmp next
		skipOFF3:
		
			cmp al,74
			jne skipOFF4
			jmp OFF
		skipOFF4:
		
			jmp OFF
		
	
;_____________________________________________________
	
	PerformOperations:
		mov dx, msg4
		mov cx, len4
		mov bx, 1
		mov ax, 4000h
		int 0x21
		call Line_Break
		mov ah,01
		int 0x21
		
			cmp al,97
			jne skipTwoNumberOperations
			jmp TwoNumberOperations
		
		skipTwoNumberOperations:
		
			cmp al,65
			jne skipTwoNumberOperations1
			jmp TwoNumberOperations
		
		skipTwoNumberOperations1:

		
			cmp al,99
			jne skipOFF7
			jmp OFF
			
		skipOFF7:	
		
			cmp al,67
			jne skipOFF8
			jmp OFF
		
		skipOFF8:
			jmp OFF
			

		
;___________________________________________________________________________________________________________-
		
	 TwoNumberOperations:
		mov dx, msg5
		mov cx, len5
		mov bx, 1
		mov ax, 4000h
		int 0x21
		call Line_Break
		call Systems
		mov dx, msg7
		mov cx, len7
		mov bx, 1
		mov ax, 4000h
		int 0x21
		mov ah, 0x01
		int 0x21
		
			cmp al,65
			jne skipadd_
			call add_
			jmp next
		skipadd_:
		
			cmp al,97
			jne skipadd_1
			call add_
			jmp next
		skipadd_1:
		
			cmp al,66
			jne skipsubtract_
			call subtract_
			jmp next
		skipsubtract_:
		
			cmp al,98
			jne skipsubtract_1
			call subtract_
			jmp next
		skipsubtract_1:
		
			cmp al,67
			jne skipmultiplication_
			call multiplication_
			jmp next
		skipmultiplication_:
		
			cmp al,99
			jne skipmultiplication_1
			call multiplication_
			jmp next
		skipmultiplication_1:
		
			cmp al,68
			jne skipdivision_
			call division_
			jmp next
		skipdivision_:
		
			cmp al,100
			jne skipdivision_1
			call division_
			jmp next
		skipdivision_1:
		
			cmp al,69
			jne skipsqrt_
			call sqrt_
			jmp next
		skipsqrt_:
		
			cmp al,101
			jne skipsqrt_1
			call sqrt_
			jmp next
		skipsqrt_1:
			
			cmp al, 70
			jne skip_OFF_
			jmp OFF
			
		skip_OFF_:
			cmp al, 102
			jne skip_OFF_1
			jmp OFF
			
		skip_OFF_1:
			jmp OFF
			
		


;___________________________________________________________________________________________________________

start:
	mov ax,03
	int 0x10
	
	;push 2
	;call input
	
	;push ax
	;push 8
	;call outputNumber
	;call Line_Break
	
	mov dx, msg1
	mov cx, len1
	mov bx, 1
	mov ax, 4000h
	int 0x21
	
	call Line_Break	
next:
	mov dx, msg2
	mov cx, len2
	mov bx, 1
	mov ax, 4000h
	int 0x21
	
	mov ah,01
	int 0x21
		cmp al,97
		jne skipNumberConversion
		jmp NumberConversion
	skipNumberConversion:
		cmp al,65
		jne skipNumberConversion1
		jmp NumberConversion
	skipNumberConversion1:
		cmp al,98
		jne skipPerformOperations
		jmp PerformOperations
	skipPerformOperations:
		cmp al,66
		jne  skipPerformOperations1
		jmp PerformOperations
	skipPerformOperations1:
		cmp al,99
		jne  skipOFF9
		jmp OFF
	skipOFF9:
		cmp al,67
		jne skipOFF10
		jmp OFF
	skipOFF10:
		jmp OFF
	

	
	call Line_Break
	
	mov ax, 4c00h
	int 0x21

;_______________________________________________________________________________________________________________________________________________________

msg1: db "Welcome to My Calculator "
len1: equ ($-msg1)

msg2: db 10 ,"Which Operation Do You Want To Perform ", 10, "a) Number Conversions", 10, "b) Perform Operations ",10, "c) Off Calculator"
len2: equ ($-msg2)

msg3: db 10 ,"Select Number Conversions: ",10,"a) B-O",10,"b) B-D",10,"c) B-H",10,"d) O-B",10,"e) O-D",10,"f) O-H",10,"g) D-B",10,"h) D-O",10,"i) D-H",10,"j) H-B",10,"k) H-O",10,"l) H-D",10,"m) OFF Calculator",10
len3: equ ($-msg3)

msg4: db 10, "Perform Operations: ",10,"a) 2 Number Operations",10,"c) OFF Calculator",10
len4: equ ($-msg4)

msg5: db 10,"Select one number system: ",10,"a) Binary",10,"b) Octal",10,"c) Decimal",10,"d) Hexa-Decimal",10,"e)OFF Calculator",10
len5: equ ($-msg5)


msg7: db 10, "2 Number Operations:- ",10,"Select an Operation",10,"a) Addition",10,"b)Subtraction",10,"c)Multiplication",10,"d)Division",10,"e)Square Root",10,"f)OFF Calculator",10
len7: equ ($-msg7)

msg8: db 10,"Enter Number",10
len8: equ($-msg8)

msg9: db 10, "Enter Another Number",10
len9: equ($-msg9)

msg10: db 10,"Result: "
len10: equ($-msg10)

msg11: db 10,"Change Result to Number System ",10,"a) Binary",10,"b)Octal",10,"c)Deciaml",10,"d)Hexa-Decimal",10
len11: equ($-msg11)

msg12: db 10,"Modulus: "
len12: equ($-msg12)

buffer: db 6,   0			;making buffer for input highest size of buffer is 256 here 6 decides how many characters can buffer store
times 12 db 0				;buffer size

inputbase: dw 10
outputbase : dw 10
