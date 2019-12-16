
; http://www.acm.uiuc.edu/sigwin/old/workshops/winasmtut.pdf
; The ; is a comment!

.386					;Tells the assembler to use .386 instruction set. There are .486 and .586 instruction sets too!
.model flat,stdcall		;Tells the assembler to use flat for the memory model of the program. stdcall is the parameter passing method
						;used by windows functions, which means you need to push you parameters right to left.
						
option casemap :none	;Forces your labels to be case sensitive. Recommended as good practice.

include \masm32\include\windows.inc
include \masm32\include\kernel32.inc

; We included this so we can use the stdout function.
;include \masm32\include\masm32.inc

; We included this so we can specially use the MessageBox function!
include \masm32\include\user32.inc

includelib \masm32\lib\kernel32.lib
includelib \masm32\lib\user32.lib
;includelib \masm32\lib\masm32.lib

.data					;define data first
;
    HelloWorld db "Hello world!", 0			;db = define byte
    HelloWorld2 db "Hello world2!", 0		; HelloWorld is defined as "..",0, therefore ".." terminated with a NULL character
;


.code					;starting point for program code
start:
; invoke is a function SPECIFIC to MASM

; MessageBox parmeters: ?, message, title, mb_ok
; All these parameters must be pushed into a stack 
; Eg.:
;		invoke SendMessage, [hWnd], WM_CLOSE, 0, 0
;		becomes:
;		push 0
;		push 0
;		push WM_CLOSE
;		push [hWnd]
;		call [SendMessage]

    invoke MessageBox, NULL, addr HelloWorld, addr HelloWorld2, MB_OK
    ;invoke StdOut, addr HelloWorld			; addr = address of variable
    invoke ExitProcess, 0			;call to exitprocess function.

end start

;To compile
;\masm32\bin\ml /c /Zd /coff yourfile.asm
;\masm32\bin\Link /SUBSYSTEM:WINDOWS yourfile.obj
;or
;\masm32\bin\Link /SUBSYSTEM:CONSOLE yourfile.obj