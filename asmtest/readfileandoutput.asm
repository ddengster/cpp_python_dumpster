
;%OUT outputs to the console
%OUT ASDADASDAD


.386
.model flat, stdcall
option casemap :none

include \masm32\include\windows.inc
include \masm32\include\user32.inc
include \masm32\include\kernel32.inc
includelib \masm32\lib\user32.lib
includelib \masm32\lib\kernel32.lib

.data
	;String variables
	FileName	db "C:\Documents and Settings\ddeng\Desktop\asmtest\test.txt", 0
.data?
	hFile		HANDLE	?		;? is best to use for data types that libraries have already defined for you
	hMemory		HANDLE	?
	pMemory		DWORD	?
	ReadSize	DWORD	?
	
.const
	MEMORYSIZE	equ		65535	;The amount of memory allocated so we can store our file.
								;<varname> equ expression => assigns expression to <varname>
	
.code
start:
	; Call CreateFile and store handle of file in hFile
	invoke	CreateFile, addr FileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
	mov		hFile, eax					;Store returned value in hFile
	
	;Allocate and lock memory
	invoke	GlobalAlloc, GMEM_MOVEABLE or GMEM_ZEROINIT, MEMORYSIZE
	mov		hMemory, eax
	invoke	GlobalLock, hMemory
	mov		pMemory, eax
	
	;Reads contents of file into pMemory and output them
	invoke	ReadFile, hFile, pMemory, MEMORYSIZE-1, addr ReadSize, NULL
	invoke	MessageBox, NULL, pMemory, addr FileName, MB_OK
	
	;Cleanup
	invoke	GlobalUnlock, pMemory
	invoke	GlobalFree, hMemory
	invoke	CloseHandle, hFile
	invoke	ExitProcess, NULL
end start
