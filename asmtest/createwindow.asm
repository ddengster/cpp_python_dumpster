.386
.model flat, stdcall
option casemap :none

include \masm32\include\windows.inc
include \masm32\include\user32.inc
include \masm32\include\kernel32.inc
includelib \masm32\lib\user32.lib
includelib \masm32\lib\kernel32.lib

;define a function protype called WinMain with 4 DWORD params
WinMain proto :DWORD, :DWORD, :DWORD, :DWORD

.data
	;String variables
	ClassName	db "WinClass", 0
	AppName		db "Simple Window", 0
.data?
	hInstance	HINSTANCE ?					;http://msdn.microsoft.com/en-us/library/b3535ssw(VS.80).aspx
											;starts a near data segment for uninitialized data
	
.code
start:
	invoke GetModuleHandle, NULL			; The return value is always stored in the eax register!
	mov hInstance, eax
	invoke WinMain, hInstance, NULL, NULL, 0
	invoke ExitProcess, eax
	
	; proc => function
WinMain proc hInst:HINSTANCE, hPrevInst:HINSTANCE, CmdLine:LPSTR, CmdShow:DWORD
	local wc:WNDCLASSEX						; local variables
	local msg:MSG							; syntax: local variable:type
	local hwnd:HWND
	mov wc.cbSize, SIZEOF WNDCLASSEX
	mov wc.style, CS_HREDRAW or CS_VREDRAW	; or => | logical operator
	mov wc.lpfnWndProc, offset WndProc		; ?? offset?
	mov wc.cbClsExtra, NULL
	mov wc.cbWndExtra, NULL
	push hInstance
	pop wc.hInstance
	mov wc.hbrBackground, COLOR_WINDOW+1
	mov wc.lpszMenuName, NULL
	mov wc.lpszClassName, offset ClassName
	invoke LoadIcon, NULL, IDI_APPLICATION
	mov wc.hIcon, eax
	mov wc.hIconSm, eax
	invoke LoadCursor, NULL, IDC_ARROW
	mov wc.hCursor, eax
	invoke RegisterClassEx, addr wc
	
	;Invoke windows function
	invoke CreateWindowEx, 0, addr ClassName, addr AppName, WS_OVERLAPPEDWINDOW or WS_VISIBLE, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, NULL, NULL, hInst, NULL
	mov hwnd, eax
		
	;.while <condition>
	.while TRUE
		invoke GetMessage, addr msg, NULL, 0, 0
		.break .if (!eax)
		invoke TranslateMessage, addr msg
		invoke DispatchMessage, addr msg
	.endw			

	mov eax, msg.wParam
	ret						;Returns to the caller
WinMain endp

;Message receiving function
WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM
	.if uMsg == WM_DESTROY
		invoke PostQuitMessage, 0
	.else
		invoke DefWindowProc, hWnd, uMsg, wParam, lParam
	ret
	.endif
	xor eax, eax
	ret
WndProc endp

end start
