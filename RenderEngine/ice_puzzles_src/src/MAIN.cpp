
#include <iostream>
#include "Headers.h"
#include "AppMain.h"
#include "InputMgr.h"
#include <time.h>

#pragma comment( lib, "winmm.lib" ) // FOR timeGetTime()

using namespace ddengine_RenderEngine;

const String openglwindowname = "OpenGL: Ice puzzles";
const String directx9windowname = "Directx9: Ice puzzles";
const String processname = "Ice_puzzles";

bool use_directx9 = false;
bool fullscreen = false;
uint32_t width = 800;
uint32_t height = 600;
HWND windowhandle;

#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

const Real SEC_PER_TICK = 1.0f / Real(CLOCKS_PER_SEC);

void Run()
{
	ShowWindow(windowhandle, SW_SHOW);

	MSG		msg;
	ZeroMemory(&msg, sizeof(MSG));
	while(true)
	{
		if( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
		{
			if(msg.message == WM_QUIT)	break;

			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		clock_t currentTime = clock();
		static clock_t lastTime = currentTime;

		unsigned int clockDiff = currentTime - lastTime;
		float timeElapsed =  clockDiff * SEC_PER_TICK;

		AppMain::GetInstance()->Loop(timeElapsed);
		lastTime = currentTime;

		if(KEY_DOWN(VK_ESCAPE))
            PostMessage(windowhandle, WM_DESTROY, 0, 0);
	}
}

LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch(message)
	{
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	case WM_LBUTTONDOWN:
		{
			POINT mousept;
			GetCursorPos(&mousept);
			InputMgr::GetInputMgr()->LeftMouseButtonDown(mousept.x, mousept.y);
			return 0;
		}
	case WM_LBUTTONUP:
		{
			POINT mousept;
			GetCursorPos(&mousept);
			InputMgr::GetInputMgr()->LeftMouseButtonUp(mousept.x, mousept.y);
			return 0;
		}
	case WM_KEYDOWN:
		{
			char a[2] = ".";
		//++ space, ., ;
		if (wParam == VK_OEM_PLUS) // '+' key
			InputMgr::GetInputMgr()->SpecialKeyInput('+');
		else if (wParam == VK_OEM_MINUS) // '-' key
			InputMgr::GetInputMgr()->SpecialKeyInput('-');
		else if (wParam == VK_OEM_COMMA) // ',' key
			InputMgr::GetInputMgr()->SpecialKeyInput(',');
		else if (wParam == VK_OEM_PERIOD) // '.' key
			InputMgr::GetInputMgr()->SpecialKeyInput('.');
		else if (wParam == VK_SPACE)
			InputMgr::GetInputMgr()->SpecialKeyInput(' ');
		else if (wParam == VK_BACK)
			InputMgr::GetInputMgr()->Back();
		else if (wParam == VK_SHIFT)
			InputMgr::GetInputMgr()->SetUpperCase(true);
		//number keys
		else if (wParam == 0x30 && wParam == 0x31 && wParam == 0x32 && wParam == 0x33 && wParam == 0x34 && 
				wParam == 0x35 && wParam == 0x36 && wParam == 0x37 && wParam == 0x38 && wParam == 0x39)
			InputMgr::GetInputMgr()->SpecialKeyInput((char)wParam);
		else if (wParam != VK_CONTROL && wParam != VK_TAB && wParam != VK_RETURN && wParam != VK_MENU &&
				wParam != VK_OEM_1 && wParam != VK_OEM_2 && wParam != VK_OEM_3 && wParam != VK_OEM_4 &&
				wParam != VK_OEM_5 && wParam != VK_OEM_6 && wParam != VK_OEM_7)
			InputMgr::GetInputMgr()->KeyInput((char)wParam);
		break;
		}
	case WM_KEYUP:
		if (KEY_UP(VK_LSHIFT))
			InputMgr::GetInputMgr()->SetUpperCase(false);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	//// Create a window //////////////////
	WNDCLASSEX	wcex;
	ZeroMemory(&wcex, sizeof(WNDCLASSEX));

	wcex.cbSize = sizeof(WNDCLASSEX);
	if (use_directx9)
		wcex.style = CS_HREDRAW | CS_VREDRAW;
	else
		wcex.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;

	wcex.lpfnWndProc = (WNDPROC)WindowProc;
	wcex.hInstance = hInstance;
	wcex.hCursor = LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground = (HBRUSH)COLOR_WINDOW;
	wcex.lpszClassName = processname.c_str();

	RegisterClassEx(&wcex);

	//Creat window for DirectX or OpenGL depending on whether it is full screen or not
	if (!use_directx9)
	{
		//Opengl
		if (fullscreen)												// Attempt Fullscreen Mode?
		{
			DEVMODE dmScreenSettings;								// Device Mode
			memset(&dmScreenSettings,0,sizeof(dmScreenSettings));	// Makes Sure Memory's Cleared
			dmScreenSettings.dmSize = sizeof(dmScreenSettings);		// Size Of The Devmode Structure
			dmScreenSettings.dmPelsWidth	= width;				// Selected Screen Width
			dmScreenSettings.dmPelsHeight	= height;				// Selected Screen Height
			dmScreenSettings.dmBitsPerPel	= 32;					// Selected Bits Per Pixel
			dmScreenSettings.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;

			// Try To Set Selected Mode And Get Results.  NOTE: CDS_FULLSCREEN Gets Rid Of Start Bar.
			if (ChangeDisplaySettings(&dmScreenSettings,CDS_FULLSCREEN)!=DISP_CHANGE_SUCCESSFUL)
			{
				// If The Mode Fails, Offer Two Options.  Quit Or Use Windowed Mode.
				if (MessageBox(	NULL, "The Requested Fullscreen Mode Is Not Supported By\nYour Video Card. Use Windowed Mode Instead?",
								"", MB_YESNO|MB_ICONEXCLAMATION)==IDYES)
				{
					return 0;
				}
			}
		}

		DWORD	dwExStyle = 0;
		DWORD	dwStyle = 0;
		if (fullscreen)												// Are We Still In Fullscreen Mode?
		{
			dwExStyle = WS_EX_APPWINDOW;								// Window Extended Style
			dwStyle = WS_POPUP;										// Windows Style
//			ShowCursor(FALSE);										// Hide Mouse Pointer
		}
		else
		{
			dwExStyle=WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;			// Window Extended Style
			//dwStyle = WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX;				// Windows Style
			dwStyle = WS_OVERLAPPEDWINDOW ^ WS_MAXIMIZEBOX;			//excluding maximize window
		}

		RECT WindowRect;				// Grabs Rectangle Upper Left / Lower Right Values
		WindowRect.left=(long)0;			// Set Left Value To 0
		WindowRect.right=(long)width;		// Set Right Value To Requested Width
		WindowRect.top=(long)0;				// Set Top Value To 0
		WindowRect.bottom=(long)height;		// Set Bottom Value To Requested Height

		AdjustWindowRectEx(&WindowRect, dwStyle, FALSE, dwExStyle);		// Adjust Window To True Requested Size

		windowhandle = CreateWindowEx(	dwExStyle,							// Extended Style For The Window
										processname.c_str(),							// Class Name
										openglwindowname.c_str(),					// Window Title
										dwStyle | WS_CLIPSIBLINGS |	WS_CLIPCHILDREN,					// Required Window Style
										0, 0,								// Window Position
										width, height,
										NULL,								// No Parent Window
										NULL,								// No Menu
										hInstance,							// Instance
										NULL );								// Dont Pass Anything To WM_CREATE
	}
	else	// using DirectX
	{
		DWORD dwStyle = 0;

		if (fullscreen)
			dwStyle = WS_EX_TOPMOST|WS_POPUP;
		else
			dwStyle = WS_OVERLAPPEDWINDOW ^ WS_MAXIMIZEBOX;	

		windowhandle = CreateWindowEx(NULL, processname.c_str(), directx9windowname.c_str(), dwStyle,
									  0, 0, width, height, NULL, NULL, hInstance, NULL);
	}

	// Setup parameters for the render engine
	RenderSetup	rs;
	rs.width = width;
	rs.height = height;
	rs.full_screen = fullscreen;
	if(use_directx9)
		rs.renderer_type = DIRECTX9_RENDERING_SYSTEM;
	else	
		rs.renderer_type = OPENGL_RENDERING_SYSTEM;
	rs.window_handle = windowhandle;

	AppMain::GetInstance()->Init(rs);

	Run();

	AppMain::GetInstance()->Shutdown();
	return 0;
}
