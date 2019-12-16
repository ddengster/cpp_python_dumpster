

#include "Headers.h"

using namespace ddengine_RenderEngine;

const String openglwindowname = "OpenGL: Test_SceneManager_SceneNodes";
const String directx9windowname = "Directx9: Test_SceneManager_SceneNodes";
const String processname = "Test_SceneManager_SceneNodes";

bool use_directx9 = true;
bool fullscreen = false;
uint32_t width = 800;
uint32_t height = 600;
HWND windowhandle;

#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

void Init()
{
	IRenderer *renderer = RenderEngine::GetInstance()->GetRenderer();
	SceneManager* scenemgr = RenderEngine::GetInstance()->GetSceneManager();

	renderer->SetViewPort(0, 0, 800, 600);
	renderer->SetPerspective(45.0f, 0.1f, 1000.0f, 800, 600);
	renderer->LookAt(Vector3(0, 0, 10), Vector3(0, 0, -6), Vector3(0, 1, 0));

	SceneNode* rootnode = scenemgr->GetRootSceneNode();
	//Position
	SceneNode* node02 = rootnode->CreateChildSceneNode("node02");
	node02->SetPosition(Vector3(-3, 0, 0));

	//Position + Rotation
	SceneNode* node03 = rootnode->CreateChildSceneNode("node03");
	node03->SetPosition(Vector3(2, -2, 0));
	node03->SetOrientation(Quaternion(Degree(30).ConvertToRadian(), Vector3(0, 1, 0)));

	//Position + Scaling
	SceneNode* node04 = rootnode->CreateChildSceneNode("node04");
	node04->SetPosition(Vector3(-2, -3, 0));
	node04->SetScaleFactor(Vector3(2, 3, 1));
}

void Render()
{
	SceneManager* scenemgr = RenderEngine::GetInstance()->GetSceneManager();
	scenemgr->Render();
}

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
		Render();

		if(KEY_DOWN(VK_ESCAPE))
            PostMessage(windowhandle, WM_DESTROY, 0, 0);

		SceneManager* scenemgr = RenderEngine::GetInstance()->GetSceneManager();
		/** User input based tests **/
		if (KEY_DOWN(VK_F1))
			scenemgr->GetRootSceneNode()->SetPosition(Vector3(4, 0, 0));
		if (KEY_DOWN(VK_F2))
			scenemgr->GetRootSceneNode()->SetOrientation(Quaternion(Degree(30).ConvertToRadian(), Vector3(0, 1, 0)));
		if (KEY_DOWN(VK_F3))
			scenemgr->GetRootSceneNode()->SetScaleFactor(Vector3(1.5, 1.5, 1.5));

	}
}

LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch(message)
	{
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;

	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
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
			dwStyle = WS_OVERLAPPEDWINDOW;							// Windows Style
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
			dwStyle = WS_OVERLAPPEDWINDOW;	

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

	RenderEngine::GetInstance()->CreateRenderSystem(rs);

	Init();

	Run();

	return 0;
}
