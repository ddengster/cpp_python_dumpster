
#include "Headers.h"

using namespace ddengine_RenderEngine;
bool use_directx9 = false;
bool fullscreen = false;
uint32_t width = 800;
uint32_t height = 600;
HWND windowhandle;

#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

VertexBuffer* obj1_v_buf;
VertexBuffer* obj2_v_buf;
VertexBuffer* obj3_v_buf;
IndexBuffer* obj3_i_buf;
void Init()
{
	IRenderer *renderer = RenderEngine::GetInstance()->GetRenderer();

	renderer->SetViewPort(0, 0, 800, 600);
	renderer->SetPerspective(45.0f, 0.1f, 1000.0f, 800, 600);
	renderer->LookAt(Vector3(0, 0, 10), Vector3(0, 1, -6), Vector3(0, 1, 0));

	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	/** Obj1: uses 3 floats for position and uses 4 floats for color + trianglelist**/
	VertexDeclaration* obj1_vert_dec = hwbmgr->GenerateVertexDeclaration();
	obj1_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	obj1_vert_dec->AddVertexElement(ELE_USG_COLOR, ELE_TYP_FLOAT4, 0);

	obj1_v_buf = hwbmgr->GenerateVertexBuffer(obj1_vert_dec, 3, HDW_BUF_USG_STATIC);
	Real obj1_data[][7] = {
		{ -1.0, 0, 0, 1.0, 0.0, 0.0, 1.0 },
		{ 0.0, 1.0, 0, 0.0, 1.0, 0.0, 1.0 },
		{ 1.0, 0, 0.0, 0.0, 0.0, 1.0, 1.0 }
	};
	obj1_v_buf->UpdateData(obj1_data, 0, 3);

	/** Obj2: uses 3 floats for position and uses 4 unsigned bytes for color + trianglestrip**/
	VertexDeclaration* obj2_vert_dec = hwbmgr->GenerateVertexDeclaration();
	obj2_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	obj2_vert_dec->AddVertexElement(ELE_USG_COLOR, ELE_TYP_UBYTE4, 0);

	obj2_v_buf = hwbmgr->GenerateVertexBuffer(obj2_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real obj2_data[][4] = {
		{ 1.0, 0, 0.0, COLOR_RGBA_1F(0.0, 1.0, 1.0, 1.0) },
		{ 0.0, 1.0, 0, COLOR_RGBA_1F(1.0, 1.0, 0.0, 1.0) },
		{ -2.0, 0, 0, COLOR_RGBA_1F(1.0, 0.0, 0.0, 1.0) },
		{ -2.0, 2.0, 0, COLOR_RGBA_1F(0.0, 0.0, 1.0, 1.0) }
	};
	obj2_v_buf->UpdateData(obj2_data, 0, 4);

	/** Obj3: uses 3 floats for position + linelist + uses index buffers**/
	VertexDeclaration* obj3_vert_dec = hwbmgr->GenerateVertexDeclaration();
	obj3_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);

	obj3_v_buf = hwbmgr->GenerateVertexBuffer(obj3_vert_dec, 3, HDW_BUF_USG_STATIC);
	Real obj3_data[][3] = {
		{ 0.0, 0.0, 0.0 },
		{ 1.0, 2.0, 0.0 },
		{ 1.5, 0.0, 1.0 }
	};
	obj3_v_buf->UpdateData(obj3_data, 0, 3);
	obj3_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t obj3_indices[] = { 0, 1, 1, 2, 0, 2};
	obj3_i_buf->UpdateData(obj3_indices, 0, 6);
}

void Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->ClearFrame(CLEAR_TARGET | CLEAR_ZBUFFER | CLEAR_STENCIL ,
								 0.0, 0.2, 0.8, 1.0, 1.0, 0);

	Matrix4 mat = Matrix4::IDENTITY;
	
	renderer->StartRendering();

	renderer->GetMatrix(WORLD_MATRIX, &mat);
	Vector3 trans = mat.GetTrans();
	trans = Vector3(1, 2, 0);
	mat.MakeTransform(trans, Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));

	renderer->SetMatrix(WORLD_MATRIX, mat);
	obj1_v_buf->SetBuffer();
	renderer->DrawPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_v_buf->GetNumberOfVertices()/3));
	obj1_v_buf->UnsetBuffer();

	//move -2 along x
	mat.MakeTransform(Vector3(-1, 2, 0), Vector3(2, 2, 2), Quaternion(Degree(45).ConvertToRadian(), Vector3(0,1,0)));
	renderer->SetMatrix(WORLD_MATRIX, mat);
	obj2_v_buf->SetBuffer();
	renderer->DrawPrimitive(PRIMITIVE_TRI_STRIP, 0, 2);
	obj2_v_buf->UnsetBuffer();

	//move -2 along y
	mat.MakeTransform(Vector3(0, 0, 0), Vector3(1, 1, 1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));
	renderer->SetMatrix(WORLD_MATRIX, mat);
	obj3_v_buf->SetBuffer();
	obj3_i_buf->SetBuffer();
	renderer->DrawIndexedPrimitive(PRIMITIVE_LINE_LIST, 0, obj3_i_buf->GetNumOfIndices()/2);
	obj3_i_buf->UnsetBuffer();
	obj3_v_buf->UnsetBuffer();

	renderer->EndRendering();
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
		/*unsigned long current_t = timeGetTime();
		static unsigned long last_t = current_t;
		float  delta_t = (current_t - last_t )*0.001f;	// record how much time have passed
		last_t = current_t;*/

		Render();

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
	wcex.lpszClassName = "Test_RenderEngine_HardwareBuffers";

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
										"Test_RenderEngine_HardwareBuffers",							// Class Name
										"OpenGL: Test_RenderEngine_HardwareBuffers",					// Window Title
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

		windowhandle = CreateWindowEx(NULL, "Test_RenderEngine_HardwareBuffers", "DirectX9: Test_RenderEngine_HardwareBuffers", dwStyle,
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