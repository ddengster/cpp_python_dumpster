
#include "Headers.h"

using namespace ddengine_RenderEngine;

const String openglwindowname = "OpenGL: Test_RenderEngine_Textures";
const String directx9windowname = "Directx9: Test_RenderEngine_Textures";
const String processname = "Test_RenderEngine_Textures";

bool use_directx9 = false;
bool fullscreen = false;
uint32_t width = 800;
uint32_t height = 600;
HWND windowhandle;

#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

VertexBuffer* obj1_v_buf;
IndexBuffer* obj1_i_buf;

Texture* texture1;
Texture* texture2;
Texture* texture3;
Texture* texture4;

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
	obj1_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	obj1_v_buf = hwbmgr->GenerateVertexBuffer(obj1_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real obj1_data[][5] = {
		{ -1.0, -1.0,   0,   0.0, 0.0 },
		{ 1.0,  -1.0,   0,   1.0, 0.0 },
		{ 1.0,   1.0,   0.0, 1.0, 1.0 },
		{ -1.0,  1.0,   0.0, 0.0, 1.0 }
	};
	obj1_v_buf->UpdateData(obj1_data, 0, 4);

	obj1_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t obj1_indices[] = { 0, 1, 2, 0, 3, 2 };
	obj1_i_buf->UpdateData(obj1_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	//Bmp image
	texture1 = texturemgr->GetTexture2D("Nehe.bmp","./Data/Textures/");
	//tga image + mipmap
	texture2 = texturemgr->GetTexture2D("Nehe.tga","./Data/Textures/");
	texture2->SetTextureParameter(MIN_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	texture2->SetTextureParameter(MAG_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	//resized non power 2 image
	texturemgr->SetMipmapLevel(0);
	texture3 = texturemgr->GetTexture2D("NeHe_resized.bmp","./Data/Textures/");
	//resized non power 2 image with mipmap
	texturemgr->SetMipmapLevel(4);
	texture4 = texturemgr->GetTexture2D("NeHe_resized2.bmp","./Data/Textures/");
	texture4->SetTextureParameter(MIN_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	texture4->SetTextureParameter(MAG_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	
}

void Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->ClearFrame(CLEAR_TARGET | CLEAR_ZBUFFER | CLEAR_STENCIL ,
								 0.0, 0.2, 0.8, 1.0, 1.0, 0);
	
	renderer->StartRendering();

	Matrix4 mat = Matrix4::IDENTITY;
	
	obj1_v_buf->SetBuffer();
	obj1_i_buf->SetBuffer();
	
	{
		mat.MakeTransform(Vector3(0,0,0), Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));
		renderer->SetMatrix(WORLD_MATRIX, mat);

		texture1->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_i_buf->GetNumOfIndices()/3));
		texture1->UnsetTexture();
	}

	{
		mat.MakeTransform(Vector3(2, 2, 0), Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));
		renderer->SetMatrix(WORLD_MATRIX, mat);

		texture2->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_i_buf->GetNumOfIndices()/3));
		texture2->UnsetTexture();
	}

	{
		mat.MakeTransform(Vector3(-2, 2, 0), Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));
		renderer->SetMatrix(WORLD_MATRIX, mat);

		texture3->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_i_buf->GetNumOfIndices()/3));
		texture3->UnsetTexture();
	}

	{
		mat.MakeTransform(Vector3(0, 4, 0), Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(), Vector3(0,1,0)));
		renderer->SetMatrix(WORLD_MATRIX, mat);

		texture4->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_i_buf->GetNumOfIndices()/3));
		texture4->UnsetTexture();
	}

	obj1_i_buf->UnsetBuffer();
	obj1_v_buf->UnsetBuffer();
	

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
