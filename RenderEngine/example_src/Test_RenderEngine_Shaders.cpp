
#include "Headers.h"

using namespace ddengine_RenderEngine;

const String openglwindowname = "OpenGL: Test_RenderEngine_Shaders";
const String directx9windowname = "Directx9: Test_RenderEngine_Shaders";
const String processname = "Test_RenderEngine_Shaders";

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

GPUProgram* gpu_pgm;

void Init()
{
	IRenderer *renderer = RenderEngine::GetInstance()->GetRenderer();

	renderer->SetViewPort(0, 0, 800, 600);
	renderer->SetPerspective(45.0f, 0.1f, 1000.0f, 800, 600);
	renderer->LookAt(Vector3(-10, 0, 15), Vector3(10, 1, -20), Vector3(0, 1, 0));

	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	/** Obj1: uses 3 floats for position and uses 4 floats for color + trianglelist**/
	VertexDeclaration* obj1_vert_dec = hwbmgr->GenerateVertexDeclaration();
	obj1_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	obj1_vert_dec->AddVertexElement(ELE_USG_NORMAL, ELE_TYP_FLOAT3, 0);
	obj1_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);
	obj1_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT3, 1);

	obj1_v_buf = hwbmgr->GenerateVertexBuffer(obj1_vert_dec, 24, HDW_BUF_USG_STATIC);
	Real obj1_data[][11] = {
			// side 1
			{ -3.0f, 3.0f, -3.0f,  0, 0, -1, 0, 0, -1, 0, 0 },
			{ 3.0f, 3.0f, -3.0f,  0, 0, -1, 1, 0, -1, 0, 0},
			{ 3.0f, -3.0f, -3.0f,  0, 0, -1, 1, 1, -1, 0, 0 },
			{ -3.0f, -3.0f, -3.0f,  0, 0, -1, 0, 1, -1, 0, 0 },

			// side 2
			{ -3.0f, 3.0f, 3.0f,  0, 0, 1, 0, 0, 1, 0, 0 },
			{ -3.0f, -3.0f, 3.0f,  0, 0, 1 , 1, 0, 1, 0, 0 },
			{ 3.0f, -3.0f, 3.0f,  0, 0, 1 , 1, 1, 1, 0, 0 },
			{ 3.0f, 3.0f, 3.0f,  0, 0, 1, 0, 1, 1, 0, 0  },

			// side 3
			{ -3.0f, 3.0f, 3.0f, 0, 1, 0, 0, 0, 0, 0, -1},
			{ 3.0f, 3.0f, 3.0f,  0, 1, 0, 1, 0, 0, 0, -1},
			{ 3.0f, 3.0f, -3.0f,  0, 1, 0, 1, 1, 0, 0, -1},
			{ -3.0f, 3.0f, -3.0f,  0, 1, 0, 0, 1, 0, 0, -1},

			// side 4
			{ -3.0f, -3.0f, 3.0f,  0, -1, 0, 0, 0, 0, 0, 1 },
			{ -3.0f, -3.0f, -3.0f, 0, -1, 0, 1, 0, 0, 0, 1 },
			{ 3.0f, -3.0f, -3.0f,  0, -1, 0, 1, 1, 0, 0, 1 },
			{ 3.0f, -3.0f, 3.0f, 0, -1, 0, 0, 1, 0, 0, 1},

			// side 5
			{ 3.0f, 3.0f, -3.0f, 1, 0, 0, 0, 0, 1, 0, 0 },
			{ 3.0f, 3.0f, 3.0f, 1, 0, 0, 1, 0, 1, 0, 0 },
			{ 3.0f, -3.0f, 3.0f, 1, 0, 0, 1, 1, 1, 0, 0  },
			{ 3.0f, -3.0f, -3.0f, 1, 0, 0, 0, 1, 1, 0, 0 },

			// side 6
			{ -3.0f, 3.0f, -3.0f, -1, 0, 0, 0, 0, -1, 0, 0 },
			{ -3.0f, -3.0f, -3.0f, -1, 0, 0, 1, 0, -1, 0, 0 },
			{ -3.0f, -3.0f, 3.0f, -1, 0, 0, 1, 1, -1, 0, 0 },
			{ -3.0f, 3.0f, 3.0f, -1, 0, 0, 0, 1, -1, 0, 0 },
		};   
	obj1_v_buf->UpdateData(obj1_data, 0, 24);

	obj1_i_buf = hwbmgr->GenerateIndexBuffer(36, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t obj1_indices[] =  { 0, 1, 2,
								 0, 2, 3,

								 4, 5, 6,
								 4, 6, 7,

								 8, 9, 10,
								 8, 10, 11,

								 12, 13, 14,
								 12, 14, 15,

								 16, 17, 18,
								 16, 18, 19,

								 20, 21, 22,
								 20, 22, 23
	};
	obj1_i_buf->UpdateData(obj1_indices, 0, 36);

	texture1 = TextureManager::GetInstance()->GetTexture2D( "Fieldstone.tga", "./Data/Textures/");
	texture2 = TextureManager::GetInstance()->GetTexture2D( "FieldstoneBumpDOT3.tga", "./Data/Textures/");

	/* ogl */
	Shader* vert_sh = ShaderManager::GetInstance()->GetShader("toon.vert", "./Data/Shaders/", VERTEX_SHADER, OGL_VS);
	Shader* pix_sh = ShaderManager::GetInstance()->GetShader("toon.frag", "./Data/Shaders/", PIXEL_SHADER, OGL_PS);

	/* dx9
	Shader* vert_sh = ShaderManager::GetInstance()->GetShader("simple_vs.hlsl", "./Data/Shaders/", VERTEX_SHADER, DX9_VS_2_0, "vs_main");
	Shader* pix_sh = ShaderManager::GetInstance()->GetShader("simple_ps.hlsl", "./Data/Shaders/", PIXEL_SHADER, DX9_PS_2_0, "ps_main");
	*/

	gpu_pgm = ShaderManager::GetInstance()->CreateGPUProgram();

	gpu_pgm->AttachShader(vert_sh);
	gpu_pgm->AttachShader(pix_sh);
	gpu_pgm->Bind();

	/* ogl shows black and white :(*/
	vert_sh->SetFloat3ByName("DiffuseColor", Vector3(1.0, 1.0, 0.0));
	vert_sh->SetFloat3ByName("PhongColor", Vector3(0.0, 0.0, 1.0));
	vert_sh->SetFloatByName("Phong", 0.68);
	vert_sh->SetFloatByName("Edge", -0.47);

	/* dx9
	vert_sh->SetParameterAuto("wld_view_proj_mtx", SPA_WORLD_VIEW_PROJECTION_MATRIX);
	vert_sh->SetParameterAuto("wld_IT_mtx", SPA_WORLD_IT_MATRIX);
	vert_sh->SetParameterAuto("wld_mtx", SPA_WORLD_MATRIX);
	vert_sh->SetParameterAuto("eye_position", SPA_CAMERA_POSITION);
	vert_sh->SetFloat4ByName("light_position", Vector4(0, 6, 6, 1));

	pix_sh->SetFloat3ByName("attenuation", Vector3(1, 0, 0));
	pix_sh->SetParameterAuto("wld_IT_mtx", SPA_WORLD_IT_MATRIX);

	texture1->SetTexture(pix_sh->GetSamplerID("color_map"));
	texture1->SetTextureParameter(MIN_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	texture1->SetTextureParameter(MAG_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);

	texture2->SetTexture(pix_sh->GetSamplerID("normal_map"));
	texture2->SetTextureParameter(MIN_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	texture2->SetTextureParameter(MAG_FILTER, FILTER_LINEAR_MIPMAP_LINEAR);
	*/
}

void Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->ClearFrame(CLEAR_TARGET | CLEAR_ZBUFFER | CLEAR_STENCIL ,
								 0.0, 0.2, 0.8, 1.0, 1.0, 0);
	
	renderer->StartRendering();

	obj1_v_buf->SetBuffer();
	obj1_i_buf->SetBuffer();

	gpu_pgm->SetGPUProgram();

	gpu_pgm->Update(0);

	/* ogl */
	Shader* vert_sh = ShaderManager::GetInstance()->GetShader("toon.vert", "./Data/Shaders/", VERTEX_SHADER, OGL_VS);
	vert_sh->SetFloat3ByName("DiffuseColor", Vector3(1.0, 1.0, 0.0));
	vert_sh->SetFloat3ByName("PhongColor", Vector3(0.0, 0.0, 1.0));
	vert_sh->SetFloatByName("Phong", 0.68);
	vert_sh->SetFloatByName("Edge", -0.47);

	renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, obj1_i_buf->GetNumOfIndices()/3);

	gpu_pgm->UnsetGPUProgram();

	obj1_v_buf->UnsetBuffer();
	obj1_i_buf->UnsetBuffer();

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
