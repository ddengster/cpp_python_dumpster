
#include "RenderSystem/RenderingComponents/DX9Renderer.h"
#include "RenderSystem/RenderEngine.h"
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"
#include "Math/Matrix4.h"
#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

DX9Renderer::DX9Renderer()
:mWindowHandle(NULL), mpD3DDevice(NULL)
{
}

DX9Renderer::~DX9Renderer()
{
	Shutdown();
}

bool DX9Renderer::Init()
{
	mWindowHandle = (HWND)RenderEngine::GetInstance()->GetRenderSetup().window_handle;

	LPDIRECT3D9 mpD3DObject = Direct3DCreate9(D3D_SDK_VERSION);
	if (mpD3DObject == NULL)
	{
		throw std::exception("[DX9Renderer] Could not create Direct3D Object");
	}

	///////// prepare presentation parameter ////////////////

	D3DPRESENT_PARAMETERS	d3d_pp;
	ZeroMemory(&d3d_pp, sizeof(d3d_pp));

	bool is_windowed = !(RenderEngine::GetInstance()->GetRenderSetup().full_screen);

	if (!is_windowed)
	{
		d3d_pp.BackBufferWidth = RenderEngine::GetInstance()->GetRenderSetup().width;
		d3d_pp.BackBufferHeight = RenderEngine::GetInstance()->GetRenderSetup().height;
		d3d_pp.BackBufferFormat = D3DFMT_A8R8G8B8;
		d3d_pp.FullScreen_RefreshRateInHz = D3DPRESENT_RATE_DEFAULT;
	}
	else
	{
		d3d_pp.BackBufferFormat = D3DFMT_UNKNOWN;
		d3d_pp.FullScreen_RefreshRateInHz = 0;
	}
	d3d_pp.BackBufferCount = 1;
	d3d_pp.Windowed = is_windowed;
	d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
	d3d_pp.EnableAutoDepthStencil = true;
	d3d_pp.AutoDepthStencilFormat = D3DFMT_D24S8;
	d3d_pp.hDeviceWindow = mWindowHandle;
	d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
	
	if (FAILED(mpD3DObject->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL,
										mWindowHandle, D3DCREATE_HARDWARE_VERTEXPROCESSING,
										&d3d_pp, &mpD3DDevice)))
	{
		LogManager::GetInstance()->AppendToLog("[DX9Renderer] Could not create Direct3D Device");
		return false;
	}
	SAFE_RELEASE(mpD3DObject);

	CheckDeviceCaps();
	// Set up default settings
	mpD3DDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	mpD3DDevice->SetRenderState(D3DRS_LIGHTING, FALSE);
	/*mpD3DDevice->SetRenderState(D3DRS_COLORVERTEX, TRUE);
	mpD3DDevice->SetRenderState(D3DRS_ZFUNC, D3DCMP_LESS); // Same as OpenGL default
	mpD3DDevice->SetRenderState(D3DRS_SPECULARENABLE, TRUE); // For Shininess
	*/
	LogManager::GetInstance()->AppendToLog("[DX9Renderer] Initialised successfully");

	return true;
}

void DX9Renderer::Shutdown()
{
	SAFE_RELEASE(mpD3DDevice);
}

void DX9Renderer::StartRendering()
{
	//TODO: Check device?
	if (CheckDevice() == false)
	{
		throw std::exception("Could not get d3ddevice");
		return;
	}
	mpD3DDevice->BeginScene();
}

void DX9Renderer::EndRendering()
{
	mpD3DDevice->EndScene();
	mpD3DDevice->Present(0, 0, 0, 0);
}

void DX9Renderer::ClearFrame(uint32_t target, Real r, Real g, Real b, Real a, Real z, uint32_t s)
{
	DWORD flags = 0;

	if (target | CLEAR_TARGET)	
		flags |= D3DCLEAR_TARGET;
	if (target | CLEAR_ZBUFFER)	
		flags |= D3DCLEAR_ZBUFFER;
	if (target | CLEAR_STENCIL)	
		flags |= D3DCLEAR_STENCIL;

	mpD3DDevice->Clear(0, 0, flags, D3DXCOLOR(r, g, b, a), z, s);
}


bool DX9Renderer::SetRenderState(RENDERSTATE renderstate, uint32_t value)
{
	D3DRENDERSTATETYPE dxRenderState;
	DWORD dxValue;

	// TODO: optimise this
	switch (renderstate)
	{
		case RS_SHADING:
			{
				dxRenderState = D3DRS_SHADEMODE;
				switch (value)
				{
					case SHADEMODE_FLAT:
						dxValue = D3DSHADE_FLAT;
						break;
					case SHADEMODE_SMOOTH:
						dxValue = D3DSHADE_GOURAUD;
						break;
					default:
						return false;
				}
				break;
			}
		case RS_DEPTH_TESTING:
			{
				dxRenderState = D3DRS_ZENABLE;
				dxValue = (value == DEPTHMODE_DISABLE) ? D3DZB_FALSE : D3DZB_TRUE;
				break;
			}
		case RS_CULLING:
			{
				dxRenderState = D3DRS_CULLMODE;
				switch (value)
				{
					case CULLMODE_DISABLE:
						dxValue = D3DCULL_NONE;
						break;
					case CULLMODE_CW:
						dxValue = D3DCULL_CW;
						break;
					case CULLMODE_CCW:
						dxValue = D3DCULL_CCW;
						break;
					default:
						return false;
				}
				break;
			}
		case RS_FILLMODE:
			{
				dxRenderState = D3DRS_FILLMODE;
				switch (value)
				{
					case FILLMODE_POINT:
						dxValue = D3DFILL_POINT;
						break;
					case FILLMODE_WIREFRAME:
						dxValue = D3DFILL_WIREFRAME;
						break;
					case FILLMODE_SOLID:
						dxValue = D3DFILL_SOLID;
						break;
					default:
						return false;
				}
				break;
			}
		case RS_LIGHTING:
			{
				dxRenderState = D3DRS_LIGHTING;
				dxValue = (value == LIGHTING_DISABLE) ? FALSE : TRUE;
				break;
			}
		case RS_BLENDING:
			{
				dxRenderState = D3DRS_ALPHABLENDENABLE;
				if ( value == BLENDING_ENABLE )
					dxValue = TRUE;
				else
					dxValue = FALSE;
			}
		case RS_TEXTURE_2D:
			{
				if (value == TEXTURE_2D_DISABLE)
				{
					for(uint32_t i = 0; i < 8; ++i)
						mpD3DDevice->SetTexture(i, NULL);
					return true;
				}
				else if (value == TEXTURE_2D_ENABLE)
				{
					// Don't do anything?
					return true;
				}
				else
					return false;
				break;
			}
		default:
			return false;
	}

	mpD3DDevice->SetRenderState(dxRenderState, dxValue);

	return true;
}

void DX9Renderer::SetViewPort(Real x, Real y, Real width, Real height)
{
    // DirectX defines the full-sized viewport to have origin at the upper
    // left corner of the screen. The engine uses the OpenGL convention that
    // fB specifies the relative distance from the bottom of the screen.
    // DirectX needs a specification of relative distance from the top of
    // the screen, which is 1-y.
	// Note: Should Call this again after setting your render target surface.
	//Todo: triangles still drawn inaccurately?
	//+ Inconsistency
/* Having weird problems, commenting out...
	D3DVIEWPORT9 viewport;
	viewport.X = x;
	viewport.Y = (height - y);
	//viewport.Y = y;
	viewport.Width = width;
	viewport.Height = height;
	viewport.MinZ = 0.0f;
	viewport.MaxZ = 1.0f;

	if (FAILED(mpD3DDevice->SetViewport(&viewport)))
	{
		throw std::exception("fail viewport");
	}
	*/
}

void DX9Renderer::SetPerspective(Real fov, Real nearDist, Real farDist, int32_t width, int32_t height)
{
	D3DXMATRIX projMat;
	D3DXMatrixIdentity(&projMat);
	// In order to be exactly the same as the OGL renderer, we can't use the following function exactly
	D3DXMatrixPerspectiveFovRH(&projMat, Degree(fov).ConvertToRadian().GetValue(), (Real)width/(Real)height, nearDist, farDist);
	projMat._33 = (farDist + nearDist) / (nearDist - farDist);
	projMat._43 *= 2.0f;

	mpD3DDevice->SetTransform(D3DTS_PROJECTION, &projMat);
}

void DX9Renderer::SetOrtho(int32_t left, int32_t right, int32_t bottom, int32_t top, Real nearDist, Real farDist)
{
	Real rlSum = right + left;
	Real rlDif = right - left;
	Real tbSum = top + bottom;
	Real tbDif = top - bottom;
	Real fnSum = farDist + nearDist;
	Real fnDif = farDist - nearDist;

	Matrix4 m(
		2.0f / rlDif, 0, 0, - rlSum / rlDif,
		0, 2.0f / tbDif, 0, - tbSum / tbDif,
		0, 0, 2.0f / fnDif,   fnSum / fnDif,
		0, 0, 0, 1);

	SetMatrix(PROJECTION_MATRIX, m);
}

void DX9Renderer::LookAt(const Vector3& pos,
						 const Vector3& lookat,
						 const Vector3& up)
{
	D3DXMATRIX viewMat;

	D3DXVECTOR3 eyePos(pos.x, pos.y, pos.z);
	D3DXVECTOR3 targetPos(lookat.x, lookat.y, lookat.z);
	D3DXVECTOR3 upVec(up.x, up.y, up.z);

	D3DXMatrixLookAtRH(&viewMat, &eyePos, &targetPos, &upVec);

	mpD3DDevice->SetTransform(D3DTS_VIEW, &viewMat);
}

void DX9Renderer::GetMatrix(MATRIX_TYPE type, Matrix4 *matrix)
{
	if (type >= NUM_MATRICES || type < 0)
	{
		LogManager::GetInstance()->AppendToLog("[DX9Renderer] Invalid matrix mode");
		return;
	}

	switch(type)
	{
	case WORLD_MATRIX:
		{
			D3DXMATRIX	d3d_model;
			mpD3DDevice->GetTransform(D3DTS_WORLD, &d3d_model);
			_ConvertD3DMatrixToEngineMatrix(matrix, &d3d_model);
			break;
		}
	case VIEW_MATRIX:
		{
			D3DXMATRIX	d3d_view;
			mpD3DDevice->GetTransform(D3DTS_VIEW, &d3d_view);
			_ConvertD3DMatrixToEngineMatrix(matrix, &d3d_view);
			break;
		}
	case PROJECTION_MATRIX:
		{
			D3DXMATRIX	d3d_proj;
			mpD3DDevice->GetTransform(D3DTS_PROJECTION, &d3d_proj);
			_ConvertD3DMatrixToEngineMatrix(matrix, &d3d_proj);
			break;
		}
	}
}

void DX9Renderer::SetMatrix(MATRIX_TYPE type, const Matrix4& matrix)
{
	if (type >= NUM_MATRICES || type < 0)
	{
		LogManager::GetInstance()->AppendToLog("[DX9Renderer] Invalid matrix mode");
		return;
	}

	D3DXMATRIX d3d_matrix;
	_ConvertEngineMatrixToD3DMatrix(&d3d_matrix, &matrix);

	switch(type)
	{
	case WORLD_MATRIX:
		{
			mpD3DDevice->SetTransform(D3DTS_WORLD, &d3d_matrix);
			return;
		}
	case VIEW_MATRIX:
		{
			mpD3DDevice->SetTransform(D3DTS_VIEW, &d3d_matrix);
			return;
		}
	case PROJECTION_MATRIX:
		{
			mpD3DDevice->SetTransform(D3DTS_PROJECTION, &d3d_matrix);
			return;
		}
	}
}

void DX9Renderer::DrawPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount)
{
	mpD3DDevice->DrawPrimitive(_ConvertToD3DPrimitiveType(type), start, polycount);
}

void DX9Renderer::DrawIndexedPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount)
{
	VertexBuffer* cur_vert_buf = HardwareBufferManager::GetInstance()->GetCurrentVertexBuffer();

	mpD3DDevice->DrawIndexedPrimitive(_ConvertToD3DPrimitiveType(type), 0, 0, cur_vert_buf->GetNumberOfVertices(), start, polycount);
}

void DX9Renderer::CheckDeviceCaps()
{
	mpD3DDevice->GetDeviceCaps(&mDeviceCaps);
}

bool DX9Renderer::CheckDevice()
{
	// Check device state
	switch (mpD3DDevice->TestCooperativeLevel())
	{
		case D3DERR_DEVICELOST:
			return false;
		case D3DERR_DEVICENOTRESET:
			{
				// Reset device

				//TODO
				//Might crash
				HardwareBufferManager::GetInstance()->DestroyAllVertexBuffer();
				HardwareBufferManager::GetInstance()->DestroyAllIndexBuffer();
				HardwareBufferManager::GetInstance()->DestroyAllVertexDeclaration();

				/*if (FAILED(mpD3DDevice->Reset(&mPresentParameters)))
				{
					throw RendererException("[DX9Renderer] Failed to reset DirectX 9 device");
					return false;
				}

				// Init scene again
				SetDefaultSettings();
				SetDefaultMaterial();

				MEdgeMain::GetInstance()->PostDeviceReset();*/

				//Reshape(mPresentParameters.BackBufferWidth, mPresentParameters.BackBufferHeight);

				//if (!SetupOcclusion())
				//	return false;
				return true;
			}
		default:
			return true;
	}
}

D3DPRIMITIVETYPE DX9Renderer::_ConvertToD3DPrimitiveType(PRIMITIVE_TYPE type)
{
	static D3DPRIMITIVETYPE	d3d_table[NUM_PRIMITIVES] = {	D3DPT_POINTLIST,
															D3DPT_TRIANGLELIST,
															D3DPT_TRIANGLESTRIP,
															D3DPT_TRIANGLEFAN,
															D3DPT_LINELIST,
															D3DPT_LINESTRIP
															//TODO: QUAD LIST AND POLYGON LIST
	};

	return d3d_table[type];
}

void DX9Renderer::_ConvertD3DMatrixToEngineMatrix(Matrix4* out, const D3DXMATRIX* in)
{
	assert(out != NULL);
	assert(in != NULL);
	for (uint32_t i=0; i<4; ++i)
	{
		for (uint32_t j=0; j<4; ++j)
		{
			(*out)[i][j] = (*in)(j, i);
		}
	}
}

void DX9Renderer::_ConvertEngineMatrixToD3DMatrix(D3DXMATRIX* out, const Matrix4* in)
{
	assert(out != NULL);
	assert(in != NULL);
	for (uint32_t i=0; i<4; ++i)
	{
		for (uint32_t j=0; j<4; ++j)
		{
			(*out)(j, i) = (*in)[i][j];
		}
	}
}

END_ENGINE_NAMESPACE