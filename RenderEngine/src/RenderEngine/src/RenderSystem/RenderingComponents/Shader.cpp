
#include "RenderSystem/RenderingComponents/Shader.h"
#include "RenderSystem/RenderingComponents/ShaderManager.h"
#include "RenderSystem/RenderingComponents/IRenderer.h"
#include "RenderSystem/RenderEngine.h"
#include "Math/Vector2.h"
#include "Math/Vector3.h"
#include "Math/Vector4.h"
#include "Math/Matrix4.h"

DECLARE_ENGINE_NAMESPACE

Shader::Shader()
:Resource(), /*mShaderType(0), mVersion(0),*/ mEntryFunctionName(""), mCurrentRenderer(0), mProgramAttachedTo(0)
{
}

void Shader::Initialize(const String& name, const String &path, SHADER_TYPE type,
						SHADER_VERSION version, const String& entry_name)
{
	Resource::Initialize(name, path);
	mShaderType = type;
	mVersion = version;
	mEntryFunctionName = entry_name;

	mProgramAttachedTo = NULL;

	mAutoParameters.clear();
	mCurrentRenderer = RenderEngine::GetInstance()->GetRenderer();
}

Shader::~Shader()
{
	Destroy();
}

void Shader::Destroy()
{
	mProgramAttachedTo = NULL;
	Resource::Destroy();
}

bool Shader::SetParameterAuto(const String& name, SHADER_PARAMETER_AUTO spa)
{
	mAutoParameters.insert(std::pair<String, uint32_t>(name, (uint32_t)spa));
	return true;
}

void Shader::Update(Real delta_t)
{
	std::map<String, uint32_t>::iterator iter = mAutoParameters.begin();

	String name;
	uint32_t spa;
	for (; iter != mAutoParameters.end(); ++iter)
	{
		name = iter->first;
		spa = iter->second;

		switch(spa)
		{
		case SPA_WORLD_MATRIX:
			{
				Matrix4	wld_mtx;
				mCurrentRenderer->GetMatrix(WORLD_MATRIX, &wld_mtx);
				SetFloat4x4ByName(name, wld_mtx);
				break;
			}
		case SPA_VIEW_MATRIX:
			{
				Matrix4	view_mtx;
				mCurrentRenderer->GetMatrix(VIEW_MATRIX, &view_mtx);
				SetFloat4x4ByName(name, view_mtx);
				break;
			}
		case SPA_PROJECTION_MATRIX:
			{
				Matrix4	proj_mtx;
				mCurrentRenderer->GetMatrix(PROJECTION_MATRIX, &proj_mtx);
				SetFloat4x4ByName(name, proj_mtx);
				break;
			}
		case SPA_WORLD_VIEW_PROJECTION_MATRIX:
			{
				Matrix4	wld_mtx;
				mCurrentRenderer->GetMatrix(WORLD_MATRIX, &wld_mtx);

				Matrix4	view_mtx;
				mCurrentRenderer->GetMatrix(VIEW_MATRIX, &view_mtx);
			
				Matrix4	proj_mtx;
				mCurrentRenderer->GetMatrix(PROJECTION_MATRIX, &proj_mtx);

				Matrix4	wld_view_proj_mtx = proj_mtx*view_mtx*wld_mtx;
				SetFloat4x4ByName(name, wld_view_proj_mtx);
				break;
			}
		case SPA_WORLD_IT_MATRIX:
			{
				Matrix4	wld_mtx;
				mCurrentRenderer->GetMatrix(WORLD_MATRIX, &wld_mtx);
				Matrix4	wld_it_mtx = wld_mtx.Inverse()*wld_mtx.Transpose();
				SetFloat4x4ByName(name, wld_it_mtx);
				break;
			}
		case SPA_CAMERA_POSITION:
			{
				Matrix4	view_mtx;
				mCurrentRenderer->GetMatrix(VIEW_MATRIX, &view_mtx);

				Vector3	pos = -view_mtx.GetTrans();
				Matrix3	view_3x3_mtx;
				view_mtx.Extract3x3Matrix(view_3x3_mtx);
				pos = view_3x3_mtx.Transpose()*pos;
				Vector4	cam_pos(pos);
				SetFloat4ByName(name, cam_pos);

				break;
			}
		}
	}
}

///////////////////////////////////////

void GPUProgram::AttachShader(Shader* sh)
{
	sh->mProgramAttachedTo = this;
	if (sh->GetShaderType() == VERTEX_SHADER)
		mVertexShader = sh;
	else //if (sh->GetShaderType() == PIXEL_SHADER)
		mPixelShader = sh;
}

void GPUProgram::DetachVertexShader()
{
	mVertexShader->mProgramAttachedTo = NULL;
	mVertexShader = NULL;
}

void GPUProgram::DetachPixelShader()
{
	mPixelShader->mProgramAttachedTo = NULL;
	mPixelShader = NULL;
}

void GPUProgram::DetachBothShaders()
{
	mVertexShader->mProgramAttachedTo = NULL;
	mVertexShader = NULL;
	mPixelShader->mProgramAttachedTo = NULL;
	mPixelShader = NULL;
}

void GPUProgram::SetGPUProgram()
{
	if (mVertexShader != NULL)
		mVertexShader->SetShader();
	if (mPixelShader != NULL)
		mPixelShader->SetShader();

	ShaderManager::GetInstance()->_RegisterCurrentGPUProgram(this);
}

void GPUProgram::UnsetGPUProgram()
{
	if (mVertexShader != NULL)
		mVertexShader->UnsetShader();
	if (mPixelShader != NULL )
		mPixelShader->UnsetShader();

	ShaderManager::GetInstance()->_RegisterCurrentGPUProgram(NULL);
}

void GPUProgram::Update(Real delta_t)
{
	if (mVertexShader != NULL)
		mVertexShader->Update(delta_t);
	if (mPixelShader != NULL)
		mPixelShader->Update(delta_t);
}

END_ENGINE_NAMESPACE
