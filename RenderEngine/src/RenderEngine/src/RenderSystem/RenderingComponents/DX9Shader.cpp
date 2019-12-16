
#include "RenderSystem/RenderingComponents/DX9Shader.h"
#include "LogManager/LogManager.h"
#include "Math/Vector2.h"
#include "Math/Vector3.h"
#include "Math/Vector4.h"
#include "Math/Matrix4.h"

DECLARE_ENGINE_NAMESPACE

DX9Shader::DX9Shader()
:Shader(), mpD3DDevice(NULL), mD3DVertexShader(NULL), mD3DPixelShader(NULL), mD3DConstantTable(NULL)
{
}

void DX9Shader::Initialize(	const String& name, const String &path, SHADER_TYPE type,
							 SHADER_VERSION version, const String& entry_name,
							 const String& data, LPDIRECT3DDEVICE9 dev)
{
	Shader::Initialize(name, path, type, version, entry_name);
	mpD3DDevice = dev;

	LPD3DXBUFFER	shader_buf = NULL;
	LPD3DXBUFFER	msg_buf = NULL;
	mD3DConstantTable = NULL;

	static const String ver_table[NUM_SHADER_VERSION] = 
	{ 
		"vs_1_1",
		"vs_2_0",
		"vs_2_a",
		"vs_3_0",
		"ps_1_1",
		"ps_1_2",
		"ps_1_3",
		"ps_1_4",
		"ps_2_0",
		"ps_2_a",
		"ps_2_b",
		"ps_3_0"
	};

	String		version_name;
	if (mVersion >= DX9_VS_1_1 && mVersion <= DX9_PS_3_0)
		version_name = ver_table[mVersion];
	else
		LogManager::GetInstance()->AppendToLog(name + " : Unknown shader version!");

	//todo: set to d3dxshader non debug?
	D3DXCompileShader(data.c_str(), (uint32_t)data.length(), NULL, NULL, mEntryFunctionName.c_str(), version_name.c_str(), 
						D3DXSHADER_DEBUG, &shader_buf, &msg_buf, &mD3DConstantTable);

	if (msg_buf != NULL)
	{
		if (mShaderType == VERTEX_SHADER)
		{
			LogManager::GetInstance()->AppendToLog("\nVertex Shader Compile Info: \n" + name);
		}

		if (mShaderType == PIXEL_SHADER)
		{
			LogManager::GetInstance()->AppendToLog("\nPixel Shader Compile Info: \n" + name);
		}

		LogManager::GetInstance()->AppendToLog(String((char*)msg_buf->GetBufferPointer()));
	}

	if (mShaderType == VERTEX_SHADER)
	{
		mpD3DDevice->CreateVertexShader((DWORD*)shader_buf->GetBufferPointer(), &mD3DVertexShader);
		mD3DPixelShader = NULL;
	}

	if (mShaderType == PIXEL_SHADER)
	{
		mpD3DDevice->CreatePixelShader((DWORD*)shader_buf->GetBufferPointer(), &mD3DPixelShader);
		mD3DVertexShader = NULL;
	}

	SAFE_RELEASE(shader_buf);
	SAFE_RELEASE(msg_buf);
}

DX9Shader::~DX9Shader()
{
	Destroy();
}

void DX9Shader::Destroy()
{
	SAFE_RELEASE(mD3DVertexShader);
	SAFE_RELEASE(mD3DPixelShader);
	SAFE_RELEASE(mD3DConstantTable);
	Shader::Destroy();
}

void DX9Shader::SetShader()
{
	if (mShaderType == VERTEX_SHADER)
	{
		mpD3DDevice->SetVertexShader(mD3DVertexShader);
	}

	if (mShaderType == PIXEL_SHADER)
	{
		mpD3DDevice->SetPixelShader(mD3DPixelShader);
	}
}

void DX9Shader::UnsetShader()
{
	if (mShaderType == VERTEX_SHADER)
	{
		mpD3DDevice->SetVertexShader(NULL);
	}

	if (mShaderType == PIXEL_SHADER)
	{
		mpD3DDevice->SetPixelShader(NULL);
	}
}

void DX9Shader::SetFloatByName(const String& name, float v)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());
	mD3DConstantTable->SetFloat(mpD3DDevice, constant_hdl, v);
}

void DX9Shader::SetFloat2ByName(const String& name, const Vector2& v2)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());
	mD3DConstantTable->SetFloatArray(mpD3DDevice, constant_hdl, v2.ptr(), 2);
}

void DX9Shader::SetFloat3ByName(const String& name, const Vector3& v3)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());
	mD3DConstantTable->SetFloatArray(mpD3DDevice, constant_hdl, &v3.x, 3);
}

void DX9Shader::SetFloat4ByName(const String& name, const Vector4& v4)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());
	mD3DConstantTable->SetFloatArray(mpD3DDevice, constant_hdl, &v4.x, 4);
}

void DX9Shader::SetFloat4x4ByName(const String& name, const Matrix4& m4)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());

	mD3DConstantTable->SetFloatArray(mpD3DDevice, constant_hdl, m4[0], 16);
}

int32_t DX9Shader::GetSamplerID(const String& name)
{
	D3DXHANDLE	constant_hdl = mD3DConstantTable->GetConstantByName(NULL, name.c_str());

	return	mD3DConstantTable->GetSamplerIndex(constant_hdl);
}

////////////// DX9GPUProgram //////////////////////

DX9GPUProgram::DX9GPUProgram() 
{
}

DX9GPUProgram::~DX9GPUProgram()
{
}

END_ENGINE_NAMESPACE
