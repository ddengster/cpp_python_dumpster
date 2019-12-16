
#ifndef _DX9SHADER_H
#define _DX9SHADER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/Shader.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class DX9Shader : public Shader
{
private:
	LPDIRECT3DDEVICE9			mpD3DDevice;
	
	//todo: use one instead of 2?
	LPDIRECT3DVERTEXSHADER9		mD3DVertexShader;
	LPDIRECT3DPIXELSHADER9		mD3DPixelShader;

	LPD3DXCONSTANTTABLE			mD3DConstantTable;

public:
	void Initialize(const String& name, const String &path, SHADER_TYPE type,
					SHADER_VERSION version, const String& entry_name,
					const String& data, LPDIRECT3DDEVICE9 dev);
	DX9Shader();
	virtual ~DX9Shader();
	void Destroy();

	// these two functions are called by GPUProgram class
	void	SetShader();
	void	UnsetShader();

	void	SetFloatByName(const String& name, float v);
	void	SetFloat2ByName(const String& name, const Vector2& v2);
	void	SetFloat3ByName(const String& name, const Vector3& v3);
	void	SetFloat4ByName(const String& name, const Vector4& v4);
	void	SetFloat4x4ByName(const String& name, const Matrix4& m4);

	int32_t	GetSamplerID(const String& name);

};

class DX9GPUProgram : public GPUProgram
{
public:
	DX9GPUProgram();
	virtual ~DX9GPUProgram();

	virtual void Initialize()
	{
		GPUProgram::Initialize();
	}

	virtual void Destroy()
	{
		GPUProgram::Destroy();
	}

};

END_ENGINE_NAMESPACE

#endif