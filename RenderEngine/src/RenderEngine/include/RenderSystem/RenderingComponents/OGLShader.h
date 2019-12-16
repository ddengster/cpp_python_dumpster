
#ifndef _OGLSHADER_H
#define _OGLSHADER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/Shader.h"
#include "RenderSystem/GLHeaders.h"

DECLARE_ENGINE_NAMESPACE

class OGLGPUProgram;

class OGLShader : public Shader
{
protected:
	//todo: use one instead of 2?
	friend class OGLGPUProgram;
	GLuint	mOGLVertexShader;
	GLuint	mOGLPixelShader;
	
	/*LPDIRECT3DVERTEXSHADER9		mD3DVertexShader;
	LPDIRECT3DPIXELSHADER9		mD3DPixelShader;

	LPD3DXCONSTANTTABLE			mD3DConstantTable;*/

public:
	void Initialize(const String& name, const String &path, SHADER_TYPE type,
					SHADER_VERSION version, const String& entry_name,
					const String& data);
	OGLShader();
	virtual ~OGLShader();
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

class OGLGPUProgram : public GPUProgram
{
protected:
	friend class OGLShader;
	GLuint mProgram;

public:
	OGLGPUProgram();
	virtual ~OGLGPUProgram();

	void Initialize();
	virtual void Destroy()
	{
		GPUProgram::Destroy();
	}
	
	void	AttachShader(Shader* sh);
	void	DetachVertexShader();
	void	DetachPixelShader();
	void	DetachBothShaders();

	void	Bind();

	// set or unset the shaders for using
	void	SetGPUProgram();
	void	UnsetGPUProgram();
};


END_ENGINE_NAMESPACE

#endif
