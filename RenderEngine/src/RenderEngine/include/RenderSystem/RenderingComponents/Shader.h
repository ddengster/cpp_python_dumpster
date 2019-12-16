
#ifndef _SHADER_H
#define _SHADER_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include "DataManagement/Resource.h"
#include <map>

DECLARE_ENGINE_NAMESPACE

class Vector2;
class Vector3;
class Vector4;
class Matrix4;
class IRenderer;
class GPUProgram;

class Shader : public Resource
{
protected:
	SHADER_TYPE		mShaderType;
	SHADER_VERSION	mVersion;
	String			mEntryFunctionName;

	std::map<String, uint32_t>		mAutoParameters;
	IRenderer*						mCurrentRenderer;

public:
	Shader();
	virtual void Initialize(const String& name, const String &path, SHADER_TYPE type,
							SHADER_VERSION version, const String& entry_name);
	virtual ~Shader();
	virtual void Destroy();

	SHADER_TYPE		GetShaderType()			{	return	mShaderType;	}
	SHADER_VERSION	GetShaderVersion()		{	return	mVersion;		}
	const String&	GetEntryFunctionName()	{	return	mEntryFunctionName;	}

	// These two functions are called by GPUProgram class
	virtual void	SetShader() = 0;
	virtual void	UnsetShader() = 0;

	// Call the following functions to set value for global variables, 
	// after Bind function of GPUProgram is called
	virtual	void	SetFloatByName(const String& name, float v) = 0;
	virtual	void	SetFloat2ByName(const String& name, const Vector2& v2) = 0;
	virtual	void	SetFloat3ByName(const String& name, const Vector3& v3) = 0;
	virtual	void	SetFloat4ByName(const String& name, const Vector4& v4) = 0;
	virtual	void	SetFloat4x4ByName(const String& name, const Matrix4& m4) = 0;

	virtual int32_t	GetSamplerID(const String& name) = 0;

	bool	SetParameterAuto(const String& name, SHADER_PARAMETER_AUTO spa);
	
	virtual	void	Update(Real delta_t);

	GPUProgram*						mProgramAttachedTo;

};

// GPUProgram is used to bind vertex shader and pixel shader together
class GPUProgram 
{
protected:
	Shader*		mVertexShader;
	Shader*		mPixelShader;

public:
	GPUProgram()
		:mVertexShader(NULL), mPixelShader(NULL)
	{
		
	}

	virtual void Initialize()
	{
		mVertexShader = NULL;
		mPixelShader = NULL;
	}

	virtual	~GPUProgram()
	{
	}

	virtual void Destroy()
	{
		mVertexShader = NULL;
		mPixelShader = NULL;
	}


	Shader*			GetVertexShader()		{	return mVertexShader;	}
	Shader*			GetPixelShader()		{	return mPixelShader;	}

	// attach shaders to the GPU program
	virtual	void	AttachShader(Shader* sh);

	virtual	void	DetachVertexShader();
	virtual void	DetachPixelShader();
	virtual	void	DetachBothShaders();

	// call this function after shaders are attached to the GPUProgram
	virtual void	Bind()
	{
	}

	// set or unset the shaders for using
	virtual	void	SetGPUProgram();
	virtual	void	UnsetGPUProgram();

	virtual void	Update(Real delta_t);
};

END_ENGINE_NAMESPACE

#endif