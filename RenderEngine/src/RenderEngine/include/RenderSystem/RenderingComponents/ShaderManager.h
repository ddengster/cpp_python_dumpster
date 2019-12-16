
#ifndef _SHADERMANAGER_H
#define _SHADERMANAGER_H

#include "Prerequisites.h"
#include "DataManagement/ResourceManager.h"
#include "RenderSystem/RendererDefines.h"
#include "RenderSystem/RenderingComponents/Shader.h"
#include <vector>

DECLARE_ENGINE_NAMESPACE

// always use ShaderManager functions to get, create, destroy shaders and gpuprograms
// otherwise users will be reposible for memory management
class ShaderManager : public ResourceManager<Shader>
{
protected:
	std::vector<GPUProgram*>	mGPUPrograms;
	GPUProgram*					mCurrentGPUProgram;

public:
	static ShaderManager* GetInstance();
	virtual ~ShaderManager();

	// this function will automatically load the shader if the shader is not loaded yet
	// it requires full path, shader type, shader version, and entry function name
	// You can always use GetResource() to get the shader by name only,
	// but GetResource will not load the shader if it is not loaded, instead NULL will be returned
	// In the case of Opengl shaders, you can omit out the entry_name param
	virtual	Shader*	GetShader(	const String& name, const String& path, SHADER_TYPE type,
								SHADER_VERSION version, const String& entry_name = "") = 0;
	
	// Create a GPUProgram
	virtual GPUProgram*		CreateGPUProgram() = 0;
	
	virtual bool		DestroyGPUProgram(GPUProgram *prog) = 0;
	virtual void		DestroyAllGPUPrograms() = 0;

	// called by GPUProgram to register itself, users shouldn't call this function
	void _RegisterCurrentGPUProgram(GPUProgram *prog)
	{
		mCurrentGPUProgram = prog;
	}

	virtual bool	DestroyShader(const String &name) = 0;
	virtual bool	DestroyShader(Shader* res) = 0;
	virtual void	DestroyAllShaders() = 0;
	virtual bool	RemoveShader(const String &name) = 0;
	virtual bool	RemoveShader(Shader* res) = 0;

protected:
	friend class RenderEngine;
	ShaderManager();
};

END_ENGINE_NAMESPACE

#endif