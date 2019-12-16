
#ifndef _OGLSHADERMANAGER_H
#define _OGLSHADERMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/ShaderManager.h"
#include "RenderSystem/GLHeaders.h"
#include "DataManagement/FreeListPool.h"

DECLARE_ENGINE_NAMESPACE

class Shader;
class GPUProgram;
class OGLShader;
class OGLGPUProgram;
class RenderEngine;

class OGLShaderManager : public ShaderManager
{
private:
	
	FreeListPool<OGLShader> *mShaderMemPool;
	FreeListPool<OGLGPUProgram> *mGPUProgMemPool;

public:
	virtual ~OGLShaderManager();

	GPUProgram*		CreateGPUProgram();
	bool			DestroyGPUProgram(GPUProgram *prog);
	void			DestroyAllGPUPrograms();

	Shader*	GetShader(	const String& name, const String& path, SHADER_TYPE type,
						SHADER_VERSION version, const String& entry_name = "");
	
	virtual bool	RemoveShader(const String &name);
	virtual bool	RemoveShader(Shader* res);

	virtual bool	DestroyShader(const String &name);
	virtual bool	DestroyShader(Shader* res);

	virtual void	DestroyAllShaders();
protected:
	friend class RenderEngine;
	OGLShaderManager();
};

END_ENGINE_NAMESPACE

#endif
