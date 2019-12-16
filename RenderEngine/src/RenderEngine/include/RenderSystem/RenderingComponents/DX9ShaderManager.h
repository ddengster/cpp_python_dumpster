
#ifndef _DX9SHADERMANAGER_H
#define _DX9SHADERMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/ShaderManager.h"
#include "DataManagement/FreeListPool.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class Shader;
class GPUProgram;
class DX9Shader;
class DX9GPUProgram;
class RenderEngine;

class DX9ShaderManager : public ShaderManager
{
private:
	LPDIRECT3DDEVICE9		mpD3DDevice;

	FreeListPool<DX9Shader> *mShaderMemPool;
	FreeListPool<DX9GPUProgram> *mGPUProgMemPool;

public:
	virtual ~DX9ShaderManager();

	GPUProgram*	CreateGPUProgram();
	bool			DestroyGPUProgram(GPUProgram *prog);
	void			DestroyAllGPUPrograms();

	Shader*	GetShader(	const String& name, const String& path, SHADER_TYPE type,
						SHADER_VERSION version, const String& entry_name);
	
	virtual bool	RemoveShader(const String &name);
	virtual bool	RemoveShader(Shader* res);

	virtual bool	DestroyShader(const String &name);
	virtual bool	DestroyShader(Shader* res);

	virtual void	DestroyAllShaders();
protected:
	friend class RenderEngine;
	DX9ShaderManager();
};

END_ENGINE_NAMESPACE

#endif