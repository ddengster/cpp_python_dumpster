
#include "RenderSystem/RenderingComponents/DX9ShaderManager.h"
#include "RenderSystem/RenderingComponents/DX9Shader.h"
#include "RenderSystem/RenderingComponents/DX9Renderer.h"
#include "RenderSystem/RenderEngine.h"
#include <fstream>

DECLARE_ENGINE_NAMESPACE

DX9ShaderManager::DX9ShaderManager()
:ShaderManager()
{
	mpD3DDevice = ((DX9Renderer*)RenderEngine::GetInstance()->GetRenderer())->GetD3DDevice();
	mShaderMemPool = new FreeListPool<DX9Shader>(TABLE_SMALL_SIZE);
	mGPUProgMemPool = new FreeListPool<DX9GPUProgram>(TABLE_SMALL_SIZE);
}

DX9ShaderManager::~DX9ShaderManager()
{
	DestroyAllShaders();
	DestroyAllGPUPrograms();
	SAFE_DELETE(mShaderMemPool);
	SAFE_DELETE(mGPUProgMemPool);
}

GPUProgram*	DX9ShaderManager::CreateGPUProgram()
{
	DX9GPUProgram*	prog = mGPUProgMemPool->NewInstance();
	prog->Initialize();

	mGPUPrograms.push_back((GPUProgram*)prog);
	return prog;
}

bool DX9ShaderManager::DestroyGPUProgram(GPUProgram *prog)
{
	std::vector<GPUProgram*>::iterator iter = std::find(mGPUPrograms.begin(), mGPUPrograms.end(), prog);

	if (iter == mGPUPrograms.end())
		return false;

	if (mCurrentGPUProgram == prog)
		mCurrentGPUProgram = NULL;

	mGPUPrograms.erase(iter);
	
	DX9GPUProgram *dxprog = (DX9GPUProgram*)prog;
	dxprog->Destroy();
	mGPUProgMemPool->FreeInstance(dxprog);
	return true;
}

void DX9ShaderManager::DestroyAllGPUPrograms()
{
	for (std::vector<GPUProgram*>::iterator iter = mGPUPrograms.begin(); iter != mGPUPrograms.end(); ++iter)
	{
		DX9GPUProgram *dxprog = (DX9GPUProgram*)(*iter);
		dxprog->Destroy();
		mGPUProgMemPool->FreeInstance(dxprog);
	}
	mGPUPrograms.clear();
	mCurrentGPUProgram = NULL;
}

Shader*	DX9ShaderManager::GetShader(const String &name, const String &path, SHADER_TYPE type,
									SHADER_VERSION version, const String &entry_name)
{
	Shader*	sh = GetResource(name);

	if (sh != NULL)
		return sh;

	String full_name = path + name;

	String sh_data;

	std::ifstream sh_file(full_name.c_str());

	String one_line;
	while (!sh_file.eof())
	{
		getline(sh_file, one_line);

		sh_data += one_line;
		sh_data += '\n';
	}

	sh_file.close();

	//sh = new DX9Shader(name, path, type, version, entry_name, sh_data, mpD3DDevice);
	DX9Shader* newshader = mShaderMemPool->NewInstance();
	newshader->Initialize(name, path, type, version, entry_name, sh_data, mpD3DDevice);

	AddResource(newshader);

	return newshader;
}

bool DX9ShaderManager::RemoveShader(const String &name)
{
	Shader* toDestroy = 0;

	if (RemoveResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((DX9Shader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool DX9ShaderManager::RemoveShader(Shader* res)
{
	Shader* toDestroy = 0;

	if (RemoveResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((DX9Shader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool DX9ShaderManager::DestroyShader(const String &name)
{
	Shader* toDestroy = 0;

	if (DestroyResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((DX9Shader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool DX9ShaderManager::DestroyShader(Shader* res)
{
	Shader* toDestroy = 0;

	if (DestroyResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((DX9Shader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

void DX9ShaderManager::DestroyAllShaders()
{
	for (uint32_t i=0; i<(uint32_t)m_ResourceList.size(); ++i)
	{
		DestroyShader(m_ResourceList[i]);
	}
	m_ResourceList.clear();
}

END_ENGINE_NAMESPACE
