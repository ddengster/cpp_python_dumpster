
#include "RenderSystem/RenderingComponents/OGLShaderManager.h"
#include "RenderSystem/RenderingComponents/OGLShader.h"
#include <fstream>

DECLARE_ENGINE_NAMESPACE

OGLShaderManager::OGLShaderManager()
:ShaderManager()
{
	mShaderMemPool = new FreeListPool<OGLShader>(TABLE_SMALL_SIZE);
	mGPUProgMemPool = new FreeListPool<OGLGPUProgram>(TABLE_SMALL_SIZE);
}

OGLShaderManager::~OGLShaderManager()
{
	DestroyAllShaders();
	DestroyAllGPUPrograms();
	SAFE_DELETE(mShaderMemPool);
	SAFE_DELETE(mGPUProgMemPool);
}

GPUProgram*	OGLShaderManager::CreateGPUProgram()
{
	OGLGPUProgram*	prog = mGPUProgMemPool->NewInstance();
	prog->Initialize();

	mGPUPrograms.push_back((GPUProgram*)prog);
	return prog;
}

bool OGLShaderManager::DestroyGPUProgram(GPUProgram *prog)
{
	std::vector<GPUProgram*>::iterator iter = std::find(mGPUPrograms.begin(), mGPUPrograms.end(), prog);

	if (iter == mGPUPrograms.end())
		return false;

	if (mCurrentGPUProgram == prog)
		mCurrentGPUProgram = NULL;

	mGPUPrograms.erase(iter);
	
	OGLGPUProgram *oglprog = (OGLGPUProgram*)prog;
	oglprog->Destroy();
	mGPUProgMemPool->FreeInstance(oglprog);
	return true;
}

void OGLShaderManager::DestroyAllGPUPrograms()
{
	for (std::vector<GPUProgram*>::iterator iter = mGPUPrograms.begin(); iter != mGPUPrograms.end(); ++iter)
	{
		OGLGPUProgram *oglprog = (OGLGPUProgram*)(*iter);
		oglprog->Destroy();
		mGPUProgMemPool->FreeInstance(oglprog);
	}
	mGPUPrograms.clear();
	mCurrentGPUProgram = NULL;
}


Shader*	OGLShaderManager::GetShader(const String &name, const String &path, SHADER_TYPE type,
									SHADER_VERSION version, const String &entry_name)
{
	Shader*	sh = GetResource(name);

	if (sh != NULL)
		return sh;

	String full_name = path + name;

	String sh_data;

	std::ifstream sh_file(full_name.c_str());

	//read the file's contents
	String one_line;
	while (!sh_file.eof())
	{
		getline(sh_file, one_line);

		sh_data += one_line;
		sh_data += '\n';
	}

	sh_file.close();

	OGLShader* newshader = mShaderMemPool->NewInstance();
	newshader->Initialize(name, path, type, version, "", sh_data);

	AddResource(newshader);

	return newshader;
}

bool OGLShaderManager::RemoveShader(const String &name)
{
	Shader* toDestroy = 0;

	if (RemoveResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((OGLShader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool OGLShaderManager::RemoveShader(Shader* res)
{
	Shader* toDestroy = 0;

	if (RemoveResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((OGLShader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool OGLShaderManager::DestroyShader(const String &name)
{
	Shader* toDestroy = 0;

	if (DestroyResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((OGLShader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool OGLShaderManager::DestroyShader(Shader* res)
{
	Shader* toDestroy = 0;

	if (DestroyResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mShaderMemPool->FreeInstance((OGLShader*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

void OGLShaderManager::DestroyAllShaders()
{
	for (uint32_t i=0; i<(uint32_t)m_ResourceList.size(); ++i)
	{
		DestroyShader(m_ResourceList[i]);
	}
	m_ResourceList.clear();
}

END_ENGINE_NAMESPACE
