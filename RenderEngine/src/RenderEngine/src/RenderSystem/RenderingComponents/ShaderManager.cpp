
#include "RenderSystem/RenderingComponents/ShaderManager.h"
#include "RenderSystem/RenderEngine.h"

DECLARE_ENGINE_NAMESPACE

ShaderManager* ShaderManager::GetInstance()
{
	return RenderEngine::GetShaderManager();
}

ShaderManager::ShaderManager()
: ResourceManager(TABLE_SMALL_SIZE)
{
	mGPUPrograms.clear();
	mCurrentGPUProgram = NULL;
}

ShaderManager::~ShaderManager()
{
}

END_ENGINE_NAMESPACE
