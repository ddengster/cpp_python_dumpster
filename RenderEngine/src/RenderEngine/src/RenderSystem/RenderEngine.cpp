
#include "RenderSystem/RenderEngine.h"

#include "RenderSystem/RenderingComponents/OGLRenderer.h"
#include "RenderSystem/RenderingComponents/DX9Renderer.h"

#include "RenderSystem/RenderingComponents/OGLHardwareBufferManager.h"
#include "RenderSystem/RenderingComponents/DX9HardwareBufferManager.h"

#include "RenderSystem/RenderingComponents/OGLTextureManager.h"
#include "RenderSystem/RenderingComponents/DX9TextureManager.h"

#include "RenderSystem/RenderingComponents/OGLShaderManager.h"
#include "RenderSystem/RenderingComponents/DX9ShaderManager.h"

#include "RenderSystem/SceneManager/SceneManager.h"

DECLARE_ENGINE_NAMESPACE

HardwareBufferManager* RenderEngine::mHwbManager = 0;
TextureManager* RenderEngine::mTextureManager = 0;
ShaderManager* RenderEngine::mShaderManager = 0;

RenderEngine::RenderEngine()
:mRenderer(0), mSceneManager(0)
{
}

RenderEngine::~RenderEngine()
{
	Shutdown();
}

RenderEngine* RenderEngine::GetInstance()
{
	static RenderEngine re;
	return &re;
}

void RenderEngine::Shutdown()
{
	SAFE_DELETE(mSceneManager);
	SAFE_DELETE(mShaderManager);
	SAFE_DELETE(mTextureManager);
	SAFE_DELETE(mHwbManager);
	SAFE_DELETE(mRenderer);
}

IRenderer* RenderEngine::CreateRenderSystem(RenderSetup setup)
{
	mSetup = setup;
	if (mSetup.renderer_type == OPENGL_RENDERING_SYSTEM)
	{
		mRenderer = new OGLRenderer();
		mRenderer->Init();
		mHwbManager = new OGLHardwareBufferManager();
		mTextureManager = new OGLTextureManager();
		mShaderManager = new OGLShaderManager();
	}
	else if (mSetup.renderer_type == DIRECTX9_RENDERING_SYSTEM)
	{
		mRenderer = new DX9Renderer();
		mRenderer->Init();
		mHwbManager = new DX9HardwareBufferManager();
		mTextureManager = new DX9TextureManager();
		mShaderManager = new DX9ShaderManager();
	}
	mSceneManager = new SceneManager();

	return mRenderer;
}



END_ENGINE_NAMESPACE
