
#ifndef _RENDERENGINE_H
#define _RENDERENGINE_H

#include "Prerequisites.h"

#include <windows.h>

DECLARE_ENGINE_NAMESPACE

class IRenderer;
class HardwareBufferManager;
class TextureManager;
class ShaderManager;
class SceneManager;

/** Enum of the types of Renderers available */
enum RENDERSYSTEM
{
	NULL_RENDERING_SYSTEM=0, /*!< Invalid rendering system */
	OPENGL_RENDERING_SYSTEM, /*!< OpenGL Renderer */
	DIRECTX9_RENDERING_SYSTEM /*!< DirectX9 Renderer */
};

struct RenderSetup
{
	RENDERSYSTEM	renderer_type;
	uint32_t		width;
	uint32_t		height;
	bool			full_screen;
	HWND			window_handle;
};

class RenderEngine
{
public:
	virtual ~RenderEngine();

	static RenderEngine* GetInstance();

	void Shutdown();

	/** Create the Renderer */
	IRenderer* CreateRenderSystem(RenderSetup setup);

	/** Get the current Renderer Setup */
	RenderSetup& GetRenderSetup() { return mSetup; }
	/** Get the current Renderer */
	//TODO: Give all of them a GetInstance() function so we don't have to include RenderEngine and the associated header?
	IRenderer* GetRenderer() { return mRenderer; }

	//Todo: Allow Multiple SceneManagers?
	SceneManager* GetSceneManager() { return mSceneManager; }

protected:
	friend class HardwareBufferManager;
	static HardwareBufferManager* GetHardwareBufferManager() { return mHwbManager; }

protected:
	friend class TextureManager;
	static TextureManager* GetTextureManager() { return mTextureManager; }

protected:
	friend class ShaderManager;
	static ShaderManager* GetShaderManager() { return mShaderManager; }

private:
	IRenderer* mRenderer;
	SceneManager* mSceneManager;

	static HardwareBufferManager* mHwbManager;
	static TextureManager* mTextureManager;
	static ShaderManager* mShaderManager;
	RenderSetup mSetup;

	RenderEngine();
};

END_ENGINE_NAMESPACE

#endif
