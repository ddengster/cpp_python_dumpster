
#ifndef _DX9TEXTUREMANAGER_H
#define _DX9TEXTUREMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/TextureManager.h"
#include <d3d9.h>
#include <d3dx9.h>
#include "DataManagement/FreeListPool.h"

DECLARE_ENGINE_NAMESPACE

class DX9Texture;
class RenderEngine;

class DX9TextureManager : public TextureManager
{
private:
	LPDIRECT3DDEVICE9	mpD3DDevice;
	
public:
	virtual ~DX9TextureManager();

	Texture*	CreateTexture(	const String& name, TEXTURE_TYPE type,
								uint32_t width, uint32_t height,
								PIXEL_FORMAT format, void* data);

	Texture*	GetTexture1D(const String& name, const String& dir);
	Texture*	GetTexture2D(const String& name, const String& dir);
	Texture*	GetTextureCUBE(const String& name, const String& dir);

	virtual bool	RemoveTexture(const String &name);
	virtual bool	RemoveTexture(Texture* res);

	virtual bool	DestroyTexture(Texture* res);
	virtual bool	DestroyTexture(const String &name);

	virtual void	DestroyAllTextures();
	
	FreeListPool<DX9Texture>* GetMemPool();
protected:
	friend class RenderEngine;
	DX9TextureManager();
protected:
	FreeListPool<DX9Texture> *mMemPool;
};

END_ENGINE_NAMESPACE

#endif
