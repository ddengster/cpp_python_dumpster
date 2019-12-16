
#ifndef _OGLTEXTUREMANAGER_H
#define _OGLTEXTUREMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/TextureManager.h"
#include "RenderSystem/GLHeaders.h"
#include "DataManagement/FreeListPool.h"

DECLARE_ENGINE_NAMESPACE

class OGLTexture;
class RenderEngine;

class OGLTextureManager : public TextureManager
{	
public:
	virtual ~OGLTextureManager();

	Texture*	CreateTexture(	const String& name, TEXTURE_TYPE type,
								uint32_t width, uint32_t height,
								PIXEL_FORMAT format, void* data);

	Texture*	GetTexture1D(const String& name, const String& dir);
	Texture*	GetTexture2D(const String& name, const String& dir);
	Texture*	GetTextureCUBE(const String& name, const String& dir);

	virtual bool	RemoveTexture(const String &name);
	virtual bool	RemoveTexture(Texture* res);

	virtual bool	DestroyTexture(const String &name);
	virtual bool	DestroyTexture(Texture* res);

	virtual void	DestroyAllTextures();

	FreeListPool<OGLTexture>* GetMemPool();
	
protected:
	friend class RenderEngine;
	OGLTextureManager();
protected:
	FreeListPool<OGLTexture> *mMemPool;
};


END_ENGINE_NAMESPACE

#endif