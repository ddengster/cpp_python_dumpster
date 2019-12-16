
#ifndef _TEXTUREMANAGER_H
#define _TEXTUREMANAGER_H

#include "Prerequisites.h"
#include "DataManagement/ResourceManager.h"
#include "RenderSystem/RendererDefines.h"
#include <windows.h>

#include "RenderSystem/RenderingComponents/Texture.h"

DECLARE_ENGINE_NAMESPACE

class RenderEngine;

class TextureManager : public ResourceManager<Texture>
{
protected:
	uint32_t	mMipmapLevel;
	BYTE*		mCubeImages[6];
	BYTE*		m2DImage;

public:
	static TextureManager* GetInstance();

	virtual ~TextureManager();

	void		SetMipmapLevel(uint32_t level)		{	mMipmapLevel = level;	}
	uint32_t	GetMipmapLevel()					{	return mMipmapLevel;	}

	// return NULL, when texture is not found or failed to load
	virtual Texture*	CreateTexture(	const String& name, TEXTURE_TYPE type,
										uint32_t width, uint32_t height,
										PIXEL_FORMAT format, void* data) = 0;

	virtual Texture*	GetTexture1D(const String& name, const String& dir) = 0;
	virtual Texture*	GetTexture2D(const String& name, const String& dir) = 0;
	virtual Texture*	GetTextureCUBE(const String& name, const String& dir) = 0;

	bool	Load2DImageFromFile( const String& full_name,
								 uint32_t &width, uint32_t &height,
								 PIXEL_FORMAT &pix_fmt);

	void				Release2DImage();

	// use these two function to load images from files
	// remember to call ReleaseCubeImages when image data is no longer needed
	bool		LoadCubeImagesFromFile(	const String& name, 
										const String& dir, 
										uint32_t &width, uint32_t& height, 
									PIXEL_FORMAT &pix_fmt);
	void				ReleaseCubeImages();

	/** Wrapper for DestroyAllResources */
	virtual void	DestroyAllTextures() = 0;
	/** Wrapper for RemoveResource */
	virtual bool	RemoveTexture(const String &name) = 0;
	virtual bool	RemoveTexture(Texture* res) = 0;
	/** Wrapper for DestroyResource */
	virtual bool	DestroyTexture(const String &name) = 0;
	virtual bool	DestroyTexture(Texture* res) = 0;

protected:
	friend class RenderEngine;
	TextureManager();
};

END_ENGINE_NAMESPACE

#endif