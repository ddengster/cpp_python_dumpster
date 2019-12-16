
#ifndef _TEXTURE_H
#define _TEXTURE_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include "DataManagement/Resource.h"

#define NUM_CUBE_FACES 6

DECLARE_ENGINE_NAMESPACE

class Texture : public Resource
{
protected:
	TEXTURE_TYPE	mType;
	uint32_t		mWidth;
	uint32_t		mHeight;
	uint32_t		mMipmapLevel;
	PIXEL_FORMAT	mFormat;
	uint32_t		mSamplerID;

public:
	Texture();

	virtual void Initialize(const String& name, const String &path, TEXTURE_TYPE type, 
							uint32_t width, uint32_t height, uint32_t mip_level, PIXEL_FORMAT format);

	virtual ~Texture();

	virtual void Destroy();


	TEXTURE_TYPE	getType()			{	return mType;	}
	uint32_t		getWidth()			{	return mWidth;	}
	uint32_t		getHeight()			{	return mHeight;	}
	uint32_t		getMipmapLevel()	{	return mMipmapLevel;	}
	PIXEL_FORMAT	getFormat()			{	return mFormat;	}

	// The following two functions should be called after SetTexture, otherwise their behavior is not certain
	virtual	void SetTextureParameter(TEXTURE_PARAMETER param, uint32_t value) = 0;
	virtual void SetTextureBlend(TEXTURE_BLEND_OP op) = 0;

	virtual	void SetTexture(uint32_t sampler_id) = 0;
	virtual	void UnsetTexture() = 0;

	virtual void Update2DTexture(uint32_t x_offset, uint32_t y_offset, uint32_t width, uint32_t height,
								 PIXEL_FORMAT format, void* data) = 0;
};

END_ENGINE_NAMESPACE

#endif
