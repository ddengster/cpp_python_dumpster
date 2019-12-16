
#include "RenderSystem/RenderingComponents/Texture.h"

DECLARE_ENGINE_NAMESPACE

Texture::Texture()
:mWidth(0), mHeight(0), mMipmapLevel(0), mSamplerID(0)
{
}

void Texture::Initialize(const String& name, const String &path, TEXTURE_TYPE type,
						uint32_t width, uint32_t height, uint32_t mip_level, PIXEL_FORMAT format) 
{
	mSamplerID = 0;
	Resource::Initialize(name, path);

	mType = type;
	mWidth = width;
	mHeight = height;
	mMipmapLevel = mip_level;
	mFormat = format;
}

Texture::~Texture()
{
	Destroy();
}

void Texture::Destroy()
{
	Resource::Destroy();
}

END_ENGINE_NAMESPACE
