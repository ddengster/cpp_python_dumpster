
#include "RenderSystem/RenderingComponents/OGLTextureManager.h"
#include "RenderSystem/RenderingComponents/OGLTexture.h"
#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

OGLTextureManager::OGLTextureManager()
{
	mMemPool = new FreeListPool<OGLTexture>(TABLE_SMALL_SIZE);
}

OGLTextureManager::~OGLTextureManager()
{
	DestroyAllTextures();
	SAFE_DELETE(mMemPool);
}

Texture* OGLTextureManager::CreateTexture(	const String& name, TEXTURE_TYPE type,
											uint32_t width, uint32_t height,
											PIXEL_FORMAT format, void* data)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	OGLTexture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, "", type, 
							width, height, mMipmapLevel,
							format, data);

	AddResource(newtexture);
	return newtexture;
}

Texture* OGLTextureManager::GetTexture1D(const String& name, const String& dir)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	String full_name = dir + name;

	uint32_t			width;
	uint32_t			height;
	PIXEL_FORMAT	pix_fmt;

	bool is_loaded = Load2DImageFromFile(full_name, width, height, pix_fmt);

	if (!is_loaded)
		return NULL;

	if (height != 1)
	{
		LogManager::GetInstance()->AppendToLog("Error: " + full_name + " is not 1D texture.");
		Release2DImage();
		return NULL;
	}

	OGLTexture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, dir, TEXTURE_1D, 
							width, height, mMipmapLevel,
							pix_fmt, (void*)m2DImage);
	AddResource(newtexture);

	Release2DImage();

	return newtexture;
}

Texture* OGLTextureManager::GetTexture2D(const String& name, const String& dir)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	String full_name = dir + name;

	uint32_t		width;
	uint32_t		height;
	PIXEL_FORMAT	pix_fmt;

	bool is_loaded = Load2DImageFromFile(full_name, width, height, pix_fmt);

	if (!is_loaded)
		return NULL;

	OGLTexture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, dir, TEXTURE_2D,
							width, height, mMipmapLevel,
							pix_fmt, (void*)m2DImage);

	AddResource(newtexture);

	Release2DImage();

	return newtexture;
}

Texture* OGLTextureManager::GetTextureCUBE(const String& name, const String& dir)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	uint32_t		width;
	uint32_t		height;
	PIXEL_FORMAT	pix_fmt;

	bool is_loaded = LoadCubeImagesFromFile(name, dir, width, height, pix_fmt);

	if (!is_loaded)
		return NULL;

	OGLTexture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(name, dir, TEXTURE_CUBE,
						width, height, mMipmapLevel,
						pix_fmt, (void**)mCubeImages);
	AddResource(newtexture);

	ReleaseCubeImages();

	return newtexture;
}

bool OGLTextureManager::RemoveTexture(const String &name)
{
	Texture* toDestroy = 0;

	if (RemoveResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((OGLTexture*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool OGLTextureManager::RemoveTexture(Texture* res)
{
	Texture* toDestroy = 0;

	if (RemoveResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((OGLTexture*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool OGLTextureManager::DestroyTexture(const String &name)
{
	Texture* toDestroy = 0;

	if (DestroyResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((OGLTexture*)toDestroy);
		}
		return true;
	}
	else
		return false;
	
}

bool OGLTextureManager::DestroyTexture(Texture* res)
{
	Texture* toDestroy = 0;

	if (DestroyResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((OGLTexture*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

void OGLTextureManager::DestroyAllTextures()
{
	for (uint32_t i=0; i<(uint32_t)m_ResourceList.size(); ++i)
	{
		DestroyTexture(m_ResourceList[i]);
	}
	m_ResourceList.clear();
}

END_ENGINE_NAMESPACE
