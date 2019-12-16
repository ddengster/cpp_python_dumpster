
#include "RenderSystem/RenderingComponents/DX9TextureManager.h"
#include "RenderSystem/RenderingComponents/DX9Texture.h"
#include "RenderSystem/RenderEngine.h"
#include "RenderSystem/RenderingComponents/DX9Renderer.h"
#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

DX9TextureManager::DX9TextureManager()
{
	mpD3DDevice = ((DX9Renderer*)RenderEngine::GetInstance()->GetRenderer())->GetD3DDevice();

	mMemPool = new FreeListPool<DX9Texture>(TABLE_SMALL_SIZE);
}

DX9TextureManager::~DX9TextureManager()
{
	SAFE_DELETE(mMemPool);
}

Texture* DX9TextureManager::CreateTexture(	const String& name, TEXTURE_TYPE type,
											uint32_t width, uint32_t height,
											PIXEL_FORMAT format, void* data)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	DX9Texture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, "", type,
							width, height, mMipmapLevel,
							format, data,
							mpD3DDevice);
	AddResource(newtexture);

	return newtexture;
}

Texture* DX9TextureManager::GetTexture1D(const String& name, const String& dir)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	String	full_name = dir + name;

	uint32_t		width;
	uint32_t		height;
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

	DX9Texture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(name, "", TEXTURE_1D,
						width, height, mMipmapLevel,
						pix_fmt, (void*)m2DImage,
						mpD3DDevice);
	AddResource(newtexture);

	Release2DImage();

	return newtexture;
}

Texture* DX9TextureManager::GetTexture2D(const String& name, const String& dir)
{
	Texture* texture = GetResource(name);
	if (texture != NULL)
		return texture;

	String	full_name = dir + name;

	uint32_t		width;
	uint32_t		height;
	PIXEL_FORMAT	pix_fmt;

	bool is_loaded = Load2DImageFromFile(full_name, width, height, pix_fmt);

	if (!is_loaded)
		return NULL;

	DX9Texture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, "", TEXTURE_2D,
							width, height, mMipmapLevel,
							pix_fmt, (void*)m2DImage,
							mpD3DDevice);

	AddResource(newtexture);

	Release2DImage();

	return newtexture;
}

Texture* DX9TextureManager::GetTextureCUBE(const String& name, const String& dir)
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

	DX9Texture *newtexture = mMemPool->NewInstance();
	newtexture->Initialize(	name, dir, TEXTURE_CUBE,
							width, height, mMipmapLevel,
							pix_fmt, (void**)mCubeImages,
							mpD3DDevice);
	ReleaseCubeImages();

	AddResource(newtexture);
	return newtexture;
}


bool DX9TextureManager::RemoveTexture(const String &name)
{
	Texture* toDestroy = 0;

	if (RemoveResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((DX9Texture*)toDestroy);
		}
		return true;
	}
	else
		return false;	
}

bool DX9TextureManager::RemoveTexture(Texture* res)
{
	Texture* toDestroy = 0;

	if (RemoveResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((DX9Texture*)toDestroy);
		}
		return true;
	}
	else
		return false;

}

bool DX9TextureManager::DestroyTexture(const String &name)
{
	Texture* toDestroy = 0;

	if (DestroyResource(name, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((DX9Texture*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

bool DX9TextureManager::DestroyTexture(Texture* res)
{
	Texture* toDestroy = 0;

	if (DestroyResource(res, toDestroy))
	{
		if (toDestroy)
		{
			toDestroy->Destroy();
			mMemPool->FreeInstance((DX9Texture*)toDestroy);
		}
		return true;
	}
	else
		return false;
}

void DX9TextureManager::DestroyAllTextures()
{
	for (uint32_t i=0; i<(uint32_t)m_ResourceList.size(); ++i)
	{
		DestroyTexture(m_ResourceList[i]);
	}
	m_ResourceList.clear();
}

END_ENGINE_NAMESPACE
