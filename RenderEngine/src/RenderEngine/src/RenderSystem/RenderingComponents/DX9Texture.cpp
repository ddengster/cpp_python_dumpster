
#include "RenderSystem/RenderingComponents/DX9Texture.h"
#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

D3DFORMAT DX9Texture::_ConvertToD3DPixelFormat(PIXEL_FORMAT format)
{
	static D3DFORMAT table[NUM_PIXEL_FORMAT] = {D3DFMT_R8G8B8, D3DFMT_A8R8G8B8};
	return table[format];
}

D3DSAMPLERSTATETYPE	DX9Texture::_ConvertToD3DTextureParameter(TEXTURE_PARAMETER param)
{
	static D3DSAMPLERSTATETYPE table[NUM_TEXTURE_PARAMETER] = {	D3DSAMP_MINFILTER,
																D3DSAMP_MAGFILTER,
																D3DSAMP_ADDRESSU,
																D3DSAMP_ADDRESSV,
																D3DSAMP_ADDRESSW };
	return	table[param];
}

D3DTEXTUREFILTERTYPE DX9Texture::_ConvertToD3DFilterType(uint32_t filter)
{
	static D3DTEXTUREFILTERTYPE table[NUM_TEXTURE_FILTER] = {	D3DTEXF_POINT,
																D3DTEXF_LINEAR,
																D3DTEXF_POINT,
																D3DTEXF_POINT,
																D3DTEXF_LINEAR,
																D3DTEXF_LINEAR };
	return table[filter];
}

D3DTEXTUREADDRESS DX9Texture::_ConvertToD3DAddress(uint32_t address)
{
	static D3DTEXTUREADDRESS table[NUM_TEXTURE_ADDRESS] = {	D3DTADDRESS_WRAP,
															D3DTADDRESS_MIRROR,
															D3DTADDRESS_CLAMP };
	return table[address];
}

DWORD DX9Texture::_ConvertToD3DTA(TEXTURE_BLEND_ARG arg)
{
	static DWORD	table[NUM_TEXTURE_BLEND_ARG] = {D3DTA_CURRENT,
													D3DTA_TEXTURE,
													D3DTA_DIFFUSE };
	return table[arg];
}

D3DTEXTUREOP DX9Texture::_ConvertToD3DTXTUREOP(TEXTURE_BLEND_OP op)
{
	static D3DTEXTUREOP	table[NUM_TEXTURE_BLEND_OP] = {	D3DTOP_SELECTARG1,
														D3DTOP_MODULATE,
														D3DTOP_ADD,
														D3DTOP_BLENDTEXTUREALPHA };
	return table[op];
}

DX9Texture::DX9Texture()
:Texture(), mD3DTexture(NULL), mD3DCubeTexture(NULL), mpD3DDevice(NULL)
{
}

void DX9Texture::Initialize(const String& name, const String& path, TEXTURE_TYPE type,
						   uint32_t width, uint32_t height, uint32_t mip_level,
						   PIXEL_FORMAT format, void* data, LPDIRECT3DDEVICE9 dev)
{
	Texture::Initialize(name, path, type, width, height, mip_level, format);
	mpD3DDevice = dev;

	dev->SetRenderState(D3DRS_WRAP0, 0);
	mD3DCubeTexture = NULL;

	if (!mip_level) // no mipmaps
		D3DXCreateTexture(	mpD3DDevice, mWidth, mHeight, 1, 0,
						_ConvertToD3DPixelFormat(mFormat), D3DPOOL_MANAGED, &mD3DTexture);
	else
		D3DXCreateTexture(	mpD3DDevice, mWidth, mHeight, mMipmapLevel, D3DUSAGE_AUTOGENMIPMAP,
							_ConvertToD3DPixelFormat(mFormat), D3DPOOL_MANAGED, &mD3DTexture);
	if (data)
	{
		D3DLOCKED_RECT	rect;
		mD3DTexture->LockRect(0, &rect, NULL, 0);

		uint32_t pix_size = 0;

		uint32_t	temp;
		uint8_t		r, g, b, a;
		switch(mFormat)
		{
		case PIX_FMT_R8G8B8:
			{
				pix_size = 3;

				// even the format is R8G8B8, DirectX will return the texture with format X8R8G8B8
				for (uint32_t i = 0; i < mHeight; ++i)
				{
					for (uint32_t j = 0; j < mWidth; ++j)
					{
						
						r = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size];
						g = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size + 1];
						b = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size + 2];
						temp = 0xff000000 |(r << 16) | (g << 8) | b;
						memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
					}
				}
				break;
			}
		case PIX_FMT_A8R8G8B8:
			{
				pix_size = 4;

				for (uint32_t i = 0; i < mHeight; ++i)
				{
					for (uint32_t j = 0; j < mWidth; ++j)
					{
						r = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size];
						g = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size + 1];
						b = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size + 2];
						a = ((BYTE*)data)[i*mWidth*pix_size + j*pix_size + 3];
						temp = (a << 24) |(r << 16) | (g << 8) | b;
						memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
					}
				}
				break;
			}
		}

		mD3DTexture->UnlockRect(0);
	}
}

void DX9Texture::Initialize(const String& name, const String& path, TEXTURE_TYPE type,
						   uint32_t width, uint32_t height, uint32_t mip_level, 
						   PIXEL_FORMAT format, void** data6, LPDIRECT3DDEVICE9 dev)
	
{
	Texture::Initialize(name, path, type, width, height, mip_level, format);
	mpD3DDevice = dev;

	mD3DTexture = NULL;
	
	if (!mMipmapLevel) // no mipmaps
		D3DXCreateCubeTexture(	mpD3DDevice, width, 1, 0,
								_ConvertToD3DPixelFormat(mFormat), D3DPOOL_MANAGED, &mD3DCubeTexture);
	else	
		D3DXCreateCubeTexture(	mpD3DDevice, width, mMipmapLevel, D3DUSAGE_AUTOGENMIPMAP,
								_ConvertToD3DPixelFormat(mFormat), D3DPOOL_MANAGED, &mD3DCubeTexture);
	if (data6)
	{
		D3DCUBEMAP_FACES cube_faces[NUM_CUBE_FACES] = {	D3DCUBEMAP_FACE_POSITIVE_X,
														D3DCUBEMAP_FACE_NEGATIVE_X,
														D3DCUBEMAP_FACE_POSITIVE_Y,
														D3DCUBEMAP_FACE_NEGATIVE_Y,
														D3DCUBEMAP_FACE_POSITIVE_Z,
														D3DCUBEMAP_FACE_NEGATIVE_Z
													};

		uint32_t	temp;
		uint8_t		r, g, b, a;
		for (uint32_t face_i=0; face_i < NUM_CUBE_FACES; ++face_i)
		{
			D3DLOCKED_RECT	rect;
			mD3DCubeTexture->LockRect(cube_faces[face_i], 0, &rect, NULL, 0);

			uint32_t pix_size = 0;

			switch(mFormat)
			{
			case PIX_FMT_R8G8B8:
				{
					pix_size = 3;

					// even the format is R8G8B8, DirectX will return the texture with format X8R8G8B8
					for (uint32_t i = 0; i < mHeight; ++i)
					{
						for (uint32_t j = 0; j < mWidth; ++j)
						{
							r = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size];
							g = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size + 1];
							b = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size + 2];
							temp = 0xff000000 |(r << 16) | (g << 8) | b;
							memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
						}
					}
					break;
				}
			case PIX_FMT_A8R8G8B8:
				{
					pix_size = 4;
					
					for(uint32_t i = 0; i < mHeight; ++i)
					{
						for(uint32_t j = 0; j < mWidth; ++j)
						{
							r = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size];
							g = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size + 1];
							b = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size + 2];
							a = ((BYTE*)data6[face_i])[i*mWidth*pix_size + j*pix_size + 3];
							temp = (a << 24) |(r << 16) | (g << 8) | b;
							memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
						}
					}
					break;
				}
			}
			mD3DCubeTexture->UnlockRect(cube_faces[face_i], 0);
		}
	}
}

DX9Texture::~DX9Texture()
{
	Destroy();
}

void DX9Texture::Destroy()
{
	SAFE_RELEASE(mD3DTexture);
	SAFE_RELEASE(mD3DCubeTexture);
	Texture::Destroy();
}

void DX9Texture::SetTexture(uint32_t sampler_id)
{
	mSamplerID = sampler_id;
	if (mType == TEXTURE_2D || mType == TEXTURE_1D)
		mpD3DDevice->SetTexture(sampler_id, mD3DTexture);
	if (mType == TEXTURE_CUBE)
		mpD3DDevice->SetTexture(sampler_id, mD3DCubeTexture);
}

void DX9Texture::UnsetTexture()
{
	mpD3DDevice->SetTexture(mSamplerID, 0);
}

void DX9Texture::SetTextureParameter(TEXTURE_PARAMETER param, uint32_t value)
{
	if (param >= 0 && param <= MAG_FILTER)
	{
		mpD3DDevice->SetSamplerState(mSamplerID, _ConvertToD3DTextureParameter(param), _ConvertToD3DFilterType(value));

		// for mipmap filter
		if (value == FILTER_NEAREST_MIPMAP_NEAREST || value == FILTER_LINEAR_MIPMAP_NEAREST)
			mpD3DDevice->SetSamplerState(mSamplerID, D3DSAMP_MIPFILTER,D3DTEXF_POINT);

		if (value == FILTER_NEAREST_MIPMAP_LINEAR || value == FILTER_LINEAR_MIPMAP_LINEAR)
			mpD3DDevice->SetSamplerState(mSamplerID, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
	}

	if (param >= ADDRESS_U && param <= ADDRESS_W)
		mpD3DDevice->SetSamplerState(mSamplerID, _ConvertToD3DTextureParameter(param), _ConvertToD3DAddress(value));
}

void DX9Texture::SetTextureBlend(TEXTURE_BLEND_OP op)
{
	mpD3DDevice->SetTextureStageState(mSamplerID, D3DTSS_COLOROP, _ConvertToD3DTXTUREOP(op));
	mpD3DDevice->SetTextureStageState(mSamplerID, D3DTSS_COLORARG1, D3DTA_TEXTURE);

	if (op != TB_REPLACE)
		mpD3DDevice->SetTextureStageState(mSamplerID, D3DTSS_COLORARG2, D3DTA_CURRENT);
}

void DX9Texture::Update2DTexture(uint32_t x_offset, uint32_t y_offset, uint32_t width, uint32_t height,
								 PIXEL_FORMAT format, void* data)
{
	D3DLOCKED_RECT	rect;
	mD3DTexture->LockRect(0, &rect, NULL, 0);

	uint32_t pix_size = 0;

	uint32_t	temp;
	uint8_t		r, g, b, a;
	switch(mFormat)
	{
	case PIX_FMT_R8G8B8:
		{
			pix_size = 3;

			// even the format is R8G8B8, DirectX will return the texture with format X8R8G8B8
			for (uint32_t i = 0; i < height; ++i)
			{
				for (uint32_t j = 0; j < width; ++j)
				{
					
					r = ((BYTE*)data)[i*width*pix_size + j*pix_size];
					g = ((BYTE*)data)[i*width*pix_size + j*pix_size + 1];
					b = ((BYTE*)data)[i*width*pix_size + j*pix_size + 2];
					temp = 0xff000000 |(r << 16) | (g << 8) | b;
					memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
				}
			}
			break;
		}
	case PIX_FMT_A8R8G8B8:
		{
			pix_size = 4;

			for (uint32_t i = 0; i < height; ++i)
			{
				for (uint32_t j = 0; j < width; ++j)
				{
					r = ((BYTE*)data)[i*width*pix_size + j*pix_size];
					g = ((BYTE*)data)[i*width*pix_size + j*pix_size + 1];
					b = ((BYTE*)data)[i*width*pix_size + j*pix_size + 2];
					a = ((BYTE*)data)[i*width*pix_size + j*pix_size + 3];
					temp = (a << 24) |(r << 16) | (g << 8) | b;
					memcpy( (BYTE*)rect.pBits + i*rect.Pitch + j*4, &temp, 4);
				}
			}
			break;
		}
	}

	mD3DTexture->UnlockRect(0);
}

END_ENGINE_NAMESPACE
