
#ifndef _DX9TEXTURE_H
#define _DX9TEXTURE_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/Texture.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class DX9Texture : public Texture
{
private:
	LPDIRECT3DTEXTURE9		mD3DTexture;
	LPDIRECT3DCUBETEXTURE9	mD3DCubeTexture;
	LPDIRECT3DDEVICE9		mpD3DDevice;

public:
	DX9Texture();

	// constructor for texture 2d
	void Initialize(const String& name, const String& path, TEXTURE_TYPE type, 
					uint32_t width, uint32_t height, uint32_t mip_level, 
					PIXEL_FORMAT format, void* data, LPDIRECT3DDEVICE9 dev);
	// constructor for texture cube
	void Initialize(const String& name, const String& path, TEXTURE_TYPE type,
					uint32_t width, uint32_t height, uint32_t mip_level, PIXEL_FORMAT format,
					void** data6, LPDIRECT3DDEVICE9 dev);
	virtual ~DX9Texture();

	void Destroy();

	// set filter and address parameter
	void SetTextureParameter(TEXTURE_PARAMETER param, uint32_t value);
	// set texture combine
	void SetTextureBlend(TEXTURE_BLEND_OP op);

	// set texture before set parameters
	void SetTexture(uint32_t sampler_id);
	void UnsetTexture();

	virtual void Update2DTexture(uint32_t x_offset, uint32_t y_offset, uint32_t width, uint32_t height,
								 PIXEL_FORMAT format, void* data);

	static  D3DFORMAT				_ConvertToD3DPixelFormat(PIXEL_FORMAT format);
	static  D3DSAMPLERSTATETYPE		_ConvertToD3DTextureParameter(TEXTURE_PARAMETER param);
	static	D3DTEXTUREFILTERTYPE	_ConvertToD3DFilterType(uint32_t filter);
	static	D3DTEXTUREADDRESS		_ConvertToD3DAddress(uint32_t address);
	static	DWORD					_ConvertToD3DTA(TEXTURE_BLEND_ARG arg);
	static	D3DTEXTUREOP			_ConvertToD3DTXTUREOP(TEXTURE_BLEND_OP op);

};

END_ENGINE_NAMESPACE

#endif
