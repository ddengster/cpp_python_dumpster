
#ifndef _OGLTEXTURE_H
#define _OGLTEXTURE_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/Texture.h"
#include "RenderSystem/GLHeaders.h"

DECLARE_ENGINE_NAMESPACE

class OGLTexture : public Texture
{
private:
	GLenum		mGLTextureType;		// OpenGL texture type
	GLuint		mTextureID;

public:
	OGLTexture();
	// Constructor for 1D and 2D texture
	void Initialize(const String& name, const String& path, TEXTURE_TYPE type,
					uint32_t width, uint32_t height, uint32_t mip_level,
					PIXEL_FORMAT format, void* data);
	void Initialize(const String& name, const String& path, TEXTURE_TYPE type,
					uint32_t width, uint32_t height, uint32_t mip_level,
					PIXEL_FORMAT format, void** data6);
	virtual ~OGLTexture();
	void Destroy();

	void SetTextureParameter(TEXTURE_PARAMETER param, uint32_t value);
	void SetTextureBlend(TEXTURE_BLEND_OP op);
	
	void SetTexture(uint32_t sampler_id);
	void UnsetTexture();

	virtual void Update2DTexture(uint32_t x_offset, uint32_t y_offset, uint32_t width, uint32_t height,
								 PIXEL_FORMAT format, void* data);

	// functions for converting Medge format to OpenGL format
	static	GLenum	_ConvertToGLPixelFormat(PIXEL_FORMAT format);
	static  GLenum	_ConvertToGLTextureParameter(TEXTURE_PARAMETER param);
	static	GLint	_ConvertToGLFilterType(uint32_t filter);
	static	GLint	_ConvertToGLAddress(uint32_t address);
	static	GLint	_ConvertToGLTEXTUREOP(TEXTURE_BLEND_OP op);
};

END_ENGINE_NAMESPACE

#endif