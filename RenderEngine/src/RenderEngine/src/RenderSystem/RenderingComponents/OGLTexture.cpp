
#include "RenderSystem/RenderingComponents/OGLTexture.h"
#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

GLenum OGLTexture::_ConvertToGLPixelFormat(PIXEL_FORMAT format)
{
	static GLenum table[NUM_PIXEL_FORMAT] = { GL_RGB8, GL_RGBA8 };
	return table[format];
}

GLenum OGLTexture::_ConvertToGLTextureParameter(TEXTURE_PARAMETER param)
{
	static GLenum table[NUM_TEXTURE_PARAMETER] = {	GL_TEXTURE_MIN_FILTER,
													GL_TEXTURE_MAG_FILTER,
													GL_TEXTURE_WRAP_S,
													GL_TEXTURE_WRAP_T,
													GL_TEXTURE_WRAP_R };

	return table[param];
}

GLint OGLTexture::_ConvertToGLFilterType(uint32_t filter)
{
	static GLint table[NUM_TEXTURE_FILTER] = {	GL_NEAREST,
												GL_LINEAR,
												GL_NEAREST_MIPMAP_NEAREST,
												GL_NEAREST_MIPMAP_LINEAR,
												GL_LINEAR_MIPMAP_NEAREST,
												GL_LINEAR_MIPMAP_LINEAR };

	return table[filter];
}

GLint OGLTexture::_ConvertToGLAddress(uint32_t address)
{
	static GLint table[NUM_TEXTURE_ADDRESS] = {	GL_REPEAT,
												GL_MIRRORED_REPEAT,
												GL_CLAMP };
	return table[address];
}

GLint OGLTexture::_ConvertToGLTEXTUREOP(TEXTURE_BLEND_OP op)
{
	static GLint table[NUM_TEXTURE_BLEND_OP] = {GL_REPLACE,
												GL_MODULATE,
												GL_ADD,
												GL_DECAL
											};
	return table[op];
}

OGLTexture::OGLTexture()
:Texture(), mGLTextureType(0), mTextureID(0)
{
}

void OGLTexture::Initialize(const String& name, const String& path, TEXTURE_TYPE type,
						   uint32_t width, uint32_t height, uint32_t mip_level,
						   PIXEL_FORMAT format, void* data)
{
	Texture::Initialize(name, path, type, width, height, mip_level, format);

	if (mType == TEXTURE_2D)	
		mGLTextureType = GL_TEXTURE_2D;
	if (mType == TEXTURE_1D)
		mGLTextureType = GL_TEXTURE_1D;

	if (mType != TEXTURE_2D && mType != TEXTURE_1D)
	{
		LogManager::GetInstance()->AppendToLog("OGLTexture constructor: Incorrect TEXTURE_TYPE");
		return;
	}

	glGenTextures(1, &mTextureID);
	if (data)
	{
		glBindTexture(mGLTextureType, mTextureID);
		//glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); is faster according to:
		//http://www.gamedev.net/community/forums/topic.asp?topic_id=356323

		GLint img_fmt;
		switch (mFormat)
		{
			case PIX_FMT_R8G8B8:
				img_fmt = GL_RGB;
				break;
			case PIX_FMT_A8R8G8B8:
				img_fmt = GL_RGBA;
				break;
			default:
				LogManager::GetInstance()->AppendToLog("Error: OGLTexture unknown format");
				return;
		}

		if (mType == TEXTURE_2D)
		{
			gluBuild2DMipmaps(GL_TEXTURE_2D, _ConvertToGLPixelFormat(format), width, height, img_fmt, GL_UNSIGNED_BYTE, data);
		}
		if (mType == TEXTURE_1D)
		{
			gluBuild1DMipmaps(GL_TEXTURE_1D, _ConvertToGLPixelFormat(format), width, img_fmt, GL_UNSIGNED_BYTE, data);
		}

		glTexParameteri(mGLTextureType, GL_TEXTURE_BASE_LEVEL, 0);
		glTexParameteri(mGLTextureType, GL_TEXTURE_MAX_LEVEL, mMipmapLevel);
	}
}

void OGLTexture::Initialize(const String& name, const String& path, TEXTURE_TYPE type, 
						   uint32_t width, uint32_t height, uint32_t mip_level, 
						   PIXEL_FORMAT format, void** data6)
{
	Texture::Initialize(name, path, type, width, height, mip_level, format);

	mGLTextureType = GL_TEXTURE_CUBE_MAP;
	if (mType != TEXTURE_CUBE)
	{
		LogManager::GetInstance()->AppendToLog("OGLTexture constructor: Incorrect TEXTURE_TYPE");
		return;
	}

	glGenTextures(1, &mTextureID);
	if (data6)
	{
		glBindTexture(GL_TEXTURE_CUBE_MAP, mTextureID);

		GLint	img_fmt;
		switch (mFormat)
		{
			case PIX_FMT_R8G8B8:
				img_fmt = GL_RGB;
				break;
			case PIX_FMT_A8R8G8B8:
				img_fmt = GL_RGBA;
				break;
			default:
				LogManager::GetInstance()->AppendToLog("Error: OGLTexture unknown format");
				return;
		}

		GLenum cube_faces[NUM_CUBE_FACES] =	{	GL_TEXTURE_CUBE_MAP_POSITIVE_X,
												GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
												GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
												GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
												GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
												GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 
											};

		for (uint32_t i=0; i < NUM_CUBE_FACES; ++i)
		{
			glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_GENERATE_MIPMAP, GL_TRUE);
			glTexImage2D(cube_faces[i], 0, _ConvertToGLPixelFormat(format), width, height, 0, img_fmt, GL_UNSIGNED_BYTE, data6[i]);
		}

		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_BASE_LEVEL, 0);
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_LEVEL, mMipmapLevel);
	}
}

OGLTexture::~OGLTexture()
{
	Destroy();
}

void OGLTexture::Destroy()
{
	glDeleteTextures(1, &mTextureID);
	Texture::Destroy();
}

void OGLTexture::SetTextureParameter(TEXTURE_PARAMETER param, uint32_t value)
{
	if (param >= 0 && param <= MAG_FILTER)
		glTexParameteri(mGLTextureType, _ConvertToGLTextureParameter(param), _ConvertToGLFilterType(value));
	if (param >= ADDRESS_U && param <= ADDRESS_W)
		glTexParameteri(mGLTextureType, _ConvertToGLTextureParameter(param), _ConvertToGLAddress(value));
}

void OGLTexture::SetTextureBlend(TEXTURE_BLEND_OP op)
{
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, _ConvertToGLTEXTUREOP(op));
}

void OGLTexture::SetTexture(uint32_t sampler_id)
{
	mSamplerID = sampler_id;
	glActiveTexture(GL_TEXTURE0 + sampler_id);
	glEnable(mGLTextureType);

	glBindTexture(mGLTextureType, mTextureID);
}
	
void OGLTexture::UnsetTexture()
{
	glActiveTexture(mSamplerID);
	glBindTexture(mGLTextureType, 0);
	glDisable(mGLTextureType);
}

void OGLTexture::Update2DTexture(uint32_t x_offset, uint32_t y_offset, uint32_t width, uint32_t height,
								 PIXEL_FORMAT format, void* data)
{
	glEnable(mGLTextureType);
	glBindTexture(mGLTextureType, mTextureID);

	mFormat = format;

	GLint	img_fmt;
	switch (mFormat)
	{
		case PIX_FMT_R8G8B8:
			img_fmt = GL_RGB;
			break;
		case PIX_FMT_A8R8G8B8:
			img_fmt = GL_RGBA;
			break;
		default:
			LogManager::GetInstance()->AppendToLog("Error: OGLTexture unknown format");
			return;
	}
	gluBuild2DMipmaps(GL_TEXTURE_2D, _ConvertToGLPixelFormat(format), width, height, img_fmt, GL_UNSIGNED_BYTE, data);
	
	//glTexSubImage2D can only be used if you use glTexImage2D to construct your image; Generates GL_INVALID_OPERATION(1282) error code.
	//glTexSubImage2D(mGLTextureType, mMipmapLevel, x_offset, y_offset, width, height, img_fmt, GL_UNSIGNED_BYTE, data);
	//GLenum a = glGetError();
	//std::cout << a << std::endl;
	glDisable(mGLTextureType);
}

END_ENGINE_NAMESPACE
