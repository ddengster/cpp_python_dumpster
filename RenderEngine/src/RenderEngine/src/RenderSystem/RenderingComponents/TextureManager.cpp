
#include "RenderSystem/RenderingComponents/TextureManager.h"
#include "RenderSystem/RenderEngine.h"
#include "LogManager/LogManager.h"
#include "FreeImage.h"

DECLARE_ENGINE_NAMESPACE

TextureManager* TextureManager::GetInstance()
{
	return RenderEngine::GetTextureManager();
}

TextureManager::TextureManager()
:ResourceManager<Texture>(TABLE_SMALL_SIZE)
{	
	mMipmapLevel = 4;

	for (uint32_t i = 0; i < NUM_CUBE_FACES; ++i)
		mCubeImages[i] = NULL;

	m2DImage = NULL;
}

TextureManager::~TextureManager()
{
}

bool TextureManager::Load2DImageFromFile(const String& full_name,
										 uint32_t &width, uint32_t &height,
										 PIXEL_FORMAT &pix_fmt)
{
	// Get the image file type from FreeImage.
	FREE_IMAGE_FORMAT fileFormat = FreeImage_GetFileType(full_name.c_str(), 0);
	if (fileFormat == FIF_UNKNOWN)
	{
		//no signature, thus try to get file format from file extension
		fileFormat = FreeImage_GetFIFFromFilename(full_name.c_str());
	}

	FIBITMAP* dib = FreeImage_Load(fileFormat, full_name.c_str(), 0);

	if (dib == NULL)
	{
		LogManager::GetInstance()->AppendToLog("Error: " + full_name + " : format not supported.");
		FreeImage_Unload(dib);

		return false;
	}

	pix_fmt = NUM_PIXEL_FORMAT;

	if (FreeImage_GetImageType(dib) == FIT_BITMAP)
	{
		width        = FreeImage_GetWidth(dib);
		height       = FreeImage_GetHeight(dib);
		uint32_t	bpp = FreeImage_GetBPP(dib);

		if (bpp == 24)
		{
			// This is important to note, FreeImage loads textures in
			// BGR format. Now we could just use the GL_BGR extension
			// But, we will simply swap the B and R components ourselves.
			// Firstly, allocate the new bit data doe the image.
			m2DImage = new BYTE[width*height * 3];

			// get a pointer to FreeImage's data.
			BYTE *pixels = (BYTE*)FreeImage_GetBits(dib);

			// Iterate through the pixels, copying the data
			// from 'pixels' to 'bits' except in RGB format.
			for(uint32_t pix=0; pix< width * height; ++pix)
			{
				m2DImage[pix*3+0]=pixels[pix*3+2];
				m2DImage[pix*3+1]=pixels[pix*3+1];
				m2DImage[pix*3+2]=pixels[pix*3+0];
			}

			pix_fmt = PIX_FMT_R8G8B8;
			FreeImage_Unload(dib);

			return true;
		}

		if (bpp == 32)		
		{
			m2DImage = new BYTE[width*height * 4];
			BYTE *pixels = (BYTE*)FreeImage_GetBits(dib);

			for(uint32_t pix = 0; pix < width * height; ++pix)
			{
				m2DImage[pix*4 + 0] = pixels[pix*4 + 2];
				m2DImage[pix*4 + 1] = pixels[pix*4 + 1];
				m2DImage[pix*4 + 2] = pixels[pix*4 + 0];
				m2DImage[pix*4 + 3] = pixels[pix*4 + 3];
			}

			pix_fmt = PIX_FMT_A8R8G8B8;
			FreeImage_Unload(dib);

			return true;
		}
	}
	LogManager::GetInstance()->AppendToLog("Error: " + full_name + " : format not supported.");
	FreeImage_Unload(dib);
	return false;
}

void TextureManager::Release2DImage()
{
	if(m2DImage != NULL)
		delete[] m2DImage;
	m2DImage = NULL;
}

bool TextureManager::LoadCubeImagesFromFile(const String& name, 
											const String& dir, 
											uint32_t &width, uint32_t& height, 
											PIXEL_FORMAT &pix_fmt)
{
	size_t	p_dot = name.rfind('.');

	String	b_name = name.substr(0, p_dot);
	String	e_name = name.substr(p_dot+1);

	String	full_names[NUM_CUBE_FACES];
	full_names[0] = dir + b_name + "_rt." + e_name;
	full_names[1] = dir + b_name + "_lf." + e_name;
	full_names[2] = dir + b_name + "_up." + e_name;
	full_names[3] = dir + b_name + "_dn." + e_name;
	full_names[4] = dir + b_name + "_fr." + e_name;
	full_names[5] = dir + b_name + "_bk." + e_name;

	pix_fmt = NUM_PIXEL_FORMAT;
	width = 1;
	height = 1;

	for (uint32_t image_i = 0; image_i < NUM_CUBE_FACES; ++image_i)
	{
		FREE_IMAGE_FORMAT fileFormat = FreeImage_GetFileType(full_names[image_i].c_str(), 0);
		if (fileFormat == FIF_UNKNOWN)
		{
			fileFormat = FreeImage_GetFIFFromFilename(full_names[image_i].c_str());
		}

		FIBITMAP* dib = FreeImage_Load(fileFormat, full_names[image_i].c_str(), 0);

		if (dib == NULL)	
		{
			LogManager::GetInstance()->AppendToLog("Error: " + full_names[image_i] + " : format not support.");
			FreeImage_Unload(dib);

			return false;
		}
		FreeImage_FlipVertical(dib);

		if (FreeImage_GetImageType(dib) == FIT_BITMAP)
		{
			width        = FreeImage_GetWidth(dib);
			height       = FreeImage_GetHeight(dib);
			uint32_t	bpp = FreeImage_GetBPP(dib);

			if (bpp == 24)
			{
				mCubeImages[image_i] = new BYTE[width*height * 3];
				BYTE *pixels = (BYTE*)FreeImage_GetBits(dib);

				for(uint32_t pix=0; pix < width * height; ++pix)
				{
					mCubeImages[image_i][pix*3+0]=pixels[pix*3+2];
					mCubeImages[image_i][pix*3+1]=pixels[pix*3+1];
					mCubeImages[image_i][pix*3+2]=pixels[pix*3+0];
				}

				FreeImage_Unload(dib);
				pix_fmt = PIX_FMT_R8G8B8;
			}

			if (bpp == 32)		
			{
				mCubeImages[image_i] = new BYTE[width*height* 4];
				BYTE *pixels = (BYTE*)FreeImage_GetBits(dib);

				for(uint32_t pix = 0; pix < width * height; ++pix)
				{
					mCubeImages[image_i][pix*4 + 0] = pixels[pix*4 + 2];
					mCubeImages[image_i][pix*4 + 1] = pixels[pix*4 + 1];
					mCubeImages[image_i][pix*4 + 2] = pixels[pix*4 + 0];
					mCubeImages[image_i][pix*4 + 3] = pixels[pix*4 + 3];
				}

				FreeImage_Unload(dib);
				pix_fmt = PIX_FMT_A8R8G8B8;
			}

			if (pix_fmt == NUM_PIXEL_FORMAT)
			{
				LogManager::GetInstance()->AppendToLog("Error: " + full_names[image_i] + " : format not supported.");
				FreeImage_Unload(dib);
				return false;
			}
		}
		else
		{
			LogManager::GetInstance()->AppendToLog("Error: " + full_names[image_i] + " : format not supported.");
			FreeImage_Unload(dib);
			return false;
		}
	}

	return true;
}

void TextureManager::ReleaseCubeImages()
{
	for (uint32_t image_i=0; image_i < NUM_CUBE_FACES; ++image_i)
	{
		if (mCubeImages[image_i] != NULL)
			delete[] mCubeImages[image_i];
		mCubeImages[image_i] = NULL;
	}
}

END_ENGINE_NAMESPACE
