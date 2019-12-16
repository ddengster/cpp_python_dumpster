
#ifndef _RENDERERDEFINES_H
#define _RENDERERDEFINES_H

#include "Prerequisites.h"

DECLARE_ENGINE_NAMESPACE
/** 
 * Clear Buffer Type 
 * For use in parameter target of IRenderer::ClearFrame function. 
 * Use bitwise operators if you want to use more than 1 buffer type.
 */
enum CLEAR_BUFFER_TYPE
{
	CLEAR_TARGET = 0x01,		/*!< Clear Target*/
	CLEAR_ZBUFFER = 0x02,		/*!< Clear Z Buffer*/
	CLEAR_STENCIL = 0x04		/*!< Clear Stencil Buffer*/
};

/** Matrix type for use with renderers */
enum MATRIX_TYPE 
{
	PROJECTION_MATRIX = 0,		/*!< Projection matrix */
	WORLD_MATRIX,				/*!< World matrix */
	VIEW_MATRIX,				/*!< View matrix */
	TEXTURE_MATRIX,				/*!< Texture matrix */
	NUM_MATRICES				/*!< Number of matrix types */
};

/*****************************/
/**** Vertex Buffer stuff ****/
/*****************************/
// to do: support more usage of element 
enum ELEMENT_USAGE
{
	ELE_USG_POSITION = 0,		/*!< Position */
	ELE_USG_NORMAL,				/*!< Normals */
	ELE_USG_COLOR,				/*!< Color */
	ELE_USG_TEXCOORD,			/*!< Texture Coordinates */
	NUM_ELEMENT_USAGES			/*!< Number of Element usage */
};

// to do: support more data type
enum ELEMENT_TYPE
{
	ELE_TYP_FLOAT = 0,		/*!< 1 Float for the element type*/
	ELE_TYP_FLOAT2,			/*!< 2 Floats for the element type*/
	ELE_TYP_FLOAT3,			/*!< 3 Floats for the element type*/
	ELE_TYP_FLOAT4,			/*!< 4 Floats for the element type*/
	ELE_TYP_UBYTE4,			/*!< 4 unsigned bytes for the element type*/
	NUM_ELEMENT_TYPES
};

// to do: support more buffer usage type
enum HARDWARE_BUFFER_USAGE
{
	HDW_BUF_USG_STATIC = 0,		// don't change at runtime
	HDW_BUF_USG_DYNAMIC			// change often
};

enum INDEX_BUFFER_FORMAT
{
	IDX_BUF_16,			/*!< 16 bit index buffer */
	IDX_BUF_32			/*!< 32 bit index buffer */
};

// to do: support more lock option
enum LOCK
{
	LOCK_DEFAULT = 0,		// no special instruction
	LOCK_READONLY,			// read only
	LOCK_DISCARD,			// discard the memory, dynamic buffer only.
	NUM_LOCK
};

/** Primitive types to be used with renderers */
enum PRIMITIVE_TYPE 
{
	PRIMITIVE_POINT_LIST = 0,	/*!< List of points */
	PRIMITIVE_TRI_LIST,			/*!< List of triangles */
	PRIMITIVE_TRI_STRIP,		/*!< Triangle strip */
	PRIMITIVE_TRI_FAN,			/*!< Triangle fan */
	PRIMITIVE_LINE_LIST,		/*!< List of lines */
	PRIMITIVE_LINE_STRIP,		/*!< Line strip */
	PRIMITIVE_QUAD_LIST,		/*!< List of quads */
	PRIMITIVE_POLYGON_LIST,		/*!< List of polygons */
	NUM_PRIMITIVES				/*!< Number of primitive types */
};

/**************************/
/****Renderstate stuff ****/
/**************************/
/** Render states for renderer */
enum RENDERSTATE 
{
	RS_NULL = 0,		/*!< Invalid render state */
	RS_SHADING,			/*!< Shading mode */
	RS_DEPTH_TESTING,	/*!< Depth-testing mode */
	RS_CULLING,			/*!< Backface culling mode */
	RS_FILLMODE,		/*!< Polygon fill mode */
	RS_LIGHTING,		/*!< Lighting mode */
	RS_ALPHA_TESTING,	/*!< Alpha blending mode */
	RS_BLENDING,		/*!< Blending mode */
	RS_TEXTURE_2D,		/*!< Texture mode */
	RS_ZWRITE,			/*!< ZWrite mode, only for directx*/
	NUM_RENDERSTATES	/*!< Number of render states */
};

/** Shading mode constants */
enum RS_SHADING_MODE
{
	SHADEMODE_FLAT = 0,		/*!< Flat-shading mode */
	SHADEMODE_SMOOTH,		/*!< Smooth-shading mode */
	NUM_SHADEMODE		/*!< Number of shading modes */
};
/** Depth-testing mode */
enum RS_DEPTH_TESTING_MODE
{
	DEPTHMODE_DISABLE = 0, /*!< Depth-testing disabled */
	DEPTHMODE_ENABLE,		 /*!< Depth-testing enabled */
	NUM_DEPTHMODE			 /*!< Number of depth-testing modes */
};
/** Backface culling mode */
enum RS_CULLING_MODE
{
	CULLMODE_DISABLE = 0, /*!< Backface culling disabled */
	CULLMODE_CW,			/*!< Clockwise backface culling */
	CULLMODE_CCW,			/*!< Counter-clockwise backface culling */
	NUM_CULLMODE			/*!< Number of culling modes */
};
/** Polygon fill mode */
enum RS_FILLMODE
{
	FILLMODE_POINT = 0,	/*!< Draw only points */
	FILLMODE_WIREFRAME,	/*!< Draw polygons in wireframe mode */
	FILLMODE_SOLID,		/*!< Draw polygons and fill as solid */
	NUM_FILLMODE			/*!< Number of fill modes */
};
/** Lighting mode */
enum RS_LIGHTING_MODE
{
	LIGHTING_DISABLE = 0,	/*!< Disable lighting */
	LIGHTING_ENABLE,		/*!< Enable lighting */
	NUM_LIGHTINGMODE		/*!< Number of lighting modes */
};
/** Blending mode */
enum RS_BLENDING_MODE
{
	BLENDING_DISABLE = 0,	/*!< Disable blending */
	BLENDING_ENABLE,		/*!< Enable blending */
	NUM_BLENDINGMODE		/*!< Number of blending modes */
};

enum ALPHA_TESTING_MODE
{
	ALPHA_TESTING_ENABLE = 0,		/*!< Enable Alpha testing */
	ALPHA_TESTING_DISABLE,		/*!< Disable Alpha testing */
	NUM_ALPHA_TESTING				/*!< Number of alpha testing modes */
};

/** 2D Texture mode */
enum RS_TEXTURE_2D
{
	TEXTURE_2D_DISABLE = 0,	/*!< Disable textures */
	TEXTURE_2D_ENABLE,		/*!< Enable textures */
	NUM_TEXTURE_2D			/*!< Number of texture modes */
};

enum RS_ZWRITE_MODE
{
	ZWRITE_FALSE = 0,       /*< Disable Z Writing */
	ZWRITE_TRUE,            /*< Enable Z Writing */
	NUM_ZRITEMODE           /*< Number of Z Write modes */
};

/** Color modes: DX9CreateTextureFromFile reads in colors differently, but freeImage will flip them for you**/

/*****************/
/***Color stuff***/
/*****************/
//Macros that help combine 4 rgba values into 1
#define COLOR uint32_t

/** Define a color in ARGB mode */
#define COLOR_ARGB(a,r,g,b) ((COLOR)((((a)&0xff)<<24)|(((r)&0xff)<<16)|(((g)&0xff)<<8)|((b)&0xff)))
/** Define a color in RGBA mode */
#define COLOR_RGBA(r,g,b,a) COLOR_ARGB(a,r,g,b)

//Preferable to use the following 4 macros

//for 0 to 255 range
/** Define a color in RGBA mode, using the 0-255 range for each component */
#define COLOR_RGBA_255INT(r, g, b,a) COLOR_RGBA(r,g,b,a)
//for 0.0f to 1.0f range
/** Define a color in RGBA mode, using the 0.0-1.0 range for each component */
#define COLOR_RGBA_1F(r,g,b,a) \
	COLOR_RGBA((COLOR)((r)*255.f),(COLOR)((g)*255.f),(COLOR)((b)*255.f),(COLOR)((a)*255.f))
//for 0 to 255 range
/** Define a color in ARGB mode, using the 0-255 range for each component */
#define COLOR_ARGB_255INT(a, r, g, b) COLOR_ARGB(a,r,g,b)
//for 0.0f to 1.0f range
/** Define a color in ARGB mode, using the 0.0-1.0 range for each component */
#define COLOR_ARGB_1F(a,r,g,b) \
	COLOR_ARGB((COLOR)((a)*255.f),(COLOR)((r)*255.f),(COLOR)((g)*255.f),(COLOR)((b)*255.f))

/*******************/
/***Texture Stuff***/
/*******************/

/** Texture type */
enum TEXTURE_TYPE
{
	TEXTURE_1D = 0,	/*!< 1D texture */
	TEXTURE_2D,		/*!< 2D texture */
	TEXTURE_CUBE,		/*!< Cube texture */
	NUM_TEXTURE_TYPE	/*!< Number of texture types */
};

enum PIXEL_FORMAT
{
	PIX_FMT_R8G8B8,			/*!< RGB 24bit format */
	PIX_FMT_A8R8G8B8,		/*!< ARGB 32bit format */
	NUM_PIXEL_FORMAT		/*!< Number of pixel formats */
};

enum TEXTURE_PARAMETER
{
	MIN_FILTER = 0,			/*!< Minification parameter */
	MAG_FILTER,				/*!< Magnification parameter */
	ADDRESS_U,				/*!< Address U parameter */
	ADDRESS_V,				/*!< Address V parameter */
	ADDRESS_W,				/*!< Address W parameter */
	NUM_TEXTURE_PARAMETER		/*!< Number of texture parameters */
};

enum TEXTURE_ADDRESS
{
	ADDRESS_WRAP = 0,		/*!< Wrap address */
	ADDRESS_MIRROR,		/*!< Mirror address */
	ADDRESS_CLAMP,		/*!< Clamp address */
	NUM_TEXTURE_ADDRESS	/*!< Number of texture addresses */
};

enum TEXTURE_FILTER
{
	FILTER_NEAREST,						/*!< Nearest filter */
	FILTER_LINEAR,						/*!< Linear filter */
	FILTER_NEAREST_MIPMAP_NEAREST,		/*!< Nearest mipmap nearest filter */
	FILTER_NEAREST_MIPMAP_LINEAR,		/*!< Nearest mipmap linear filter */
	FILTER_LINEAR_MIPMAP_NEAREST,		/*!< Linear mipmap nearest filter */
	FILTER_LINEAR_MIPMAP_LINEAR,		/*!< Linear mipmap linear filter */
	NUM_TEXTURE_FILTER					/*!< Number of texture filters */
};

// blend operation
enum TEXTURE_BLEND_OP
{
	TB_REPLACE,				/*!< Replace blending operation */
	TB_MODULATE,				/*!< Modulate blending operation */
	TB_ADD,					/*!< Addition blending operation */
	TB_DECAL,					/*!< Decal blending operation */
	NUM_TEXTURE_BLEND_OP		/*!< Number of blending operations */
};

// blend args, 
// Only support color blend right now
// Not in use currently
enum TEXTURE_BLEND_ARG
{
	TBA_CURRENT,
	TBA_TEXTURE,
	TBA_DIFFUSE,
	NUM_TEXTURE_BLEND_ARG
};

/******************/
/***Shader Stuff***/
/******************/
// Vertex shader or pixel shader
enum SHADER_TYPE
{
	VERTEX_SHADER,	/*!< Vertex shader type*/
	PIXEL_SHADER,		/*!< Pixel shader type*/
	NUM_SHADER_TYPE	/*!< Number of shader types*/
};

// Shader profile 
enum SHADER_VERSION
{
	DX9_VS_1_1,
	DX9_VS_2_0,
	DX9_VS_2_A,
	DX9_VS_3_0,

	DX9_PS_1_1,
	DX9_PS_1_2,
	DX9_PS_1_3,
	DX9_PS_1_4,
	DX9_PS_2_0,
	DX9_PS_2_A,
	DX9_PS_2_B,
	DX9_PS_3_0,

	OGL_VS,
	OGL_PS,

	NUM_SHADER_VERSION
};

enum SHADER_PARAMETER_AUTO
{
	SPA_WORLD_MATRIX,						/*!< float4x4 world matrix */
	SPA_VIEW_MATRIX,						/*!< float4x4 view matrix */
	SPA_PROJECTION_MATRIX,				/*!< float4x4 projection matrix */
	SPA_WORLD_VIEW_PROJECTION_MATRIX,		/*!< float4x4 world view projection matrix */
	SPA_WORLD_IT_MATRIX,					/*!< float4x4 world IT matrix */
	SPA_CAMERA_POSITION,					/*!< float4 camera position in world space*/

	NUM_SPA			/*!< Number of shader parameter autos */
};


END_ENGINE_NAMESPACE

#endif
