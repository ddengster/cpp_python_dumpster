
#ifndef _OGLRENDERER_H
#define _OGLRENDERER_H

#include "RenderSystem/RenderingComponents/IRenderer.h"
#include "RenderSystem/GLHeaders.h"
#include <windows.h>

DECLARE_ENGINE_NAMESPACE

class RenderEngine;

class OGLRenderer : public IRenderer
{
	INTERFACE_RENDERER(;)
	virtual ~OGLRenderer();

	static	GLenum		_ConvertToGLPrimitiveType(PRIMITIVE_TYPE type);
	static	uint32_t	_CalculatePrimitiveIndexCount(PRIMITIVE_TYPE type, uint32_t count);

protected:
	friend class RenderEngine;
	OGLRenderer();

	bool CheckCompatibility();
private:
	HWND	mWindowHandle;

	HDC		mhDC;
	HGLRC	mhRC;

	Matrix4	mModelMatrix;
	Matrix4	mViewMatrix;
};

END_ENGINE_NAMESPACE

#endif
