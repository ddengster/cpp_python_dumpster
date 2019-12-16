
#ifndef _OGLHARDWAREBUFFERMANAGER_H
#define _OGLHARDWAREBUFFERMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"
#include "RenderSystem/GLHeaders.h"

DECLARE_ENGINE_NAMESPACE

class OGLHardwareBufferManager : public HardwareBufferManager
{
public:
	OGLHardwareBufferManager();
	virtual ~OGLHardwareBufferManager();

	VertexBuffer* GenerateVertexBuffer(VertexDeclaration* vert_dec, uint32_t n, HARDWARE_BUFFER_USAGE usg);
	IndexBuffer*  GenerateIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT ft, HARDWARE_BUFFER_USAGE usg);

	static	GLenum			_ConvertToGLLock(LOCK lock);			// convert to OpenGL map option
	static	GLint			_ConvertToGLType(ELEMENT_TYPE type, GLenum &gl_typ);
};

END_ENGINE_NAMESPACE

#endif