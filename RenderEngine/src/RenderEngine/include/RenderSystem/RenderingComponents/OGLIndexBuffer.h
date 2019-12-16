
#ifndef _OGLINDEXBUFFER_H
#define _OGLINDEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/IndexBuffer.h"
#include "RenderSystem/GLHeaders.h"

DECLARE_ENGINE_NAMESPACE

// OpenGL implementation of IndexBuffer
class OGLIndexBuffer : public IndexBuffer
{
private:
	GLuint	mIndexBufferID;

public:
	OGLIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT format, HARDWARE_BUFFER_USAGE usg);
	virtual ~OGLIndexBuffer();

	void*	Lock(uint32_t begin, uint32_t n, LOCK lock);
	void	Unlock();

	void	UpdateData(void* data, uint32_t start, uint32_t length);
	void	CopyData(void* out_data, uint32_t start, uint32_t length);

	void	SetBuffer();
	void	UnsetBuffer();
};

END_ENGINE_NAMESPACE

#endif