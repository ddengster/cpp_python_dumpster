
#ifndef _OGLVERTEXBUFFER_H
#define _OGLVERTEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/VertexBuffer.h"
#include "RenderSystem/GLHeaders.h"

DECLARE_ENGINE_NAMESPACE

class OGLVertexBuffer : public VertexBuffer
{
public:
	OGLVertexBuffer(VertexDeclaration* vert_dec, uint32_t vertex_number, HARDWARE_BUFFER_USAGE usg);
	~OGLVertexBuffer();

	// it actually locks the entire buffer no matter what are the values of begin and n
	void*		Lock(uint32_t begin, uint32_t n, LOCK lock);
	void		Unlock();

	void		UpdateData(void* data, uint32_t start, uint32_t length);
	void		UpdateData(ELEMENT_USAGE usg, uint32_t usg_index,  void* data, uint32_t start, uint32_t length);
	
	void		CopyData(void* out_data, uint32_t start, uint32_t length);
	void		CopyData(ELEMENT_USAGE usg, uint32_t usg_index, void* out_data, uint32_t start, uint32_t length);

	void		SetBuffer();
	void		UnsetBuffer();

private:
	GLuint			mVertexBufferID;
	GLbyte			*mElementOffsets;
};

END_ENGINE_NAMESPACE

#endif