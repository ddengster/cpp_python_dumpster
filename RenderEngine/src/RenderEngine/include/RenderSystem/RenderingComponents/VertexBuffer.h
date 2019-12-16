
#ifndef _VERTEXBUFFER_H
#define _VERTEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include "RenderSystem/RenderingComponents/VertexDeclaration.h"
#include <vector>

DECLARE_ENGINE_NAMESPACE

/** \ingroup RenderSystem
 * \class VertexBuffer
 *
 * Vertex Buffer class to manage vertices, so that both render systems can accept them.
 *
 * \Header
 */
class VertexBuffer
{
public:
	VertexBuffer()
		: mDeclaration(0), mVertexNumber(0), mVertexStride(0), mBufferUsage(HDW_BUF_USG_STATIC)
	{
	}

	virtual ~VertexBuffer()
	{
	}

	VertexDeclaration*		GetVertexDeclaration()	{	return mDeclaration;	}
	uint32_t				GetVertexStride()		{	return mVertexStride;	}
	uint32_t				GetNumberOfVertices()	{	return mVertexNumber;	}
	uint32_t				GetBufferSize()			{	return mVertexNumber*mVertexStride;	}
	bool					IsStatic()				{	return (mBufferUsage == HDW_BUF_USG_STATIC);	}

	// lock the buffer to get the data pointer
	virtual	void*			Lock(uint32_t begin, uint32_t n, LOCK lock) = 0;
	// unlock the buffer
	virtual void			Unlock() = 0;

	// copy data to the vertex buffer, with this function, there is no need to call Lock() and Unlock()
	virtual void			UpdateData(void* data, uint32_t start, uint32_t length) = 0;
	// copy only the data for certain usage (e.g. position) to the vertex buffer 
	virtual void			UpdateData(ELEMENT_USAGE usg, uint32_t usg_index,  void* data, uint32_t start, uint32_t length) = 0;
	
	// copy data out of vertex buffer 
	virtual void			CopyData(void* out_data, uint32_t start, uint32_t length) = 0;
	virtual void			CopyData(ELEMENT_USAGE usg, uint32_t usg_index, void* out_data, uint32_t start, uint32_t length) = 0;

	// Before using vertex buffer, this function has to be called
	virtual	void			SetBuffer() = 0;
	virtual void			UnsetBuffer() = 0;

protected:
	VertexDeclaration		*mDeclaration;

	// Do we need to keep a copy? Keep a copy for animations in meshes
//	void							*mData;
	uint32_t						mVertexNumber;
	uint32_t						mVertexStride;
	HARDWARE_BUFFER_USAGE			mBufferUsage;
};

END_ENGINE_NAMESPACE

#endif
