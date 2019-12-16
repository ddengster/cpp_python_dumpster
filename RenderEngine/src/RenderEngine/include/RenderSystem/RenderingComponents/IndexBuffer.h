
#ifndef _INDEXBUFFER_H
#define _INDEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"

DECLARE_ENGINE_NAMESPACE

/** \ingroup RenderSystem
 * \class IndexBuffer
 *
 * Index Buffer class to manage indices.
 * 
 * \Header
 */
class IndexBuffer
{
protected:
	INDEX_BUFFER_FORMAT			mFormat;
	uint32_t					mIndexNumber;		// number of indices
	uint32_t					mIndexSize;			// index size (16|32)
	HARDWARE_BUFFER_USAGE		mBufferUsage;		// static or dynamic

public:
	IndexBuffer()				{ }
	virtual ~IndexBuffer()		{ }

	uint32_t		GetNumOfIndices()		{	return mIndexNumber;	}
	uint32_t		GetIndexSize()			{	return mIndexSize;		}
	uint32_t		GetBufferSize()			{	return mIndexNumber*mIndexSize;	}
	bool			IsStatic()				{	return (mBufferUsage == HDW_BUF_USG_STATIC);	}

	// Lock the buffer from "begin", return the pointer to the buffer
	// note that both "begin" and "n" are measured by indices, NOT bytes
	virtual void*	Lock(uint32_t begin, uint32_t n, LOCK lock) = 0;
	// Unlock the buffer	
	virtual void	Unlock() = 0;

	// Update the content of the buffer, from "start" to "start + leng" measured by indices.
	// When UpdateData and CopyData is used, there is no need to lock and unlock the buffer,
	// since these two functions have lock and unlock the buffer internally
	virtual void	UpdateData(void* data, uint32_t start, uint32_t length) = 0;
	// Copy the content of the buffer out, from "start" to "start + leng"
	virtual void	CopyData(void* out_data, uint32_t start, uint32_t length) = 0;

	// Call before use the buffer
	virtual void	SetBuffer() = 0;
	// Call if no long use the buffer
	virtual void	UnsetBuffer() = 0;
};

END_ENGINE_NAMESPACE

#endif