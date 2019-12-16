
#ifndef _HARDWAREBUFFERMANAGER_H
#define _HARDWAREBUFFERMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include <vector>

#include "RenderSystem/RenderingComponents/VertexBuffer.h"
#include "RenderSystem/RenderingComponents/IndexBuffer.h"

DECLARE_ENGINE_NAMESPACE

class VertexBuffer;
class IndexBuffer;
class VertexDeclaration;
class HardwareBufferManager
{
protected:
	std::vector<VertexBuffer*>		mVertexBuffers;
	std::vector<VertexDeclaration*>	mVertexDeclarations;
	std::vector<IndexBuffer*>		mIndexBuffers;

	VertexBuffer				*mCurrentVertexBuffer;
	IndexBuffer					*mCurrentIndexBuffer;
	VertexDeclaration			*mCurrentDeclaration;
public:
	static HardwareBufferManager* GetInstance();

	HardwareBufferManager();
	virtual ~HardwareBufferManager();

	// manage vertex declaration, users should only use these function to manage vertex declaration
	VertexDeclaration*	GenerateVertexDeclaration();
	void				DestroyVertexDeclaration(VertexDeclaration* vert_dec);
	void				DestroyAllVertexDeclaration();

	// manage vertex buffer, users should only use these function to creaet or destroy vertex buffer
	// users should not call new/delete VertexBuffer directly.
	virtual VertexBuffer* GenerateVertexBuffer(VertexDeclaration* vert_dec, uint32_t n, HARDWARE_BUFFER_USAGE usg) = 0;
	virtual void		  DestroyVertexBuffer(VertexBuffer *buf);
	virtual void		  DestroyAllVertexBuffer();

	// manage index buffer
	virtual IndexBuffer*  GenerateIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT ft, HARDWARE_BUFFER_USAGE usg) = 0;
	virtual void		  DestroyIndexBuffer(IndexBuffer *buf);
	virtual void		  DestroyAllIndexBuffer();

	VertexBuffer*		GetCurrentVertexBuffer()	{	return mCurrentVertexBuffer;	}
	IndexBuffer*		GetCurrentIndexBuffer()		{	return mCurrentIndexBuffer;		}
	VertexDeclaration*	GetCurrentVertexDeclaration()	{	return mCurrentDeclaration;	}

	void _RegisterCurrentVertexBufferAndDeclaration(VertexBuffer *buf, VertexDeclaration *vert_dec)
	{
		mCurrentVertexBuffer = buf;
		mCurrentDeclaration = vert_dec;
	}

	void _RegisterCurrentIndexBuffer(IndexBuffer *buf)
	{
		mCurrentIndexBuffer = buf;
	}
};

END_ENGINE_NAMESPACE

#endif
