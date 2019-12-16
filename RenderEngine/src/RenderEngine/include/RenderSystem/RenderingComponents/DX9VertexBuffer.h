
#ifndef _DX9VERTEXBUFFER_H
#define _DX9VERTEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/VertexBuffer.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class DX9VertexBuffer : public VertexBuffer
{
public:
	// though there is no restriction, 
	// constructor should be called only by DX9HardwareBufferManager
	// DX9HardwareBufferManager is responsible for creating and destroying DX9VertexBuffer
	DX9VertexBuffer(VertexDeclaration *vert_dec, uint32_t vertex_number, HARDWARE_BUFFER_USAGE usg,
					LPDIRECT3DDEVICE9 dev);
	virtual ~DX9VertexBuffer();

	void*			Lock(uint32_t begin, uint32_t n, LOCK lock);
	void			Unlock();

	void			UpdateData(void* data, uint32_t start, uint32_t length);
	void			UpdateData(ELEMENT_USAGE usg, uint32_t usg_index,  void* data, uint32_t start , uint32_t length);
	
	void			CopyData(void* out_data, uint32_t start, uint32_t length);
	void			CopyData(ELEMENT_USAGE usg, uint32_t usg_index, void* out_data, uint32_t start, uint32_t length);

	void			SetBuffer();
	void			UnsetBuffer();
private:
	LPDIRECT3DVERTEXDECLARATION9	mD3DDeclaration;
	LPDIRECT3DDEVICE9				mpD3DDevice;
	LPDIRECT3DVERTEXBUFFER9			mBuffer;
};

END_ENGINE_NAMESPACE

#endif
