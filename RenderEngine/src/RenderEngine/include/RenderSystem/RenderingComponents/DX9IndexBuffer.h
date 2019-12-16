
#ifndef _DX9INDEXBUFFER_H
#define _DX9INDEXBUFFER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/IndexBuffer.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

// Implementation for DirectX
class DX9IndexBuffer : public IndexBuffer
{
public:
	// Constructor should be called by DX9HardwareBufferManager
	DX9IndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT format, HARDWARE_BUFFER_USAGE usg, LPDIRECT3DDEVICE9 dev);
	virtual ~DX9IndexBuffer();

	void*	Lock(uint32_t begin, uint32_t n, LOCK lock);
	void	Unlock();

	void	UpdateData(void* data, uint32_t start, uint32_t length);
	void	CopyData(void* out_data, uint32_t start, uint32_t length);

	void	SetBuffer();
	void	UnsetBuffer();

private:
	LPDIRECT3DDEVICE9		mpD3DDevice;
	LPDIRECT3DINDEXBUFFER9	mBuffer;
};

END_ENGINE_NAMESPACE

#endif