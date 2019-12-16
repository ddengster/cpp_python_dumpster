
#include "RenderSystem/RenderingComponents/DX9HardwareBufferManager.h"
#include "RenderSystem/RenderEngine.h"
#include "RenderSystem/RenderingComponents/DX9VertexBuffer.h"
#include "RenderSystem/RenderingComponents/DX9IndexBuffer.h"

#include "RenderSystem/RenderingComponents/DX9Renderer.h"

DECLARE_ENGINE_NAMESPACE

DX9HardwareBufferManager::DX9HardwareBufferManager()
{
	mpD3DDevice = ((DX9Renderer*)RenderEngine::GetInstance()->GetRenderer())->GetD3DDevice();
}

DX9HardwareBufferManager::~DX9HardwareBufferManager()
{
}

VertexBuffer* DX9HardwareBufferManager::GenerateVertexBuffer(VertexDeclaration* vert_dec, uint32_t n, HARDWARE_BUFFER_USAGE usg)
{
	VertexBuffer *buf = new DX9VertexBuffer(vert_dec, n, usg, mpD3DDevice);
	mVertexBuffers.push_back(buf);
	return buf;
}

IndexBuffer* DX9HardwareBufferManager::GenerateIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT ft, HARDWARE_BUFFER_USAGE usg)
{
	IndexBuffer *buf = new DX9IndexBuffer(n, ft, usg, mpD3DDevice);
	mIndexBuffers.push_back(buf);
	return	buf;
}

BYTE DX9HardwareBufferManager::_ConvertToD3DElementUsage(ELEMENT_USAGE usg)
{
	static BYTE table[NUM_ELEMENT_USAGES] = {	D3DDECLUSAGE_POSITION,
												D3DDECLUSAGE_NORMAL,
												D3DDECLUSAGE_COLOR,
												D3DDECLUSAGE_TEXCOORD
	};
	return table[usg];
}

BYTE DX9HardwareBufferManager::_ConvertToD3DElementType(ELEMENT_TYPE type)
{
	static BYTE table[NUM_ELEMENT_TYPES] = {	D3DDECLTYPE_FLOAT1,
												D3DDECLTYPE_FLOAT2,
												D3DDECLTYPE_FLOAT3,
												D3DDECLTYPE_FLOAT4,
												D3DDECLTYPE_UBYTE4
	};

	return table[type];
}

DWORD DX9HardwareBufferManager::_ConvertToD3DLock(LOCK lock)
{
	static DWORD table[NUM_LOCK] = {0, D3DLOCK_READONLY, D3DLOCK_DISCARD};
	return table[lock];
}

END_ENGINE_NAMESPACE
