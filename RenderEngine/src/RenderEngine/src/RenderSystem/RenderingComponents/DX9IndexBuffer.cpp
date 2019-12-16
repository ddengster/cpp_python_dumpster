
#include "RenderSystem/RenderingComponents/DX9IndexBuffer.h"
#include "RenderSystem/RenderingComponents/DX9HardwareBufferManager.h"

DECLARE_ENGINE_NAMESPACE

DX9IndexBuffer::DX9IndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT format, HARDWARE_BUFFER_USAGE usg, LPDIRECT3DDEVICE9 dev)
:mpD3DDevice(dev)
{
	mFormat = format;
	mIndexNumber = n;
	mBufferUsage = usg;

	if (mFormat == IDX_BUF_16)
		mIndexSize = 2;
	if (mFormat == IDX_BUF_32)
		mIndexSize = 4;

	DWORD	d3d_usg;
	D3DPOOL	d3d_pool;

	// There are usage to be added
	// Here are simply two common usages
	if (mBufferUsage == HDW_BUF_USG_STATIC)	// static, don't update very often
	{
		d3d_usg = 0;
		d3d_pool = D3DPOOL_MANAGED;
	}

	if (mBufferUsage == HDW_BUF_USG_DYNAMIC)	// dynamic, updated very often
	{
		d3d_usg = D3DUSAGE_DYNAMIC;
		d3d_pool = D3DPOOL_DEFAULT;
	}

	// Create d3d index buffer
	mpD3DDevice->CreateIndexBuffer(mIndexNumber*mIndexSize, d3d_usg, ((mFormat == IDX_BUF_16)? D3DFMT_INDEX16 : D3DFMT_INDEX32),
									d3d_pool, &mBuffer, NULL);
}

DX9IndexBuffer::~DX9IndexBuffer()
{
	SAFE_RELEASE(mBuffer);
}

void* DX9IndexBuffer::Lock(uint32_t begin, uint32_t n, LOCK lock)
{
	void* data;
	mBuffer->Lock(begin*mIndexSize, n*mIndexSize, &data, DX9HardwareBufferManager::_ConvertToD3DLock(lock));
	return data;
}

void DX9IndexBuffer::Unlock()
{
	mBuffer->Unlock();
}
	
void DX9IndexBuffer::UpdateData(void* data, uint32_t start, uint32_t length)
{
	void*	buffer_data;
	mBuffer->Lock(start*mIndexSize, length*mIndexSize, &buffer_data, (mBufferUsage == HDW_BUF_USG_DYNAMIC ? D3DLOCK_DISCARD : 0) );
	memcpy(buffer_data, data, length*mIndexSize);
	mBuffer->Unlock();
}

void DX9IndexBuffer::CopyData(void* out_data, uint32_t start, uint32_t length)
{
	void*	buffer_data;
	// read only
	mBuffer->Lock(start*mIndexSize, length*mIndexSize, &buffer_data, D3DLOCK_READONLY );
	memcpy(out_data, buffer_data, length*mIndexSize);
	mBuffer->Unlock();
}

void DX9IndexBuffer::SetBuffer()
{
	mpD3DDevice->SetIndices(mBuffer);
	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentIndexBuffer(this);
	HardwareBufferManager::GetInstance()->_RegisterCurrentIndexBuffer(this);
}
	
void DX9IndexBuffer::UnsetBuffer()
{
	mpD3DDevice->SetIndices(NULL);
	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentIndexBuffer(NULL);
	HardwareBufferManager::GetInstance()->_RegisterCurrentIndexBuffer(NULL);
}

END_ENGINE_NAMESPACE
