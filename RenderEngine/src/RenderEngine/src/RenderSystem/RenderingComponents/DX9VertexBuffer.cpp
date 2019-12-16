
#include "RenderSystem/RenderingComponents/DX9VertexBuffer.h"
#include "RenderSystem/RenderingComponents/DX9HardwareBufferManager.h"

DECLARE_ENGINE_NAMESPACE

DX9VertexBuffer::DX9VertexBuffer(VertexDeclaration *vert_dec, uint32_t vertex_number, HARDWARE_BUFFER_USAGE usg, LPDIRECT3DDEVICE9 dev)
:mpD3DDevice(dev), mD3DDeclaration(NULL), mBuffer(NULL)
{
	mDeclaration = vert_dec;
	mVertexNumber = vertex_number;

	mBufferUsage = usg;

	mVertexStride = mDeclaration->GetVertexStride();

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

	mpD3DDevice->CreateVertexBuffer(mVertexStride*mVertexNumber, d3d_usg, 0, d3d_pool, &mBuffer, 0);

	// D3D vertex declaraction
	uint32_t	ele_num = mDeclaration->GetNumberOfElements();
	D3DVERTEXELEMENT9	*d3d_elements = new D3DVERTEXELEMENT9[ele_num + 1];
	
	uint32_t	offset = 0;

	for (uint32_t i=0; i<ele_num; ++i)
	{
		VertexElement	*ele;
		ele = mDeclaration->GetVertexElement(i);

		D3DVERTEXELEMENT9 temp_ele = {0, offset, DX9HardwareBufferManager::_ConvertToD3DElementType(ele->type),
			D3DDECLMETHOD_DEFAULT, DX9HardwareBufferManager::_ConvertToD3DElementUsage(ele->usage), ele->usage_index}; 

		memcpy(d3d_elements + i, &temp_ele, sizeof(D3DVERTEXELEMENT9));
		offset += mDeclaration->GetElementSize(i);
	}

	D3DVERTEXELEMENT9 temp_ele = D3DDECL_END();
	memcpy(d3d_elements + ele_num, &temp_ele, sizeof(D3DVERTEXELEMENT9));

	mpD3DDevice->CreateVertexDeclaration(d3d_elements, &mD3DDeclaration);

	delete	[]d3d_elements;
}

DX9VertexBuffer::~DX9VertexBuffer()
{
	SAFE_RELEASE(mD3DDeclaration);
	SAFE_RELEASE(mBuffer);
}

// "begin" means starting from the "begin"th vertex, "n" means "n" vertices will be locked.
void* DX9VertexBuffer::Lock(uint32_t begin, uint32_t n, LOCK lock)
{
	void* data;
	mBuffer->Lock(begin*mVertexStride, n*mVertexStride, &data, DX9HardwareBufferManager::_ConvertToD3DLock(lock));
	return data;
}

void DX9VertexBuffer::Unlock()
{
	mBuffer->Unlock();
}

void DX9VertexBuffer::UpdateData(void* data, uint32_t start, uint32_t length)
{
	// set is_discard to be true will cause the entire verter buffer to be discarded

	void* buffer_data;
	mBuffer->Lock(start*mVertexStride, length*mVertexStride, &buffer_data, (mBufferUsage == HDW_BUF_USG_DYNAMIC ? D3DLOCK_DISCARD : 0) );
	memcpy(buffer_data, data, length*mVertexStride);
	mBuffer->Unlock();
}

// suitable for update the data of only certain usage
void DX9VertexBuffer::UpdateData(ELEMENT_USAGE usg, uint32_t usg_index,  void* data, uint32_t start, uint32_t length)
{
	uint32_t	ele_index = mDeclaration->GetElementIndexByUsage(usg, usg_index);

	uint32_t	offset = 0;
	uint32_t	ele_size = 0;
	for (uint32_t i=0; i<ele_index; ++i)
	{
		ele_size = mDeclaration->GetElementSize(i); 
		offset += ele_size;
	}
	ele_size = mDeclaration->GetElementSize(ele_index);

	void* buffer_data;
	mBuffer->Lock(start*mVertexStride, length*mVertexStride, &buffer_data, 0);

	for (uint32_t j=0; j<length; ++j)
	{
		memcpy((BYTE*)buffer_data + j*mVertexStride + offset, (BYTE*)data + j*ele_size, ele_size); 
	}

	mBuffer->Unlock();
}

void DX9VertexBuffer::CopyData(void* out_data, uint32_t start, uint32_t length)
{
	void* buffer_data;
	mBuffer->Lock(start*mVertexStride, length*mVertexStride, &buffer_data, D3DLOCK_READONLY );
	memcpy(out_data, buffer_data, length*mVertexStride);
	mBuffer->Unlock();
}

void DX9VertexBuffer::CopyData(ELEMENT_USAGE usg, uint32_t usg_index, void* out_data, uint32_t start, uint32_t length)
{
	uint32_t	ele_index = mDeclaration->GetElementIndexByUsage(usg, usg_index);

	uint32_t	offset = 0;
	uint32_t	ele_size = 0;
	for (uint32_t i=0; i<ele_index; ++i)
	{
		ele_size = mDeclaration->GetElementSize(i); 
		offset += ele_size;
	}
	ele_size = mDeclaration->GetElementSize(ele_index);

	void* buffer_data;
	mBuffer->Lock(start*mVertexStride, length*mVertexStride, &buffer_data, D3DLOCK_READONLY  );

	for (uint32_t j=0; j<length; ++j)
	{
		memcpy((BYTE*)out_data + j*ele_size, (BYTE*)buffer_data + j*mVertexStride + offset,  ele_size); 
	}

	mBuffer->Unlock();
}

void DX9VertexBuffer::SetBuffer()
{
	mpD3DDevice->SetVertexDeclaration(mD3DDeclaration);
	mpD3DDevice->SetStreamSource(0, mBuffer, 0, mVertexStride);

	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentVertexBufferAndDeclaration(this, mDeclaration);
	HardwareBufferManager::GetInstance()->_RegisterCurrentVertexBufferAndDeclaration(this, mDeclaration);
}

void DX9VertexBuffer::UnsetBuffer()
{
	mpD3DDevice->SetStreamSource(0, NULL, 0, 0);
	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentVertexBufferAndDeclaration(NULL, NULL);
	HardwareBufferManager::GetInstance()->_RegisterCurrentVertexBufferAndDeclaration(NULL, NULL);
}

END_ENGINE_NAMESPACE