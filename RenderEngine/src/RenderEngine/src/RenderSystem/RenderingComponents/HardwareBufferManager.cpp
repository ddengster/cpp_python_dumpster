
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"
#include "RenderSystem/RenderEngine.h"
#include <algorithm>

DECLARE_ENGINE_NAMESPACE

HardwareBufferManager* HardwareBufferManager::GetInstance()
{
	return RenderEngine::GetHardwareBufferManager();
}

HardwareBufferManager::HardwareBufferManager()
{
	mVertexBuffers.clear();
	mVertexDeclarations.clear();
	mIndexBuffers.clear();

	mCurrentVertexBuffer = NULL;
	mCurrentIndexBuffer = NULL;
	mCurrentDeclaration = NULL;
}

HardwareBufferManager::~HardwareBufferManager()
{
	for (uint32_t i=0; i<(uint32_t)mVertexBuffers.size(); ++i)
	{
		SAFE_DELETE(mVertexBuffers[i]);
	}
	for (uint32_t i=0; i<(uint32_t)mVertexDeclarations.size(); ++i)
	{
		SAFE_DELETE(mVertexDeclarations[i]);
	}
	for (uint32_t i=0; i<(uint32_t)mIndexBuffers.size(); ++i)
	{
		SAFE_DELETE(mIndexBuffers[i]);
	}
	/*for (std::vector<VertexBuffer*>::iterator iter = mVertexBuffers.begin(); iter != mVertexBuffers.end(); ++iter)
		SAFE_DELETE(*iter);

	for (std::vector<VertexDeclaration*>::iterator dec_iter = mVertexDeclarations.begin(); dec_iter != mVertexDeclarations.end(); ++dec_iter)
		SAFE_DELETE((*dec_iter));

	for (std::vector<IndexBuffer*>::iterator idx_iter = mIndexBuffers.begin(); idx_iter != mIndexBuffers.end(); ++idx_iter)
		SAFE_DELETE((*idx_iter));*/
}

VertexDeclaration* HardwareBufferManager::GenerateVertexDeclaration()
{
	VertexDeclaration* vert_dec = new VertexDeclaration();
	mVertexDeclarations.push_back(vert_dec);

	return vert_dec;
}

void HardwareBufferManager::DestroyVertexDeclaration(VertexDeclaration *vert_dec)
{
	std::vector<VertexDeclaration*>::iterator iter = std::find(mVertexDeclarations.begin(), mVertexDeclarations.end(), vert_dec);

	if (iter != mVertexDeclarations.end())
	{
		SAFE_DELETE(*iter);
		mVertexDeclarations.erase(iter);
	}
}

void HardwareBufferManager::DestroyAllVertexDeclaration()
{
	std::vector<VertexDeclaration*>::iterator iter = mVertexDeclarations.begin();

	for ( ; iter!=mVertexDeclarations.end(); ++iter )
	{
		SAFE_DELETE(*iter);
	}
	mVertexDeclarations.clear();
}

void HardwareBufferManager::DestroyVertexBuffer(VertexBuffer *buffer)
{
	std::vector<VertexBuffer*>::iterator iter = std::find(mVertexBuffers.begin(), mVertexBuffers.end(), buffer);

	if (iter != mVertexBuffers.end())
	{
		SAFE_DELETE(*iter);
		mVertexBuffers.erase(iter);
	}
}

void HardwareBufferManager::DestroyAllVertexBuffer()
{
	std::vector<VertexBuffer*>::iterator iter = mVertexBuffers.begin();
	for (; iter != mVertexBuffers.end(); ++iter)
	{
		SAFE_DELETE(*iter);
	}
	mVertexBuffers.clear();
}

void HardwareBufferManager::DestroyIndexBuffer(IndexBuffer *buffer)
{
	std::vector<IndexBuffer*>::iterator iter = std::find(mIndexBuffers.begin(), mIndexBuffers.end(), buffer);

	if (iter != mIndexBuffers.end())
	{
		SAFE_DELETE(*iter);
		mIndexBuffers.erase(iter);
	}
}

void HardwareBufferManager::DestroyAllIndexBuffer()
{
	/*for(std::vector<IndexBuffer*>::iterator idx_iter = mIndexBuffers.begin(); idx_iter != mIndexBuffers.end(); ++idx_iter)
		SAFE_DELETE((*idx_iter));*/
	for (uint32_t i=0; i<(uint32_t)mIndexBuffers.size(); ++i)
		delete mIndexBuffers[i];
	mIndexBuffers.clear();
}

END_ENGINE_NAMESPACE