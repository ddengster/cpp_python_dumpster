

#include "RenderSystem/RenderingComponents/OGLIndexBuffer.h"
#include "RenderSystem/RenderingComponents/OGLHardwareBufferManager.h"

DECLARE_ENGINE_NAMESPACE

OGLIndexBuffer::OGLIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT format, HARDWARE_BUFFER_USAGE usg)
{
	mFormat = format;
	mIndexNumber = n;
	mBufferUsage = usg;

	if (mFormat == IDX_BUF_16)
		mIndexSize = 2;
	if (mFormat == IDX_BUF_32)
		mIndexSize = 4;

	glGenBuffers(1, &mIndexBufferID);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mIndexBufferID);

	GLenum	gl_usg = 0;
	if (mBufferUsage == HDW_BUF_USG_STATIC)
		gl_usg = GL_STATIC_DRAW;

	if (mBufferUsage == HDW_BUF_USG_DYNAMIC)
		gl_usg = GL_DYNAMIC_DRAW;

	glBufferData(GL_ELEMENT_ARRAY_BUFFER, mIndexNumber*mIndexSize, NULL, gl_usg);
}

OGLIndexBuffer::~OGLIndexBuffer()
{
	glDeleteBuffers(1, &mIndexBufferID);
}

void* OGLIndexBuffer::Lock(uint32_t begin, uint32_t n, LOCK lock)
{
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mIndexBufferID);

	BYTE*	data = (BYTE*)glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, OGLHardwareBufferManager::_ConvertToGLLock(lock));

	data += begin*mIndexSize;

	return data;
}

void OGLIndexBuffer::Unlock()
{
	glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
}

void OGLIndexBuffer::UpdateData(void* data, uint32_t start, uint32_t length)
{
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mIndexBufferID);
	glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, start*mIndexSize, length*mIndexSize, data);
}

void OGLIndexBuffer::CopyData(void* out_data, uint32_t start, uint32_t length)
{
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mIndexBufferID);
	
	BYTE*	data = (BYTE*)glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, GL_READ_ONLY);

	memcpy(out_data, data + start*mIndexSize, length*mIndexSize);

	glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
}

void OGLIndexBuffer::SetBuffer()
{
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mIndexBufferID);
	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentIndexBuffer(this);
	HardwareBufferManager::GetInstance()->_RegisterCurrentIndexBuffer(this);
}

void OGLIndexBuffer::UnsetBuffer()
{
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentIndexBuffer(NULL);
	HardwareBufferManager::GetInstance()->_RegisterCurrentIndexBuffer(NULL);
}

END_ENGINE_NAMESPACE
