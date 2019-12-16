
#include "RenderSystem/RenderingComponents/OGLHardwareBufferManager.h"
#include "RenderSystem/RenderingComponents/OGLVertexBuffer.h"
#include "RenderSystem/RenderingComponents/OGLIndexBuffer.h"

DECLARE_ENGINE_NAMESPACE

OGLHardwareBufferManager::OGLHardwareBufferManager()
{
}

OGLHardwareBufferManager::~OGLHardwareBufferManager()
{
}

VertexBuffer* OGLHardwareBufferManager::GenerateVertexBuffer(VertexDeclaration* vert_dec, uint32_t n, HARDWARE_BUFFER_USAGE usg)
{
	VertexBuffer *buf = new OGLVertexBuffer(vert_dec, n, usg);
	mVertexBuffers.push_back(buf);
	return buf;
}

IndexBuffer* OGLHardwareBufferManager::GenerateIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT ft, HARDWARE_BUFFER_USAGE usg)
{
	IndexBuffer	*buf = new OGLIndexBuffer(n, ft, usg);
	mIndexBuffers.push_back(buf);
	return buf;
}

GLenum OGLHardwareBufferManager::_ConvertToGLLock(LOCK lock)
{
	static GLenum table[NUM_LOCK] = { GL_READ_WRITE, GL_READ_ONLY, GL_WRITE_ONLY };
	return table[lock];
}

GLint OGLHardwareBufferManager::_ConvertToGLType(ELEMENT_TYPE type, GLenum &gl_typ)
{
	static GLint table1[NUM_ELEMENT_TYPES] = {1, 2, 3, 4, 4};
	static GLenum table2[NUM_ELEMENT_TYPES] = {GL_FLOAT, GL_FLOAT, GL_FLOAT, GL_FLOAT, GL_UNSIGNED_BYTE};

	gl_typ = table2[type];
	return table1[type];
}

END_ENGINE_NAMESPACE
