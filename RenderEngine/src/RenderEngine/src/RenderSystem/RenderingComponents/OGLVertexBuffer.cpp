
#include "RenderSystem/RenderingComponents/OGLVertexBuffer.h"
#include "RenderSystem/RenderingComponents/OGLHardwareBufferManager.h"

DECLARE_ENGINE_NAMESPACE

OGLVertexBuffer::OGLVertexBuffer(VertexDeclaration* vert_dec, uint32_t vertex_number, HARDWARE_BUFFER_USAGE usg)
{
	mDeclaration = vert_dec;
	mVertexNumber = vertex_number;
	mBufferUsage = usg;
	mVertexStride = mDeclaration->GetVertexStride();

	glGenBuffers(1, &mVertexBufferID);

	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	GLenum	gl_usg = 0;
	if (mBufferUsage == HDW_BUF_USG_STATIC)
		gl_usg = GL_STATIC_DRAW;

	if(mBufferUsage == HDW_BUF_USG_DYNAMIC)
		gl_usg = GL_DYNAMIC_DRAW;

	glBufferData(GL_ARRAY_BUFFER, mVertexStride*mVertexNumber, NULL, gl_usg);

	uint32_t ele_num = mDeclaration->GetNumberOfElements();

	mElementOffsets = new GLbyte[ele_num];

	for (uint32_t i=0; i<ele_num; ++i)
	{
		mElementOffsets[i] = 0;
	}

	for (uint32_t i=1; i<ele_num; ++i)
	{
		mElementOffsets[i] = mElementOffsets[i-1] + mDeclaration->GetElementSize(i-1);
	}

}

OGLVertexBuffer::~OGLVertexBuffer()
{
	SAFE_DELETE(mElementOffsets);
	glDeleteBuffers(1, &mVertexBufferID);
}

void* OGLVertexBuffer::Lock(uint32_t begin, uint32_t n, LOCK lock)
{
	//TODO: excessive parameters??
	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	BYTE	*data = (BYTE*)glMapBuffer(GL_ARRAY_BUFFER, OGLHardwareBufferManager::_ConvertToGLLock(lock));

	data += begin*mVertexStride;

	return	(void*)data;
}

void OGLVertexBuffer::Unlock()
{
	glUnmapBuffer(GL_ARRAY_BUFFER);
}

void OGLVertexBuffer::UpdateData(void* data, uint32_t start, uint32_t length)
{
	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, start*mVertexStride, length*mVertexStride, data);
}

void OGLVertexBuffer::UpdateData(ELEMENT_USAGE usg, uint32_t usg_index,  void* data, uint32_t start, uint32_t length)
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

	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	for (uint32_t j = 0; j<length; ++j)
		glBufferSubData(GL_ARRAY_BUFFER, (start+j)*mVertexStride + offset, ele_size, (BYTE*)data + j*ele_size);
}

void OGLVertexBuffer::CopyData(void* out_data, uint32_t start, uint32_t length)
{
	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	BYTE	*data = (BYTE*)glMapBuffer(GL_ARRAY_BUFFER, GL_READ_ONLY);

	memcpy(out_data, data + start*mVertexStride, length*mVertexStride);

	glUnmapBuffer(GL_ARRAY_BUFFER);
}

void OGLVertexBuffer::CopyData(ELEMENT_USAGE usg, uint32_t usg_index, void* out_data, uint32_t start, uint32_t length)
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

	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	BYTE	*data = (BYTE*)glMapBuffer(GL_ARRAY_BUFFER, GL_READ_ONLY);

	for (uint32_t j=0; j<length; ++j)
		memcpy((BYTE*)out_data + j*ele_size, data + (start + j)*mVertexStride + offset, ele_size); 

	glUnmapBuffer(GL_ARRAY_BUFFER);
}

void OGLVertexBuffer::SetBuffer()
{
	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);
		/*glEnableClientState(GL_COLOR_ARRAY);
		glColorPointer(4, GL_UNSIGNED_BYTE, stride, ptrVerts + currentOffset);
		currentOffset += sizeof(uint8_t) * 4;*/
	GLint	s;
	GLenum	t;
	for (uint32_t i=0; i<mDeclaration->GetNumberOfElements(); ++i)
	{
		VertexElement* ele = mDeclaration->GetVertexElement(i);
		
		s = OGLHardwareBufferManager::_ConvertToGLType(ele->type, t);

		switch (ele->usage)
		{
		case ELE_USG_POSITION:
			{
				glEnableClientState(GL_VERTEX_ARRAY);
				glVertexPointer(s, t, mVertexStride, (void*)mElementOffsets[i]);
				break;
			}
		case ELE_USG_NORMAL:
			{
				glEnableClientState(GL_NORMAL_ARRAY);
				glNormalPointer(t, mVertexStride, (void*)mElementOffsets[i]);
				break;
			}
		case ELE_USG_COLOR:
			{
				glEnableClientState(GL_COLOR_ARRAY);
				glColorPointer(s, t, mVertexStride, (void*)mElementOffsets[i]);
				break;
			}
		case ELE_USG_TEXCOORD:
			{
				glClientActiveTexture(GL_TEXTURE0 + ele->usage_index);
				glEnableClientState(GL_TEXTURE_COORD_ARRAY);
				glTexCoordPointer(s, t, mVertexStride, (void*)mElementOffsets[i]);
				break;
			}
		}
	}

	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentVertexBufferAndDeclaration(this, mDeclaration);
	HardwareBufferManager::GetInstance()->_RegisterCurrentVertexBufferAndDeclaration(this, mDeclaration);
}

void OGLVertexBuffer::UnsetBuffer()
{
//	glBindBuffer(GL_ARRAY_BUFFER, mVertexBufferID);

	for (uint32_t i = 0; i<mDeclaration->GetNumberOfElements(); ++i)
	{
		VertexElement* ele = mDeclaration->GetVertexElement(i);

		switch (ele->usage)
		{
		case ELE_USG_POSITION:
			{
				glDisableClientState(GL_VERTEX_ARRAY);
				break;
			}
		case ELE_USG_NORMAL:
			{
				glDisableClientState(GL_NORMAL_ARRAY);
				break;
			}
		case ELE_USG_COLOR:
			{
				glDisableClientState(GL_COLOR_ARRAY);
				break;
			}
		case ELE_USG_TEXCOORD:
			{
				glClientActiveTexture(GL_TEXTURE0 + ele->usage_index);
				glDisableClientState(GL_TEXTURE_COORD_ARRAY);
				break;
			}
		}
	}
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	//RenderEngine::GetInstance()->GetHardwareBufferManager()->_RegisterCurrentVertexBufferAndDeclaration(NULL, NULL);
	HardwareBufferManager::GetInstance()->_RegisterCurrentVertexBufferAndDeclaration(NULL, NULL);
}

END_ENGINE_NAMESPACE
