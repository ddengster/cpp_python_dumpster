
#ifndef _DX9HARDWAREBUFFERMANAGER_H
#define _DX9HARDWAREBUFFERMANAGER_H

#include "Prerequisites.h"
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class DX9HardwareBufferManager : public HardwareBufferManager
{
private:
	LPDIRECT3DDEVICE9	mpD3DDevice;

public:
	DX9HardwareBufferManager();
	virtual ~DX9HardwareBufferManager();

	VertexBuffer* GenerateVertexBuffer(VertexDeclaration* vert_dec, uint32_t n, HARDWARE_BUFFER_USAGE usg);
	IndexBuffer*  GenerateIndexBuffer(uint32_t n, INDEX_BUFFER_FORMAT ft, HARDWARE_BUFFER_USAGE usg);

	static	BYTE	_ConvertToD3DElementUsage(ELEMENT_USAGE usg);
	static	BYTE	_ConvertToD3DElementType(ELEMENT_TYPE type);
	static	DWORD	_ConvertToD3DLock(LOCK lock);
};



END_ENGINE_NAMESPACE

#endif
