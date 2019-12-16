
#ifndef _DX9RENDERER_H
#define _DX9RENDERER_H

#include "RenderSystem/RenderingComponents/IRenderer.h"
#include <d3d9.h>
#include <d3dx9.h>

DECLARE_ENGINE_NAMESPACE

class DX9Renderer : public IRenderer
{
	INTERFACE_RENDERER(;)
	virtual ~DX9Renderer();

	LPDIRECT3DDEVICE9 GetD3DDevice() const { return mpD3DDevice; }

protected:
	friend class RenderEngine;
	DX9Renderer();

	static	D3DPRIMITIVETYPE	_ConvertToD3DPrimitiveType(PRIMITIVE_TYPE type);
	static	void	_ConvertD3DMatrixToEngineMatrix(Matrix4* out, const D3DXMATRIX* in);
	static	void	_ConvertEngineMatrixToD3DMatrix(D3DXMATRIX* out, const Matrix4* in);

	void CheckDeviceCaps();
	bool CheckDevice();
	
private:
	HWND					mWindowHandle;
	LPDIRECT3DDEVICE9		mpD3DDevice;
	D3DCAPS9				mDeviceCaps;
};

END_ENGINE_NAMESPACE

#endif