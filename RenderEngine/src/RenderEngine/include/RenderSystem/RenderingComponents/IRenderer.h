
#ifndef _IRENDERER_H
#define _IRENDERER_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include "Math/Matrix4.h"

DECLARE_ENGINE_NAMESPACE

class IRenderer
{
#define INTERFACE_RENDERER(terminal)				\
	public:											\
	virtual	bool	Init()		## terminal			\
	virtual void	Shutdown()	## terminal			\
													\
	virtual void	StartRendering() ## terminal	\
	virtual void	EndRendering()	## terminal		\
	virtual void	ClearFrame(uint32_t target, Real r, Real g, Real b, Real a, Real z, uint32_t s) ## terminal	\
													\
	virtual bool	SetRenderState(RENDERSTATE rs, uint32_t value) ## terminal	\
													\
	virtual	void	SetViewPort(Real x, Real y, Real width, Real height) ## terminal \
	virtual	void	SetPerspective(Real fov, Real nearDist, Real farDist, int32_t width, int32_t height)  ## terminal \
	virtual void	SetOrtho(int32_t left, int32_t right, int32_t bottom, int32_t top, Real nearDist, Real farDist) ## terminal \
	virtual	void	LookAt(const Vector3& pos, const Vector3& lookat, const Vector3& up) ## terminal \
													\
	virtual	void	GetMatrix(MATRIX_TYPE type, Matrix4 *matrix) ## terminal \
	virtual	void	SetMatrix(MATRIX_TYPE type, const Matrix4& matrix) ## terminal \
													\
	virtual void	DrawPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount) ## terminal \
	virtual	void	DrawIndexedPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount) ## terminal 
													
	//virtual void	SetLight(uint32_t index, SceneLight* light) ## terminal \
	//virtual void	SetMaterial(Material* basic_mat) ## terminal 

	INTERFACE_RENDERER( =0;)

	IRenderer()
	{
	}
	virtual ~IRenderer()
	{
	}
};

END_ENGINE_NAMESPACE

#endif
