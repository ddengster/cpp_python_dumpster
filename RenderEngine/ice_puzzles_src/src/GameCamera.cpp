
#include "GameCamera.h"

GameCamera::GameCamera()
:m_position(Vector3(0,0,0)), m_dir(Vector3(0,0,0)), m_up(Vector3(0,1,0))
{
}

GameCamera::GameCamera(Vector3 pos, Vector3 dir, Vector3 up)
:m_position(pos), m_dir(dir), m_up(up)
{
}

GameCamera::~GameCamera()
{
}

void GameCamera::FinalizeCamera()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->LookAt(m_position, m_dir, m_up);
}
