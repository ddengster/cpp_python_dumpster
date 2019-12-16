
#ifndef _GAMECAMERA_H
#define _GAMECAMERA_H

#include "Headers.h"

using namespace ddengine_RenderEngine;
class GameCamera
{
public:
	GameCamera();
	GameCamera(Vector3 pos, Vector3 dir, Vector3 up);

	virtual ~GameCamera();

	Vector3& GetPosition()			{	return m_position;	}
	Vector3& GetDirection()			{	return m_dir;		}
	Vector3& GetUp()				{	return m_up;		}

	void	SetPosition(Vector3 pos)			{	m_position = pos;	}
	void	SetDirection(Vector3 dir)			{	m_dir = dir;		}
	void	SetUp(Vector3 up)					{	m_up = up;			}

	void	FinalizeCamera();
protected:
	Vector3 m_position;
	Vector3 m_dir;
	Vector3 m_up;
};

#endif
