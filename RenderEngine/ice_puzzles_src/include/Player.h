
#ifndef _PLAYER_H
#define _PLAYER_H

#include "Headers.h"
#include "GameWorld.h"

using namespace ddengine_RenderEngine;

enum PLAYER_STATE
{
	PLAYER_STATE_IDLE = 0,
	PLAYER_STATE_MOVING,
	PLAYER_STATE_MAX
};

class Player
{
public:
	Player(CellLoc loc);
	virtual ~Player();

	void		SetLocation(CellLoc loc);
	CellLoc&	GetLocation();
	void		SetMoveToLocation(CellLoc loc);

	void		Update(float timeElapsed);
	void		Render();

	void			SetPlayerState(PLAYER_STATE state)		{	m_state = state;	}
	PLAYER_STATE	GetPlayerState()						{	return m_state;		}

	static void	SetUpRenderingObjects();

protected:
	CellLoc m_location;
	CellLoc m_moveto;

	Vector3 m_worldposition;
	Vector3 moveto_worldposition;

	PLAYER_STATE m_state;
};

#endif
