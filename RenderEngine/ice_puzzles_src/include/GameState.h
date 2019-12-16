
#ifndef _GAMESTATE_H
#define _GAMESTATE_H

//#include "GameStateManager.h"
enum GAMESTATE;

class GameState
{
public:
	virtual void	Init()						= 0;
	virtual void	Loop(float timeElapsed)		= 0;
	virtual void	OnExit()					= 0;

	GameState()
	{
	}

	virtual ~GameState()
	{
	}
};

#endif