
#ifndef _GAMESTATE_SINGLEPLAYERMENU_H
#define _GAMESTATE_SINGLEPLAYERMENU_H

#include "GameState.h"

class GameStateManager;

class GameState_SinglePlayerMenu : public GameState
{
public:
	void	Init();
	void	Loop(float timeElapsed);
	void	OnExit();

protected:
	friend class GameStateManager;
	GameState_SinglePlayerMenu();
	~GameState_SinglePlayerMenu();
};

#endif