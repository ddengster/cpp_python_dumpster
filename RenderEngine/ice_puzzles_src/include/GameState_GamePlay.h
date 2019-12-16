

#ifndef _GAMESTATE_GAMEPLAY_H
#define _GAMESTATE_GAMEPLAY_H

#include "GameState.h"

class GameStateManager;
class GameWorld;
class UIInteractiveElement;

class GameState_GamePlay : public GameState
{
public:
	void	Init();
	void	Loop(float timeElapsed);
	void	OnExit();

	void	InputLoop();

	void	CameraInputControl();

	static	void	RestartClickedCallback(UIInteractiveElement* interactive_ui);
	static	void	MainMenuClickedCallback(UIInteractiveElement* interactive_ui);
	static	void	CloseTutorialPopup(UIInteractiveElement* interactive_ui);
	static	void	CloseErrorPopup(UIInteractiveElement* interactive_ui);
	void	Restart();

	void	GameOver(bool win);
	void	CheckGameOverGUI();

protected:
	friend class GameStateManager;
	GameState_GamePlay();
	~GameState_GamePlay();

	GameWorld	*m_world;
	float		m_timetaken;
	bool		m_gameover;
	bool		m_win;
};

#endif