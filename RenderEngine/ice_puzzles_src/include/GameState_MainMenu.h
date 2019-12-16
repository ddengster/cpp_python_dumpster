
#ifndef _GAMESTATE_MAINMENU_H
#define _GAMESTATE_MAINMENU_H

#include "GameState.h"

class GameStateManager;
class UIInteractiveElement;

class GameState_MainMenu : public GameState
{
public:
	void	Init();
	void	Loop(float timeElapsed);
	void	OnExit();

	static	void SinglePlayerClickedCallback(UIInteractiveElement* interactive_ui);
	static	void QuitClickedCallback(UIInteractiveElement* interactive_ui);
	static	void SoundClickedCallback(UIInteractiveElement* interactive_ui);

	static	void SinglePlayerPopup_OKButtonClickedCallback(UIInteractiveElement* interactive_ui);
	static	void SinglePlayerPopup_CancelButtonClickedCallback(UIInteractiveElement* interactive_ui);
	static	void SinglePlayerPopup_TableClickedCallback(UIInteractiveElement* interactive_ui);

	static void	DisableMainMenuBtns();
	static void	EnableMainMenuBtns();
protected:
	friend class GameStateManager;
	GameState_MainMenu();
	~GameState_MainMenu();
};

#endif