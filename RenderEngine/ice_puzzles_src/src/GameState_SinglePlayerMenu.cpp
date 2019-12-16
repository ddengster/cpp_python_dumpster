
#include "GameState_SinglePlayerMenu.h"
#include "GameStateManager.h"

GameState_SinglePlayerMenu::GameState_SinglePlayerMenu()
:GameState()
{
}

GameState_SinglePlayerMenu::~GameState_SinglePlayerMenu()
{
}

void GameState_SinglePlayerMenu::Init()
{
	GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_GAMEPLAY);
}

void GameState_SinglePlayerMenu::Loop(float timeElapsed)
{
}

void GameState_SinglePlayerMenu::OnExit()
{
}

