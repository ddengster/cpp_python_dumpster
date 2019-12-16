
#include "GameStateManager.h"

#include "GameState.h"
#include "GameState_MainMenu.h"
#include "GameState_SinglePlayerMenu.h"
#include "GameState_GamePlay.h"

#include <stdio.h>

GameStateManager::GameStateManager()
: m_current_GameState(NULL), m_current_GameState_ID(GAMESTATE_NULL), m_new_GameState_ID(GAMESTATE_NULL)
{
	
}

GameStateManager::~GameStateManager()
{
	
}

GameStateManager* GameStateManager::GetInstance()
{
	static GameStateManager mgr;
	return &mgr;
}

void GameStateManager::ChangeGameState(GAMESTATE new_GameState_ID)
{
	m_new_GameState_ID = new_GameState_ID;
}

void GameStateManager::Loop(float timeElapsed)
{
	if (m_new_GameState_ID != m_current_GameState_ID)
	{
		m_current_GameState_ID = m_new_GameState_ID;
		if (m_current_GameState)
		{
			m_current_GameState->OnExit();
			delete m_current_GameState;
		}

		switch (m_current_GameState_ID)
		{
			case GAMESTATE_MAINMENU:
			{
				m_current_GameState = new GameState_MainMenu();
				m_current_GameState->Init();
			}
			break;
			case GAMESTATE_SINGLEPLAYERMENU:
			{
				m_current_GameState = new GameState_SinglePlayerMenu();
				m_current_GameState->Init();
			}
			break;
			case GAMESTATE_GAMEPLAY:
			{
				m_current_GameState = new GameState_GamePlay();
				m_current_GameState->Init();
			}
			break;
			default:
				//TODO
				break;
		}
	}
	else //put this else block so that problems wont occur if changegamestate is called during init
		m_current_GameState->Loop(timeElapsed);
}

void GameStateManager::Shutdown()
{
	if (m_current_GameState)
	{
		m_current_GameState->OnExit();
		delete m_current_GameState;
		m_current_GameState_ID = GAMESTATE_NULL;
	}
}

