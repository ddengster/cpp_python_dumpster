
#include "AppMain.h"
#include "GameStateManager.h"
#include "GUI.h"
#include "SoundManager.h"

using namespace ddengine_RenderEngine;

AppMain::AppMain()
{
}

AppMain::~AppMain()
{
}

AppMain* AppMain::GetInstance()
{
	static AppMain app;
	return &app;
}

void AppMain::Init(RenderSetup rs)
{
	RenderEngine::GetInstance()->CreateRenderSystem(rs);
	RenderEngine::GetInstance()->GetRenderer()->Init();
	GUI::GetInstance()->Initialize();
	SoundManager::GetInstance()->Init();

	GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_MAINMENU);
}

void AppMain::Shutdown()
{
	//RenderEngine::GetInstance()->GetRenderer()->Shutdown();
	GUI::GetInstance()->Destroy();
	GameStateManager::GetInstance()->Shutdown();
}

void AppMain::Loop(float timeElapsed)
{
	GameStateManager::GetInstance()->Loop(timeElapsed);
}

