
#include "GameState_GamePlay.h"

#include "Headers.h"
#include "GameStateManager.h"
#include "GameWorld.h"
#include "GameCamera.h"
#include "InputMgr.h"
#include "Player.h"

#include "GUI.h"
#include "UIButton.h"
#include "UITextBox.h"
#include "UIPopup.h"

#include "SoundManager.h"

using namespace ddengine_RenderEngine;

GameState_GamePlay::GameState_GamePlay()
:GameState(), m_world(NULL), m_timetaken(0.0), m_gameover(false), m_win(false)
{
}

GameState_GamePlay::~GameState_GamePlay()
{
	SAFE_DELETE(m_world);
}

void GameState_GamePlay::Init()
{
	//m_camera = new GameCamera(Vector3(25, 20, 75), Vector3(0, -3, -6), Vector3(0, 1, 0));
	//m_camera = new GameCamera(Vector3(4, 10, -20), Vector3(0, -3, 6), Vector3(0, 1, 0));
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->SetViewPort(0, 0, 800, 600);
	renderer->SetPerspective(80.0f, 0.1f, 1000.0f, 800, 600);
	renderer->SetRenderState(RS_DEPTH_TESTING, DEPTHMODE_ENABLE); //fixes overlapping issues!
	
	m_gameover = false;
	m_win = false;

	GUI* gui = GUI::GetInstance();
	gui->SetBackground("crackedicebg2.jpg","./data/textures/", 650, 150, 600);
	UITextBox* tbox = gui->AddTextBox("mapnametext_textbox", "Map Name:", ScreenLoc(660, 60), 24);
	string fullmapdir = GameStateManager::GetInstance()->GetCurrentMap();
	string displayedmapname = fullmapdir.erase(0, 15); //15 = sizeof(.//data//maps//)
	gui->AddTextBox("mapname_textbox", displayedmapname, ScreenLoc(670, 85), 24);

	gui->AddTextBox("timetakentext_textbox", "Time Taken:", ScreenLoc(660, 140), 24)->SetFontColor(0, 0, 0, 1.0);
	gui->AddTextBox("timetaken_textbox", "0", ScreenLoc(675, 165), 24);

	UIButton* button;
	button = GUI::GetInstance()->AddButton("Restart_button", "Restart", 
									RestartClickedCallback, ScreenLoc(660,450), 128, 36);
	button->SetFontSize(24);
	button = GUI::GetInstance()->AddButton("MainMenu_button", "Main Menu", 
									MainMenuClickedCallback, ScreenLoc(660,510), 128, 36);
	button->SetFontSize(24);

	m_world = new GameWorld();
	string errormsg;
	if(!m_world->InitializeFromFile(GameStateManager::GetInstance()->GetCurrentMap(), &errormsg))
	{
		UIPopup* popup = gui->GeneratePopup(488, 256);
		popup->AddTextBox("ErrorMsg_textbox", errormsg, ScreenLoc(20, 40), 24);
		popup->AddButton("error_popup_ok_btn", "OK", CloseErrorPopup, ScreenLoc(180, 190), 100, 48);
		SAFE_DELETE(m_world);
		return;
	}

	//add tutorial for tutorial.ipm
	if (GameStateManager::GetInstance()->GetCurrentMap() == ".//data//maps//tutorial.ipm")
	{
		UIPopup* popup = gui->GeneratePopup(488, 256);
		string tutorial_l1_text("\n ");
		popup->AddTextBox("tutorial_popup_text", "Get to the exit while avoiding the pits", ScreenLoc(20, 40), 24);
		popup->AddTextBox("tutorial_popup_text2", "and navigating the frozen walkways!", ScreenLoc(20, 65), 24);
		popup->AddTextBox("tutorial_popup_text3", "Use WASD to move around. ", ScreenLoc(20, 95), 24);
		popup->AddTextBox("tutorial_popup_text4", "You can control your movement on ", ScreenLoc(20, 125), 24);
		popup->AddTextBox("tutorial_popup_text5", "the rocky pathways, but you will have to", ScreenLoc(20, 150), 24); 
		popup->AddTextBox("tutorial_popup_text6", "slide through the ice paths.", ScreenLoc(20, 175), 24);
		popup->AddButton("tutorial_popup_ok_btn", "OK", CloseTutorialPopup, ScreenLoc(180, 190), 100, 48);
	}
	
	SoundManager::GetInstance()->SetBGM(".//data//sounds//gameplay_bgm.mp3");
}

void GameState_GamePlay::Loop(float timeElapsed)
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	
	m_timetaken += timeElapsed;

	UITextBox* timetaken_textbox = (UITextBox*)GUI::GetInstance()->GetUIElement("timetaken_textbox");

	if (timetaken_textbox)
	{
		int minutes = (int)m_timetaken/60;
		float seconds = m_timetaken - minutes*60.0;

		char converted_seconds[16];
		sprintf_s(converted_seconds, "%.4g", seconds);

		string time;
		char converted_minutes[8];
		itoa(minutes, converted_minutes, 10);
		time.append(converted_minutes);
		if (seconds < 10.0)
			time.append(":0");
		else
			time.append(":");
		time.append(string(converted_seconds));
		timetaken_textbox->SetText(time);
	}

	InputLoop();
	if (m_world)
		m_world->CheckWin();

	renderer->StartRendering();
	renderer->ClearFrame(CLEAR_TARGET | CLEAR_ZBUFFER | CLEAR_STENCIL ,
								 0.0, 0.0, 0.0, 1.0, 1.0, 0);
	
	renderer->SetPerspective(80.0f, 0.1f, 1000.0f, 800, 600);
	if (m_world)
		m_world->Update(timeElapsed);
	
	GUI::GetInstance()->Loop(timeElapsed);
	CheckGameOverGUI();
	renderer->EndRendering();

	if (InputMgr::GetInputMgr()->CheckKeyDown(VK_F1))//F1 key
		GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_MAINMENU);
}

void GameState_GamePlay::OnExit()
{
	SAFE_DELETE(m_world);
	GUI::GetInstance()->Destroy();
	m_timetaken = 0.0;
}

#define W_KEY 0x57
#define S_KEY 0x53
#define A_KEY 0x41
#define D_KEY 0x44
#define R_KEY 0x52
#define F_KEY 0x46
#define Q_KEY 0x51
#define E_KEY 0x45

void GameState_GamePlay::InputLoop()
{
	if (!m_world)
		return;
	CameraInputControl();
	static int pausecounter = 0;
	if (pausecounter < 50 && pausecounter)
	{
		++pausecounter;
		return;
	}
	else if (pausecounter >= 50)
		pausecounter = 0;
	CellLoc loc = m_world->GetPlayer()->GetLocation();
	if (InputMgr::GetInputMgr()->CheckKeyDown(S_KEY))
	{
		m_world->SetPlayerMoveDirection(PLAYER_DOWN);
		++pausecounter;
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(W_KEY))
	{
		m_world->SetPlayerMoveDirection(PLAYER_UP);
		++pausecounter;
	}	
	else if (InputMgr::GetInputMgr()->CheckKeyDown(D_KEY))
	{
		m_world->SetPlayerMoveDirection(PLAYER_RIGHT);
		++pausecounter;
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(A_KEY))
	{
		m_world->SetPlayerMoveDirection(PLAYER_LEFT);
		++pausecounter;
	}
}

void GameState_GamePlay::CameraInputControl()
{
	GameCamera* camera = m_world->GetCamera();
	if (InputMgr::GetInputMgr()->CheckKeyDown(VK_UP))
	{
		camera->SetPosition(camera->GetPosition()-Vector3(0,0,0.3));
		//m_camera->SetDirection(m_camera->GetDirection()-Vector3(0,0,0.3));
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(VK_DOWN))
	{
		camera->SetPosition(camera->GetPosition()+Vector3(0,0,0.3));
		//m_camera->SetDirection(m_camera->GetDirection()+Vector3(0,0,0.3));
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(VK_LEFT))
	{
		Matrix3 m(	1,0,0,
					0,1,0,
					0,0,1);
		m.FromAxisAngle(Vector3(0,1,0), Degree(1.0).ConvertToRadian());
		Vector3 vec = m * camera->GetDirection();
		camera->SetDirection(vec);
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(VK_RIGHT))
	{
		Matrix3 m(	1,0,0,
					0,1,0,
					0,0,1);
		m.FromAxisAngle(Vector3(0,1,0), Degree(-1.0).ConvertToRadian());
		Vector3 vec = m * camera->GetDirection();
		camera->SetDirection(vec);
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(VK_PRIOR)) //pageup
	{
		camera->SetPosition(camera->GetPosition()+Vector3(0,0.3,0));
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(VK_NEXT)) //pagedown
	{
		camera->SetPosition(camera->GetPosition()-Vector3(0,0.3,0));
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(Q_KEY))
	{
		camera->SetPosition(camera->GetPosition()-Vector3(0.3,0,0));
	}
	else if (InputMgr::GetInputMgr()->CheckKeyDown(E_KEY))
	{
		camera->SetPosition(camera->GetPosition()+Vector3(0.3,0,0));
	}
	camera->FinalizeCamera();
}

void GameState_GamePlay::Restart()
{
	SAFE_DELETE(m_world);
	m_world = new GameWorld();
	m_gameover = false;
	m_win = false;
	if(!m_world->InitializeFromFile(GameStateManager::GetInstance()->GetCurrentMap(), NULL))
		GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_MAINMENU);
	m_timetaken = 0.0;
}

void GameState_GamePlay::RestartClickedCallback(UIInteractiveElement* interactive_ui)
{
	//get rid of the popup
	GUI::GetInstance()->DestroyPopup();
	GameState_GamePlay* gstate = (GameState_GamePlay*)GameStateManager::GetInstance()->GetCurrentGameState();
	gstate->Restart();
}

void GameState_GamePlay::MainMenuClickedCallback(UIInteractiveElement* interactive_ui)
{
	GUI::GetInstance()->DestroyPopup();
	GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_MAINMENU);
}

void GameState_GamePlay::CloseTutorialPopup(UIInteractiveElement* interactive_ui)
{
	GUI::GetInstance()->DestroyPopup();
}

void GameState_GamePlay::GameOver(bool win)
{
	if (m_gameover) //if the game is over, either win or lose
		return;
	m_gameover = true;
	m_win = win;
}

void GameState_GamePlay::CheckGameOverGUI()
{
	GUI* gui = GUI::GetInstance();
	UIPopup* popup = gui->GetPopup();
	if (!m_gameover || popup) //game over or there is already a popup
		return;
	
	popup = gui->GeneratePopup(512, 256);
	UITextBox* gameover_textbox = popup->AddTextBox("gameover_popup", "", ScreenLoc(170, 40), 36);
	UITextBox* timetaken_textbox = popup->AddTextBox("time_popup", "", ScreenLoc(95, 80), 36);
	popup->AddButton("gameover_popup_mainmenu_btn", "Main Menu", MainMenuClickedCallback, ScreenLoc(70, 180), 190, 48);
	popup->AddButton("gameover_popup_restart_btn", "Restart", RestartClickedCallback, ScreenLoc(280, 180), 150, 48);
	
	if (m_win)
	{
		SoundManager::GetInstance()->PlaySoundFile(".//data//sounds//win.wav");
		gameover_textbox->SetText("You Win!");
	}
	else
	{
		SoundManager::GetInstance()->PlaySoundFile(".//data//sounds//lose.wav");
		gameover_textbox->SetText("You Lose!");
	}
	gameover_textbox->SetFontColor(0,0,0,1);

	string timetaken_text = "Time taken: ";
	timetaken_textbox->SetText("");

	int minutes = (int)m_timetaken/60;
	float seconds = m_timetaken - minutes*60.0;

	char converted_seconds[16];
	sprintf_s(converted_seconds, "%.4g", seconds);

	string time;
	char converted_minutes[8];
	itoa(minutes, converted_minutes, 10);
	time.append(converted_minutes);
	if (seconds < 10.0)
		time.append(":0");
	else
		time.append(":");
	time.append(string(converted_seconds));
	timetaken_textbox->SetText(timetaken_text.append(time));
	timetaken_textbox->SetFontColor(0, 0, 0, 1.0);
}

void GameState_GamePlay::CloseErrorPopup(UIInteractiveElement* interactive_ui)
{
	GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_MAINMENU);
}
