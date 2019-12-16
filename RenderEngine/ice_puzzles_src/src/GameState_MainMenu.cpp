
#include "GameState_MainMenu.h"
#include "GameStateManager.h"
#include "InputMgr.h"

#include "GUI.h"
#include "UIPopup.h"
#include "UITable.h"
#include "UIEditableTextBox.h"
#include "UITextBox.h"
#include "UIButton.h"
#include <windows.h>

#include "SoundManager.h"

GameState_MainMenu::GameState_MainMenu()
:GameState()
{
}

GameState_MainMenu::~GameState_MainMenu()
{
}

void GameState_MainMenu::Init()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->SetViewPort(0, 0, 800, 600);
	renderer->SetOrtho(0, 800, 600, 0, 0, 1);
	renderer->LookAt(Vector3(0, 0, 10), Vector3(0, 0, -1), Vector3(0, 1, 0));
	GUI::GetInstance()->SetBackground("crackedicebg.jpg",".//data//textures//", 0, 800, 600);
	GUI::GetInstance()->AddButton("SinglePlayer_button", "Single Player", 
									SinglePlayerClickedCallback, ScreenLoc(272,420), 256, 48);
	
	UIButton* soundbtn = GUI::GetInstance()->AddButton("Sound_button", "Mute Sounds", 
														SoundClickedCallback, ScreenLoc(266,480), 270, 48);
	if (SoundManager::GetInstance()->IsMuted())
		soundbtn->SetButtonText("Unmute Sounds");

	GUI::GetInstance()->AddButton("Quit_button", "Quit", 
									QuitClickedCallback, ScreenLoc(336,540), 128, 48);

	GUI::GetInstance()->AddTextBox("Credit_tbox", "Created by: ddeng", ScreenLoc(630, 590), 18);
	SoundManager::GetInstance()->SetBGM(".//data//sounds//introbgm.mp3");
}

void GameState_MainMenu::Loop(float timeElapsed)
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	renderer->ClearFrame(CLEAR_TARGET | CLEAR_STENCIL ,
							 0.0, 0.0, 0.0, 1.0, 1.0, 0);
	renderer->StartRendering();
	GUI::GetInstance()->Loop(timeElapsed);
	renderer->EndRendering();
}

void GameState_MainMenu::OnExit()
{
	GUI::GetInstance()->Destroy();
}

void GameState_MainMenu::SinglePlayerClickedCallback(UIInteractiveElement* interactive_ui)
{
	UIPopup* popup = GUI::GetInstance()->GeneratePopup(512, 256);
	popup->AddButton("popup_ok_btn", "OK", SinglePlayerPopup_OKButtonClickedCallback, ScreenLoc(80, 200), 128, 48);
	popup->AddButton("Cancel_button", "Cancel", SinglePlayerPopup_CancelButtonClickedCallback, ScreenLoc(300,200), 128, 48);
	UITextBox* text = popup->AddTextBox("Popup_SelectMap_text", "Select your map: ",  ScreenLoc(25, 45), 24);
	text->SetFontColor(0.0, 0.0, 0.0, 1.0);
	UIEditableTextBox* etext = popup->AddEditableTextBox("Popup_MapName_etextbox", ScreenLoc(225, 20), 256, 32, 24);
	etext->SetFontColor(0.0, 0.0, 0.0, 1.0);

	unsigned int numcolumns = 3;
	unsigned int numrows = 3;
	UITable* table = popup->AddTable("Pop_Map_table", ScreenLoc(25, 55), SinglePlayerPopup_TableClickedCallback,
									 128, 48, numrows, numcolumns);

	//add strings to file
	WIN32_FIND_DATA filedata;
	HANDLE hSearch = FindFirstFile( ".//data//maps//*.ipm", &filedata);
	if( hSearch != INVALID_HANDLE_VALUE )
	{
		do
		{
			if (filedata.cFileName[0] != '.')
			{
				string filename(filedata.cFileName);
				table->AddString(filename);
			}
		}while(FindNextFile(hSearch, &filedata) == TRUE);
	}

	if (table->GetStrings().size() > (numcolumns*numrows))
	{
		table->AddScrollButtons(48);
	}

	DisableMainMenuBtns();
}

void GameState_MainMenu::SoundClickedCallback(UIInteractiveElement* interactive_ui)
{
	UIButton* btn = (UIButton*)interactive_ui;
	if (btn->GetButtonText() == "Mute Sounds")
	{
		btn->SetButtonText("Unmute Sounds");
		SoundManager::GetInstance()->MuteAll();
	}
	else if (btn->GetButtonText() == "Unmute Sounds")
	{
		btn->SetButtonText("Mute Sounds");
		SoundManager::GetInstance()->UnmuteAll();
	}
}

void GameState_MainMenu::SinglePlayerPopup_CancelButtonClickedCallback(UIInteractiveElement* interactive_ui)
{
	GUI::GetInstance()->DestroyPopup();
	EnableMainMenuBtns();
}

void GameState_MainMenu::SinglePlayerPopup_OKButtonClickedCallback(UIInteractiveElement* interactive_ui)
{
	UIEditableTextBox* etextbox = (UIEditableTextBox*)GUI::GetInstance()->GetUIElement("Popup_MapName_etextbox");
	if (etextbox->GetText() != "")
	{
		string fulldir = ".//data//maps//" + etextbox->GetText();
		GameStateManager::GetInstance()->SetCurrentMap(fulldir);
		GameStateManager::GetInstance()->ChangeGameState(GAMESTATE_GAMEPLAY);
	}
}

void GameState_MainMenu::SinglePlayerPopup_TableClickedCallback(UIInteractiveElement* interactive_ui)
{
	UITable* table = (UITable*)interactive_ui;
	int row, column;
	string text;
	table->GetClickedCellInfo(&row, &column, &text);

	UIEditableTextBox* etextbox = (UIEditableTextBox*)GUI::GetInstance()->GetUIElement("Popup_MapName_etextbox");
	etextbox->SetText(text);
}

void GameState_MainMenu::QuitClickedCallback(UIInteractiveElement* interactive_ui)
{
	PostQuitMessage(0);
}

void GameState_MainMenu::DisableMainMenuBtns()
{
	UIInteractiveElement* e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("SinglePlayer_button");
	e->SetResponsive(false);
	e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("Quit_button");
	e->SetResponsive(false);
	e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("Sound_button");
	e->SetResponsive(false);	
}

void GameState_MainMenu::EnableMainMenuBtns()
{
	UIInteractiveElement* e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("SinglePlayer_button");
	e->SetResponsive(true);
	e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("Quit_button");
	e->SetResponsive(true);
	e = (UIInteractiveElement*)GUI::GetInstance()->GetUIElement("Sound_button");
	e->SetResponsive(true);
}
