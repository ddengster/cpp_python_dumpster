
#include "InputMgr.h"
#include <windows.h>

#include "GUI.h"

#define KEY_DOWN(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 1 : 0)
#define KEY_UP(vk_code) ((GetAsyncKeyState(vk_code) & 0x8000) ? 0 : 1)

InputMgr::InputMgr()
:m_receivetext(false), m_uppercase(false)
{
}

InputMgr::~InputMgr()
{
}

InputMgr* InputMgr::GetInputMgr()
{
	static InputMgr mgr;
	return &mgr;
}

void InputMgr::InputLoop()
{
}

bool InputMgr::CheckKeyDown(int key)
{
	if (KEY_DOWN(key))
		return true;
	return false;
}

void InputMgr::LeftMouseButtonDown(int mousex, int mousey)
{
	//GUI::GetInstance()->MouseInputReceiver(mousex, mousey);
	GUI::GetInstance()->MouseDown(mousex, mousey);
}

void InputMgr::LeftMouseButtonUp(int mousex, int mousey)
{
	GUI::GetInstance()->MouseInputReceiver(mousex, mousey);
	GUI::GetInstance()->IdleAllButtons();
}

void InputMgr::KeyInput(char key)
{
	if (!m_uppercase)
		key += 32;
	GUI::GetInstance()->KeyInputReceiver(key);
}

void InputMgr::Back()
{
	GUI::GetInstance()->KeyBack();
}

void InputMgr::SpecialKeyInput(char key)
{
	if (m_uppercase)
	{
		switch (key)
		{
		case '-':
			key = '_';
			break;
		}
	}
	else
	{
		switch (key)
		{
		case '+':
			key = '=';
			break;
		}
	}
	GUI::GetInstance()->KeyInputReceiver(key);
}