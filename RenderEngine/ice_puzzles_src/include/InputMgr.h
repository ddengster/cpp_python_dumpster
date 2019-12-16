
#ifndef _INPUTMGR_H
#define _INPUTMGR_H

class InputMgr
{
public:
	virtual ~InputMgr();

	static InputMgr* GetInputMgr();

	void InputLoop();

	void ReceiveText(bool yesno) { m_receivetext = yesno; }
	void SetUpperCase(bool yesno) { m_uppercase = yesno; }

	bool CheckKeyDown(int key); //virtual key codes as parameter. http://msdn.microsoft.com/en-us/library/ms645540(VS.85).aspx
	void LeftMouseButtonDown(int mousex, int mousey);
	void LeftMouseButtonUp(int mousex, int mousey);
	void KeyInput(char key);
	void Back();
	void SpecialKeyInput(char key);

protected:
	InputMgr();

	bool m_receivetext;
	bool m_uppercase;
};

#endif
