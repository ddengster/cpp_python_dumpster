
#ifndef _GUI_H
#define _GUI_H

#include <string>
#include <map>
#include <vector>
#include "FTGL/ftgl.h"

//macro for ftgl fonts
#define SetFontRGBA(r,g,b,a)						\
		glPixelTransferf(GL_RED_BIAS, r - 1.0f);	\
		glPixelTransferf(GL_GREEN_BIAS, g - 1.0f);	\
		glPixelTransferf(GL_BLUE_BIAS, b - 1.0f);	\
		glPixelTransferf(GL_ALPHA_BIAS, a - 1.0f);

using namespace std;

class UIInteractiveElement;
class UIButton;
class UIPopup;
class UITextBox;
class UIEditableTextBox;
class UITable;
class UIElement;


struct ScreenLoc
{
	unsigned int x, y;
	ScreenLoc(unsigned int _x, unsigned int _y)
		:x(_x), y(_y)
	{
	}
	ScreenLoc()
		:x(0), y(0)
	{
	}
};

typedef void (*OnClickCallbackFunc)(UIInteractiveElement*);
//GUI uses opengl because the engine doesn't have functionality for 2d rendering
class GUI
{
public:
	virtual ~GUI();

	void	Initialize();
	void	Destroy();

	void	Loop(float timeElapsed);
	void	RemoveMarkedElements();
	void	RenderGUI();

	void	MouseDown(int mousex, int mousey);
	void	MouseInputReceiver(int mousex, int mousey);
	void	KeyInputReceiver(char key);
	void	KeyBack();

	void	IdleAllButtons();

	static	GUI* GetInstance();

	void	SetBackground(string filename, string dir, unsigned int xpos, unsigned int length, unsigned int breadth); //pass empty string to get rid of background

	UIButton*	AddButton(	string name, string text, OnClickCallbackFunc func, 
							ScreenLoc loc, unsigned int xsize, unsigned int ysize);
	UIPopup*	GeneratePopup(unsigned int xsize, unsigned int ysize);
	UIPopup*	GetPopup();
	void		DestroyPopup();

	UITextBox*			AddTextBox(string name, string text, ScreenLoc loc, unsigned int fontsize);
	UIEditableTextBox*	AddEditableTextBox(string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize, unsigned int fontsize);

	bool	RemoveTextBox(string name);
	bool	RemoveUIElement(string name);
	bool	RemoveUIElementOnNextLoop(string name);

	UITable*	AddTable(	string name, ScreenLoc loc, OnClickCallbackFunc func, 
							unsigned int xsize_per_cell, unsigned int ysize_per_cell,
							unsigned int numrows, unsigned int numcolumns);

	bool	RemoveUITable(string name);
	
	UIElement*	GetUIElement(string name);

	FTGLPixmapFont* GetFont();
private:
	GUI();
	FTGLPixmapFont *m_font;

	UIPopup*	m_popup;
	map<string, UIElement*> m_UIElementmap;
	//for button input
	map<string, UIInteractiveElement*> m_InteractiveUImap;

	UIEditableTextBox* m_activeEditableTextbox;
};

#endif
