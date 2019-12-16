
#ifndef _UIELEMENT_H
#define _UIELEMENT_H

#include <string>
#include "GUI.h"

using namespace std;

enum UIELEMENT_TYPE
{
	UI_TO_REMOVE = 1,
	UI_BUTTON = 2,
	UI_TEXT_BOX = 4,
	UI_EDITABLE_TEXT_BOX = 8,
	UI_POPUP = 16,
	UI_TABLE = 32
};

class UIElement
{
public:
	virtual ~UIElement() { }
	virtual void	Render() = 0;
	virtual void	Loop(float timeElapsed) = 0;

	UIELEMENT_TYPE GetUIType()	{ return m_type; }

protected:
	friend class GUI;
	UIElement() 
		: m_UIElementname(""), m_location(ScreenLoc(0,0))
	{ }
	UIElement(string name, ScreenLoc loc, UIELEMENT_TYPE type) 
		: m_UIElementname(name), m_location(loc), m_type(type)
	{ }

	string m_UIElementname;
	ScreenLoc m_location;
	UIELEMENT_TYPE m_type;
};

#endif

