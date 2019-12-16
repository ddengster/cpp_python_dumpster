
#ifndef _UITEXTBOX_H
#define _UITEXTBOX_H

#include <string>
#include "UIElement.h"

using namespace std;

class UITextBox  : public UIElement
{
public:
	UITextBox();
	UITextBox(string name, string text, ScreenLoc loc, unsigned int fontsize);
	virtual ~UITextBox();

	virtual void	Render();
	virtual void	Loop(float timeElapsed);

	void	SetFontSize(unsigned int fontsize) { m_fontsize = fontsize; }
	void	SetFontColor(float font_r, float font_g, float font_b, float font_a);

	void	SetText(string text)	{	m_text = text;	}

private:
	unsigned int	m_fontsize;
	float			m_font_r, m_font_g, m_font_b, m_font_a;

	string m_text;
};

#endif
