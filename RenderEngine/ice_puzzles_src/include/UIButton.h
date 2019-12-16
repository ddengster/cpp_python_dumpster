
#ifndef _UIBUTTON_H
#define _UIBUTTON_H

#include <string>
#include "UIInteractiveElement.h"
#include "Headers.h"

using namespace std;
using namespace ddengine_RenderEngine;

class UIButton;

class UIButton : public UIInteractiveElement
{
public:
	UIButton();
	UIButton(std::string name, std::string text, OnClickCallbackFunc func, 
			 ScreenLoc loc, unsigned int xsize, unsigned int ysize);
	virtual ~UIButton();

	virtual void	Render();
	virtual void	Loop(float timeElapsed);
	
	void	SetIdleButtonTexture(Texture* tex);
	void	SetClickedButtonTexture(Texture* tex);

	void	SetFontSize(unsigned int fontsize);
	void	SetFontColor(float font_r, float font_g, float font_b, float font_a);

	void	SetClicked(bool yesno);
	void	SetButtonText(string text)	{	m_ButtonText = text; }
	string& GetButtonText()				{	return m_ButtonText; }

private:
	void	SetUpRenderingPlane();

	VertexBuffer*	button_v_buf;
	IndexBuffer*	button_i_buf;
	Texture*		m_idletexture;
	Texture*		m_clickedtexture;
	Texture*		m_buttontexture;

	string				m_ButtonText;
	unsigned int		m_fontsize;
	float				m_font_r, m_font_g, m_font_b, m_font_a;
};

#endif
