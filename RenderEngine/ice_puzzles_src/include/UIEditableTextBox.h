
#ifndef _UIEDITABLEBOX_H
#define _UIEDITABLEBOX_H

#include "UIInteractiveElement.h"
#include "Headers.h"

using namespace ddengine_RenderEngine;

class UIEditableTextBox : public UIInteractiveElement
{
public:
	UIEditableTextBox();
	UIEditableTextBox(std::string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize, unsigned int fontsize);
	virtual ~UIEditableTextBox();

	virtual void	Render();
	virtual void	Loop(float timeElapsed);

	string& GetText() { return m_text; }
	void	SetText(string &text) { m_text = text; }

	void	SetFontSize(unsigned int fontsize);
	void	SetFontColor(float font_r, float font_g, float font_b, float font_a);

protected:
	friend class GUI;
	void	SetUpRenderingPlane();

	VertexBuffer*	etextbox_v_buf;
	IndexBuffer*	etextbox_i_buf;
	Texture*		etextbox_texture;

	unsigned int	m_fontsize;
	string			m_text;
	float			m_font_r, m_font_g, m_font_b, m_font_a;
};

#endif
