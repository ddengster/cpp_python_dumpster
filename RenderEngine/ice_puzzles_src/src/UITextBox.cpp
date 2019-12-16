
#include "UITextBox.h"
#include "Headers.h"
#include "GLee.h"
#include "FTGL/ftgl.h"

using namespace ddengine_RenderEngine;

UITextBox::UITextBox()
:UIElement(), m_text(""), m_fontsize(0),
 m_font_r(1.0), m_font_g(0.0), m_font_b(0.0), m_font_a(1.0)
{
}

UITextBox::UITextBox(std::string name, string text, ScreenLoc loc, unsigned int fontsize)
:UIElement(name, loc, UI_TEXT_BOX), m_text(text), m_fontsize(fontsize),
 m_font_r(1.0), m_font_g(0.0), m_font_b(0.0), m_font_a(1.0)
{
}

UITextBox::~UITextBox()
{
}

void UITextBox::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	FTGLPixmapFont *font = GUI::GetInstance()->GetFont();
	font->FaceSize(m_fontsize);
	
	SetFontRGBA(m_font_r, m_font_g, m_font_b, m_font_a);

	font->Render(m_text.c_str(), -1, FTPoint(m_location.x, 600 - m_location.y,0));
}

void UITextBox::Loop(float timeElapsed)
{
}

void UITextBox::SetFontColor(float font_r, float font_g, float font_b, float font_a)
{
	m_font_r = font_r;
	m_font_g = font_g;
	m_font_b = font_b;
	m_font_a = font_a;
}