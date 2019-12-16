
#include "GUI.h"
#include "UIButton.h"
#include "UITable.h"
#include "UIPopup.h"
#include "UIEditableTextBox.h"
#include "UITextBox.h"

#include "InputMgr.h"
#include "Headers.h"
#include "GLee.h"

#include "SoundManager.h"

using namespace ddengine_RenderEngine;

Texture* background_texture = NULL;
VertexBuffer* bg_v_buf = NULL;
IndexBuffer* bg_i_buf = NULL;

GUI::GUI()
:m_popup(NULL), m_activeEditableTextbox(NULL)
{
	m_font = new FTGLPixmapFont("C:\\WINDOWS\\Fonts\\Arial.ttf");
	m_font->UseDisplayList(false);
}

GUI::~GUI()
{
	Destroy();
	SAFE_DELETE(m_font);
}

GUI* GUI::GetInstance()
{
	static GUI ui;
	return &ui;
}

void GUI::Initialize()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->SetViewPort(0,0, 800, 600);
	renderer->SetOrtho(0, 800, 600, 0, 0, 1);
	//the engine rendersystem should have already initialized opengl for us
}

void GUI::Destroy()
{
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();
	hwbmgr->DestroyAllVertexBuffer();
	hwbmgr->DestroyAllIndexBuffer();
	hwbmgr->DestroyAllVertexDeclaration();
	TextureManager* textmgr = TextureManager::GetInstance();
	textmgr->DestroyAllTextures();
	bg_v_buf = NULL;
	bg_i_buf = NULL;
	background_texture = NULL;
	for (map<string, UIElement*>::iterator it = m_UIElementmap.begin(); it != m_UIElementmap.end(); ++it)
	{
		if (it->second)
			delete it->second;
		it->second = 0;
	}
	SAFE_DELETE(m_popup);
	m_InteractiveUImap.clear();
	m_UIElementmap.clear();
}

void GUI::Loop(float timeElapsed)
{
	//Input();
	//check which element to remove
	RemoveMarkedElements();
	RenderGUI();
}

void GUI::RemoveMarkedElements()
{
	vector<string> namelist;
	for (map<string, UIElement*>::iterator it = m_UIElementmap.begin(); it != m_UIElementmap.end(); ++it)
	{
		if (it->second->GetUIType() & UI_TO_REMOVE)
		{
			if ((it->second->m_type & UI_BUTTON) || (it->second->m_type & UI_TABLE)|| (it->second->m_type & UI_EDITABLE_TEXT_BOX))
			{
				map<string, UIInteractiveElement*>::iterator it2 = m_InteractiveUImap.find(it->first);
				if (it2 != m_InteractiveUImap.end())
					m_InteractiveUImap.erase(it2);
			}
			SAFE_DELETE(it->second);
			namelist.push_back(it->first);
		}
	}
	
	for (unsigned int i=0; i < namelist.size() ; ++i)
	{
		m_UIElementmap.erase(namelist[i]);
	}
}

void GUI::RenderGUI()
{
	//convert renderstates
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	renderer->SetRenderState(RS_DEPTH_TESTING, DEPTHMODE_DISABLE);

	//put this somewhere depending on whether you are rendering 3d as well
	//renderer->ClearFrame(CLEAR_TARGET | CLEAR_STENCIL ,
	//						 0.0, 0.0, 0.0, 1.0, 1.0, 0);
	//renderer->StartRendering();
	renderer->SetOrtho(0, 800, 600, 0, 0, 1);

	if (background_texture)
	{
		bg_v_buf->SetBuffer();
		bg_i_buf->SetBuffer();

		background_texture->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (bg_i_buf->GetNumOfIndices()/3));
		background_texture->UnsetTexture();
		
		bg_v_buf->UnsetBuffer();
		bg_i_buf->UnsetBuffer();
	}

	if (m_popup)
		m_popup->Render();
	//ui elements render
	for (map<string, UIElement*>::iterator it = m_UIElementmap.begin(); it != m_UIElementmap.end(); ++it)
	{
		it->second->Render();
	}
	
	SetFontRGBA(1.0, 1.0, 1.0, 1.0);

	//renderer->EndRendering();
	renderer->SetRenderState(RS_DEPTH_TESTING, DEPTHMODE_ENABLE);
}

void GUI::MouseDown(int mousex, int mousey)
{
	ScreenLoc lowerpt, upperpt;
	for (map<string, UIInteractiveElement*>::iterator it = m_InteractiveUImap.begin(); it != m_InteractiveUImap.end(); ++it)
	{
		it->second->GetDimensions(&lowerpt.x, &lowerpt.y, &upperpt.x, &upperpt.y);
		if ((unsigned int)mousex <= upperpt.x && (unsigned int)mousex >= lowerpt.x &&
			(unsigned int)mousey <= upperpt.y && (unsigned int)mousey >= lowerpt.y &&
			it->second->GetResponsive() )
		{
			if (it->second->GetUIType() & UI_BUTTON)
			{
				UIButton* button = (UIButton*) it->second;
				button->SetClicked(true);
				SoundManager::GetInstance()->PlaySoundFile(".//data//sounds//click.wav");
				return;
			}
		}
	}
	IdleAllButtons();
}

void GUI::MouseInputReceiver(int mousex, int mousey)
{
	//check all buttons to see whether they are clicked
	ScreenLoc lowerpt, upperpt;
	for (map<string, UIInteractiveElement*>::iterator it = m_InteractiveUImap.begin(); it != m_InteractiveUImap.end(); ++it)
	{
		it->second->GetDimensions(&lowerpt.x, &lowerpt.y, &upperpt.x, &upperpt.y);
		if ((unsigned int)mousex <= upperpt.x && (unsigned int)mousex >= lowerpt.x &&
			(unsigned int)mousey <= upperpt.y && (unsigned int)mousey >= lowerpt.y &&
			it->second->GetResponsive() )
		{
			if (it->second->GetUIType() & UI_EDITABLE_TEXT_BOX)
			{
				InputMgr::GetInputMgr()->ReceiveText(true);
				m_activeEditableTextbox = (UIEditableTextBox*) it->second;
				return;
			}
			else if (it->second->GetUIType() & UI_TABLE)
			{
				UITable* table = (UITable*)it->second;
				table->SetMouseClickedPos(mousex, mousey);
				table->m_callbackfunc(it->second);
				break;
			}
			else if (it->second->GetUIType() & UI_BUTTON)
			{
				UIButton* button = (UIButton*) it->second;
				button->SetClicked(false);

				it->second->m_callbackfunc(it->second);
			}
		}
	}
	InputMgr::GetInputMgr()->ReceiveText(false);
	m_activeEditableTextbox = NULL;
}

void GUI::KeyInputReceiver(char key)
{
	if (m_activeEditableTextbox)
	{
		m_activeEditableTextbox->m_text += key;
	}
}

void GUI::KeyBack()
{
	if (m_activeEditableTextbox)
	{
		if (m_activeEditableTextbox->m_text.size() > 0)
			m_activeEditableTextbox->m_text.erase(m_activeEditableTextbox->m_text.size()-1);
	}
}

void GUI::IdleAllButtons()
{
	for (map<string, UIInteractiveElement*>::iterator it = m_InteractiveUImap.begin(); it != m_InteractiveUImap.end(); ++it)
	{
		if (it->second->GetUIType() & UI_BUTTON)
		{
			UIButton* button = (UIButton*) it->second;
			button->SetClicked(false);
		}
	}
}

void GUI::SetBackground(string filename, string dir, unsigned int xpos, unsigned int length, unsigned int breadth)
{
	if (bg_v_buf)
		return;
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* bg_vert_dec = hwbmgr->GenerateVertexDeclaration();
	bg_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	bg_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	bg_v_buf = hwbmgr->GenerateVertexBuffer(bg_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real bg_quad_data[][4] = {
					{ xpos, 0.0, 0.0, 1.0 },
					{ xpos+length, breadth, 1.0, 0.0 },
					{ xpos, breadth, 0.0, 0.0 },
					{ xpos+length, 0.0, 1.0, 1.0 } };

	bg_v_buf->UpdateData(bg_quad_data, 0, 4);

	bg_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t bg_quad_indices[] =  {	 0, 1, 2,
									 0, 3, 1	};

	bg_i_buf->UpdateData(bg_quad_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	background_texture = texturemgr->GetTexture2D(filename, dir);
}

UIButton* GUI::AddButton(std::string name, std::string text, OnClickCallbackFunc func, 
						 ScreenLoc loc, unsigned int xsize, unsigned int ysize)
{
	if (m_UIElementmap.find(name) != m_UIElementmap.end())
		return NULL;
	UIButton* button = new UIButton(name, text, func, loc, xsize, ysize);
	m_UIElementmap.insert(std::make_pair<string, UIElement*>(name, button));
	m_InteractiveUImap.insert(std::make_pair<string, UIInteractiveElement*>(name, button));
	return button;
}

UIPopup* GUI::GeneratePopup(unsigned int xsize, unsigned int ysize)
{
	if (!m_popup)
		m_popup = new UIPopup("GUIPopup", ScreenLoc(150, 150), xsize, ysize);

	return m_popup;
}

UIPopup* GUI::GetPopup()
{
	return m_popup;
}

void GUI::DestroyPopup()
{
	if (m_popup)
	{
		m_popup->Destroy();
		delete m_popup;
	}
	m_popup = NULL;
}

UITextBox* GUI::AddTextBox(string name, string text, ScreenLoc loc, unsigned int fontsize)
{
	if (m_UIElementmap.find(name) != m_UIElementmap.end())
		return NULL;
	UITextBox* textbox = new UITextBox(name, text, loc, fontsize);
	m_UIElementmap.insert(std::make_pair<string, UIElement*>(name, textbox));
	return textbox;
}

UIEditableTextBox* GUI::AddEditableTextBox(string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize, unsigned int fontsize)
{
	if (m_UIElementmap.find(name) != m_UIElementmap.end())
		return NULL;
	UIEditableTextBox* etextbox = new UIEditableTextBox(name, loc, xsize, ysize, fontsize);
	m_UIElementmap.insert(std::make_pair<string, UIElement*>(name, etextbox));
	m_InteractiveUImap.insert(std::make_pair<string, UIInteractiveElement*>(name, etextbox));
	return etextbox;
}

UITable*  GUI::AddTable(	string name, ScreenLoc loc, OnClickCallbackFunc func, 
							unsigned int xsize_per_cell, unsigned int ysize_per_cell,
							unsigned int numrows, unsigned int numcolumns)
{
	if (m_UIElementmap.find(name) != m_UIElementmap.end())
		return NULL;
	UITable* table = new UITable(name, loc, func, xsize_per_cell, ysize_per_cell, numrows, numcolumns);

	m_UIElementmap.insert(make_pair<string, UIElement*>(name, (UIElement*)table));
	m_InteractiveUImap.insert(std::make_pair<string, UIInteractiveElement*>(name, table));
	return table;
}

bool GUI::RemoveTextBox(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;
	if (it->second)
		delete it->second;
	it->second = NULL;
	m_UIElementmap.erase(it);
	return true;
}

bool GUI::RemoveUIElement(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;

	if (it->second)
		delete it->second;
	it->second = NULL;
	return true;
}

bool GUI::RemoveUIElementOnNextLoop(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;
	
	it->second->m_type =  (UIELEMENT_TYPE) (it->second->m_type | UI_TO_REMOVE);
	return true;
}

UIElement* GUI::GetUIElement(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return NULL;
	return it->second;
}

FTGLPixmapFont* GUI::GetFont()
{
	return m_font;
}
