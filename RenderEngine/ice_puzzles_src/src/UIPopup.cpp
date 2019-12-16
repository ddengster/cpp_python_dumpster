
#include "UIPopup.h"
#include "Headers.h"
#include "UITextBox.h"

UIPopup::UIPopup()
:UIElement(), m_length(0), m_breadth(0), popup_v_buf(NULL), popup_i_buf(NULL), popup_texture(NULL)
{
}

UIPopup::UIPopup(std::string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize)
:UIElement(name, loc, UI_POPUP), m_length(xsize), m_breadth(ysize), popup_v_buf(NULL), popup_i_buf(NULL), popup_texture(NULL)
{
	SetUpRenderingPlane();
}

UIPopup::~UIPopup()
{
}

void UIPopup::Loop(float timeElapsed)
{
}

void UIPopup::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	
	Matrix4 mat = Matrix4::IDENTITY;
	mat.MakeTransform(	Vector3(m_location.x, m_location.y, 0),
						Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
						Vector3(0,1,0)));
	renderer->SetMatrix(VIEW_MATRIX, mat);

	popup_v_buf->SetBuffer();
	popup_i_buf->SetBuffer();

	popup_texture->SetTexture(0);
	renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (popup_i_buf->GetNumOfIndices()/3));
	popup_texture->UnsetTexture();
	
	popup_v_buf->UnsetBuffer();
	popup_i_buf->UnsetBuffer();

	mat = Matrix4::IDENTITY;
	renderer->SetMatrix(VIEW_MATRIX, mat);
}

UITextBox* UIPopup::AddTextBox(string name, string text, ScreenLoc local_coords, unsigned int fontsize)
{
	UITextBox* textbox = GUI::GetInstance()->AddTextBox(name, text, ScreenLoc(m_location.x+local_coords.x, m_location.y+local_coords.y),
														fontsize);

	m_UIElementmap.insert(make_pair<string, UIElement*>(name, (UIElement*)textbox));
	return textbox;
}

UIEditableTextBox* UIPopup::AddEditableTextBox(string name, ScreenLoc local_coords, unsigned int xsize, unsigned int ysize, unsigned int fontsize)
{
	UIEditableTextBox* etextbox = GUI::GetInstance()->AddEditableTextBox(name, ScreenLoc(m_location.x+local_coords.x, m_location.y+local_coords.y),
														xsize, ysize, fontsize);

	m_UIElementmap.insert(make_pair<string, UIElement*>(name, (UIElement*)etextbox));
	return etextbox;
}

UIButton* UIPopup::AddButton(string name, string text, OnClickCallbackFunc func, 
						ScreenLoc local_coords, unsigned int xsize, unsigned int ysize)
{
	UIButton* button = GUI::GetInstance()->AddButton(name, text, func,
									ScreenLoc(m_location.x+local_coords.x, m_location.y+local_coords.y),
									xsize, ysize);

	m_UIElementmap.insert(make_pair<string, UIElement*>(name, (UIElement*)button));
	return button;
}

UITable* UIPopup::AddTable(	string name, ScreenLoc local_coords, OnClickCallbackFunc func, 
							unsigned int xsize_per_cell, unsigned int ysize_per_cell,
							unsigned int numrows, unsigned int numcolumns)
{
	UITable* table = GUI::GetInstance()->AddTable(name, ScreenLoc(m_location.x+local_coords.x, m_location.y+local_coords.y),
												func, xsize_per_cell, ysize_per_cell, numrows, numcolumns);

	m_UIElementmap.insert(make_pair<string, UIElement*>(name, (UIElement*)table));
	return table;
}

bool UIPopup::RemoveButton(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;

	m_UIElementmap.erase(it);
	if (GUI::GetInstance()->RemoveUIElementOnNextLoop(name))
		return true;
	return false;
}

bool UIPopup::RemoveTextBox(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;

	m_UIElementmap.erase(it);
	if (GUI::GetInstance()->RemoveTextBox(name))
		return true;
	return false;
}

bool UIPopup::RemoveEditableTextBox(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;

	m_UIElementmap.erase(it);
	if (GUI::GetInstance()->RemoveUIElementOnNextLoop(name))
		return true;
	return false;
}

bool UIPopup::RemoveTable(string name)
{
	map<string, UIElement*>::iterator it = m_UIElementmap.find(name);
	if (it == m_UIElementmap.end())
		return false;

	m_UIElementmap.erase(it);
	if (GUI::GetInstance()->RemoveUIElementOnNextLoop(name))
		return true;
	return false;
}

void UIPopup::SetUpRenderingPlane()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* popup_vert_dec = hwbmgr->GenerateVertexDeclaration();
	popup_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	popup_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	popup_v_buf = hwbmgr->GenerateVertexBuffer(popup_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real popup_quad_data[][4] = {
					{ 0.0, 0.0, 0.0, 1.0 },
					{ m_length, m_breadth, 1.0, 0.0 },
					{ 0.0, m_breadth, 0.0, 0.0 },
					{ m_length, 0.0, 1.0, 1.0 } };

	popup_v_buf->UpdateData(popup_quad_data, 0, 4);

	popup_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t popup_quad_indices[] =  {	 0, 1, 2,
										0, 3, 1	};

	popup_i_buf->UpdateData(popup_quad_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	popup_texture = texturemgr->GetTexture2D("popup.jpg","./data/textures/GUI/");
}

void UIPopup::Destroy()
{
	//ui elements render
	for (map<string, UIElement*>::iterator it = m_UIElementmap.begin(); it != m_UIElementmap.end(); ++it)
	{
		GUI::GetInstance()->RemoveUIElementOnNextLoop(it->first);
	}
	m_UIElementmap.clear();
}
