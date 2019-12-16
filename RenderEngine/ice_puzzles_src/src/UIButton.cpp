
#include "UIButton.h"
#include "GLee.h"
#include "FTGL/ftgl.h"

#include "InputMgr.h"

UIButton::UIButton()
:UIInteractiveElement(), m_ButtonText(""), button_v_buf(NULL), button_i_buf(NULL),
 m_buttontexture(NULL), m_clickedtexture(NULL), m_idletexture(NULL), m_fontsize(0),
 m_font_r(0.0), m_font_g(0.0), m_font_b(0.0), m_font_a(0.0)
{
}

UIButton::UIButton(std::string name, std::string text, OnClickCallbackFunc func, 
				   ScreenLoc loc, unsigned int xsize, unsigned int ysize)
			 :UIInteractiveElement(name, loc, UI_BUTTON, func, xsize, ysize), m_ButtonText(text),
			  button_v_buf(NULL), button_i_buf(NULL), m_buttontexture(NULL), m_clickedtexture(NULL), m_idletexture(NULL),
			  m_fontsize(36), m_font_r(1.0), m_font_g(0.0), m_font_b(0.0), m_font_a(1.0)
{
	SetUpRenderingPlane();
	//test
	TextureManager* texturemgr = TextureManager::GetInstance();
	Texture* t = texturemgr->GetTexture2D("button.tga","./data/textures/GUI/");
	SetIdleButtonTexture(t);
	t = texturemgr->GetTexture2D("buttondepressed.tga","./data/textures/GUI/");
	SetClickedButtonTexture(t);
	m_buttontexture = m_idletexture;
}

UIButton::~UIButton()
{
}

void UIButton::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	if (m_buttontexture)
	{
		Matrix4 mat = Matrix4::IDENTITY;
		mat.MakeTransform(	Vector3(m_location.x, m_location.y, 0),
							Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
							Vector3(0,1,0)));
		renderer->SetMatrix(VIEW_MATRIX, mat);

		renderer->SetRenderState(RS_BLENDING, BLENDING_ENABLE);
		glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		button_v_buf->SetBuffer();
		button_i_buf->SetBuffer();

		m_buttontexture->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (button_i_buf->GetNumOfIndices()/3));
		m_buttontexture->UnsetTexture();
		
		button_v_buf->UnsetBuffer();
		button_i_buf->UnsetBuffer();
		renderer->SetRenderState(RS_BLENDING, BLENDING_DISABLE);

		//fontsize in proportion to the size
		FTGLPixmapFont *font = GUI::GetInstance()->GetFont();
		font->FaceSize(m_fontsize);
	
		SetFontRGBA(m_font_r, m_font_g, m_font_b, m_font_a);
		//centering
		FTBBox box = font->BBox(m_ButtonText.c_str());
		float boxlength = box.Upper().X() - box.Lower().X();
		float offsetx = (m_length - boxlength) * 0.5;

		float boxheight = (box.Upper().Y() - box.Lower().Y());
		float offsety = m_breadth - boxheight - 5.0;

		font->Render(m_ButtonText.c_str(), -1, FTPoint(m_location.x + offsetx, 600 - m_location.y + offsety - m_breadth,0));
		
		mat = Matrix4::IDENTITY;
		renderer->SetMatrix(VIEW_MATRIX, mat);
	}
}

void UIButton::Loop(float timeElapsed)
{
}

void UIButton::SetIdleButtonTexture(ddengine_RenderEngine::Texture* tex)
{
	m_idletexture = tex;
}

void UIButton::SetClickedButtonTexture(ddengine_RenderEngine::Texture* tex)
{
	m_clickedtexture = tex;
}

void UIButton::SetFontSize(unsigned int fontsize)
{
	m_fontsize = fontsize;
}

void UIButton::SetFontColor(float font_r, float font_g, float font_b, float font_a)
{
	m_font_r = font_r;
	m_font_g = font_g;
	m_font_b = font_b;
	m_font_a = font_a;
}

void UIButton::SetClicked(bool yesno)
{
	if (yesno)
		m_buttontexture = m_clickedtexture;
	else
		m_buttontexture = m_idletexture;
}

void UIButton::SetUpRenderingPlane()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* button_vert_dec = hwbmgr->GenerateVertexDeclaration();
	button_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	button_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	button_v_buf = hwbmgr->GenerateVertexBuffer(button_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real button_quad_data[][4] = {
					{ 0.0, 0.0, 0.0, 1.0 },
					{ m_length, m_breadth, 1.0, 0.0 },
					{ 0.0, m_breadth, 0.0, 0.0 },
					{ m_length, 0.0, 1.0, 1.0 } };

	button_v_buf->UpdateData(button_quad_data, 0, 4);

	button_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t button_quad_indices[] =  {	 0, 1, 2,
										0, 3, 1	};

	button_i_buf->UpdateData(button_quad_indices, 0, 6);
}
