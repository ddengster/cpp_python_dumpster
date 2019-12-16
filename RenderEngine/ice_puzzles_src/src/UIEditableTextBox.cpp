
#include "UIEditableTextBox.h"
#include "GLee.h"
#include "FTGL/ftgl.h"

UIEditableTextBox::UIEditableTextBox()
:UIInteractiveElement(), m_text(""), m_fontsize(0), etextbox_v_buf(NULL), etextbox_i_buf(NULL), etextbox_texture(NULL),
 m_font_r(0.0), m_font_g(0.0), m_font_b(0.0), m_font_a(0.0)
{
}

UIEditableTextBox::UIEditableTextBox(std::string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize,
									 unsigned int fontsize)
:UIInteractiveElement(name, loc, UI_EDITABLE_TEXT_BOX, NULL, xsize, ysize), m_text(""), m_fontsize(fontsize),
	etextbox_v_buf(NULL), etextbox_i_buf(NULL), etextbox_texture(NULL),
	m_font_r(1.0), m_font_g(0.0), m_font_b(0.0), m_font_a(1.0)
{
	SetUpRenderingPlane();
}

UIEditableTextBox::~UIEditableTextBox()
{
}

void UIEditableTextBox::Loop(float timeElapsed)
{
}

void UIEditableTextBox::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	Matrix4 mat = Matrix4::IDENTITY;
	mat.MakeTransform(	Vector3(m_location.x, m_location.y, 0),
						Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
						Vector3(0,1,0)));
	renderer->SetMatrix(VIEW_MATRIX, mat);

	etextbox_v_buf->SetBuffer();
	etextbox_i_buf->SetBuffer();

	etextbox_texture->SetTexture(0);
	renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (etextbox_i_buf->GetNumOfIndices()/3));
	etextbox_texture->UnsetTexture();
	
	etextbox_v_buf->UnsetBuffer();
	etextbox_i_buf->UnsetBuffer();

	FTGLPixmapFont *font = GUI::GetInstance()->GetFont();
	font->FaceSize(m_fontsize);
	
	SetFontRGBA(m_font_r, m_font_g, m_font_b, m_font_a);

	font->Render(m_text.c_str(), -1, FTPoint(m_location.x + 10, 600 - m_location.y - 25,0));
	
	mat = Matrix4::IDENTITY;
	renderer->SetMatrix(VIEW_MATRIX, mat);
}

void UIEditableTextBox::SetFontSize(unsigned int fontsize)
{
	m_fontsize = fontsize;
}

void UIEditableTextBox::SetFontColor(float font_r, float font_g, float font_b, float font_a)
{
	m_font_r = font_r;
	m_font_g = font_g;
	m_font_b = font_b;
	m_font_a = font_a;
}

void UIEditableTextBox::SetUpRenderingPlane()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* etextbox_vert_dec = hwbmgr->GenerateVertexDeclaration();
	etextbox_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	etextbox_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	etextbox_v_buf = hwbmgr->GenerateVertexBuffer(etextbox_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real etextbox_quad_data[][4] = {
					{ 0.0, 0.0, 0.0, 1.0 },
					{ m_length, m_breadth, 1.0, 0.0 },
					{ 0.0, m_breadth, 0.0, 0.0 },
					{ m_length, 0.0, 1.0, 1.0 } };

	etextbox_v_buf->UpdateData(etextbox_quad_data, 0, 4);

	etextbox_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t etextbox_quad_indices[] =  {	 0, 1, 2,
										0, 3, 1	};

	etextbox_i_buf->UpdateData(etextbox_quad_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	etextbox_texture = texturemgr->GetTexture2D("editabletextbox.jpg","./data/textures/GUI/");
}
