
#include "UITable.h"
#include "GLee.h"
#include "FTGL/ftgl.h"

UITable::UITable()
:UIInteractiveElement(), m_lengthpercell(0), m_breadthpercell(0), m_numrows(0), m_numcolumns(0), 
 scrollbuttonup_texture(NULL), scrollbuttondown_texture(NULL), m_scrollbuttonsused(false), m_textoffset(0)
{
}

UITable::UITable(string name, ScreenLoc loc, OnClickCallbackFunc func, 
				 unsigned int xsize_per_cell, unsigned int ysize_per_cell,
				unsigned int numrows, unsigned int numcolumns)
:UIInteractiveElement(name, loc, UI_TABLE, func, xsize_per_cell*numcolumns, ysize_per_cell*numrows), 
					m_lengthpercell(xsize_per_cell), m_breadthpercell(ysize_per_cell),
					m_numrows(numrows), m_numcolumns(numcolumns),
					scrollbuttonup_texture(NULL), scrollbuttondown_texture(NULL),
					m_scrollbuttonsused(false), m_textoffset(0)
{
	SetUpRenderingPlane();
}

UITable::~UITable()
{
}

void UITable::Loop(float timeElapsed)
{
}

void UITable::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	Matrix4 mat = Matrix4::IDENTITY;
	float xlocation = 0;
	float ylocation = 0;
	string textdisplayed;

	for (unsigned int i=0; i<m_numrows; ++i)
	{
		for (unsigned int j=0; j<m_numcolumns; ++j)
		{
			xlocation = m_location.x+(j*m_lengthpercell);
			ylocation = m_location.y+(i*m_breadthpercell);
			mat.MakeTransform(	Vector3(xlocation, ylocation, 0),
						Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
						Vector3(0,1,0)));
			renderer->SetMatrix(VIEW_MATRIX, mat);

			uitable_v_buf->SetBuffer();
			uitable_i_buf->SetBuffer();

			uitable_texture->SetTexture(0);
			renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (uitable_i_buf->GetNumOfIndices()/3));
			uitable_texture->UnsetTexture();
			
			uitable_v_buf->UnsetBuffer();
			uitable_i_buf->UnsetBuffer();

			if (m_textlist.size() > 0 && m_textlist.size() > (i*m_numcolumns+j))
			{
				FTGLPixmapFont *font = GUI::GetInstance()->GetFont();
				font->FaceSize(24);
				SetFontRGBA(0.0, 0.0, 0.0, 1.0);

				if ((i*m_numcolumns+j+m_textoffset) >= m_textlist.size())
					textdisplayed = "";
				else
					textdisplayed = m_textlist[i*m_numcolumns+j+m_textoffset];

				font->Render(textdisplayed.c_str(), -1, FTPoint(xlocation + 10, 600 - ylocation - 25,0));
			}
			mat = Matrix4::IDENTITY;
			renderer->SetMatrix(VIEW_MATRIX, mat);
		}
	}

	if (scrollbuttonup_texture)
	{
		xlocation = m_location.x+(m_numcolumns*m_lengthpercell);
		ylocation = m_location.y;

		mat.MakeTransform(	Vector3(xlocation, ylocation, 0),
							Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
							Vector3(0,1,0)));
		renderer->SetMatrix(VIEW_MATRIX, mat);

		scrollbutton_v_buf->SetBuffer();
		scrollbutton_i_buf->SetBuffer();

		scrollbuttonup_texture->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (scrollbutton_i_buf->GetNumOfIndices()/3));
		scrollbuttonup_texture->UnsetTexture();
		
		uitable_v_buf->UnsetBuffer();
		uitable_i_buf->UnsetBuffer();

		mat = Matrix4::IDENTITY;
		renderer->SetMatrix(VIEW_MATRIX, mat);

		//downbtn
		xlocation = m_location.x + (m_numcolumns*m_lengthpercell);
		ylocation = m_location.y + (m_numrows*m_breadthpercell * 0.5);

		mat.MakeTransform(	Vector3(xlocation, ylocation, 0),
							Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
							Vector3(0,1,0)));
		renderer->SetMatrix(VIEW_MATRIX, mat);

		scrollbutton_v_buf->SetBuffer();
		scrollbutton_i_buf->SetBuffer();

		scrollbuttondown_texture->SetTexture(0);
		renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (scrollbutton_i_buf->GetNumOfIndices()/3));
		scrollbuttondown_texture->UnsetTexture();
		
		uitable_v_buf->UnsetBuffer();
		uitable_i_buf->UnsetBuffer();

		mat = Matrix4::IDENTITY;
		renderer->SetMatrix(VIEW_MATRIX, mat);
	}
}

void UITable::AddString(string text)
{
	m_textlist.push_back(text);
}

void UITable::SetMouseClickedPos(unsigned int mousex, unsigned int mousey)
{
	m_mousex = mousex;
	m_mousey = mousey;
}

void UITable::GetMouseClickedPos(unsigned int *mousex, unsigned int *mousey)
{
	*mousex = m_mousex;
	*mousey = m_mousey;
}

void UITable::GetClickedCellInfo(int *rownum, int *columnnum, string *text)
{
	//upscrollbutton
	float midpoint = m_numrows * m_breadthpercell * 0.5;
	if (m_mousex > m_location.x + m_lengthpercell*m_numcolumns && m_mousex < m_location.x + m_length &&
		m_mousey > m_location.y && m_mousey < m_location.y + midpoint)
	{
		if (((int)m_textoffset - (int)m_numcolumns) >= 0)
			m_textoffset -= m_numcolumns;
		*rownum = 0;
		*columnnum = 0;
		*text = "";
		return;
	}
	//downscrollbutton
	if (m_mousex > m_location.x + m_lengthpercell*m_numcolumns && m_mousex < m_location.x + m_length &&
		m_mousey > m_location.y + midpoint && m_mousey < m_location.y + m_numrows * m_breadthpercell)
	{
		int actualnumrows = m_textlist.size()/m_numcolumns + ((m_textlist.size()%m_numcolumns) ? 1 : 0);

		if ((m_textoffset/m_numcolumns + m_numrows) < actualnumrows)
			m_textoffset += m_numcolumns;
		*rownum = 0;
		*columnnum = 0;
		*text = "";
		return;
	}

	//cells
	for (unsigned int i=0; i<m_numrows; ++i)
	{
		for (unsigned int j=0; j<m_numcolumns; ++j)
		{
			if ( m_mousex > (m_location.x + m_lengthpercell*j) && m_mousex < (m_location.y + m_lengthpercell*(j+1)) &&
				 m_mousey > (m_location.y + m_breadthpercell*i) && m_mousey < (m_location.y + m_breadthpercell*(i+1)))
			{
				*rownum = j;
				*columnnum = i;
				unsigned int listposition = i*m_numcolumns + j;
				if ((listposition+m_textoffset) >= 0 && (listposition+m_textoffset) < m_textlist.size())
					*text = m_textlist[listposition+m_textoffset];
				else
					*text = "";
				return;
			}
		}
	}
}

void UITable::SetUpRenderingPlane()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* table_vert_dec = hwbmgr->GenerateVertexDeclaration();
	table_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	table_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	uitable_v_buf = hwbmgr->GenerateVertexBuffer(table_vert_dec, 4, HDW_BUF_USG_STATIC);
	Real uitable_quad_data[][4] = {
					{ 0.0, 0.0, 0.0, 1.0 },
					{ m_lengthpercell, m_breadthpercell, 1.0, 0.0 },
					{ 0.0, m_breadthpercell, 0.0, 0.0 },
					{ m_lengthpercell, 0.0, 1.0, 1.0 } };

	uitable_v_buf->UpdateData(uitable_quad_data, 0, 4);

	uitable_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t uitable_quad_indices[] =  {	 0, 1, 2,
										0, 3, 1	};

	uitable_i_buf->UpdateData(uitable_quad_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	uitable_texture = texturemgr->GetTexture2D("editabletextbox.jpg","./data/textures/GUI/");
}

void UITable::AddScrollButtons(unsigned int extendedlength)
{
	if (m_scrollbuttonsused)
		return;

	float heightoftable = m_numrows * m_breadthpercell;
	float heightperbutton = heightoftable * 0.5;

	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* vert_dec = hwbmgr->GenerateVertexDeclaration();
	vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT2, 0);
	vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	scrollbutton_v_buf = hwbmgr->GenerateVertexBuffer(vert_dec, 4, HDW_BUF_USG_STATIC);
	Real scrollbutton_quad_data[][4] = {
					{ 0.0, 0.0, 0.0, 1.0 },
					{ extendedlength, heightperbutton, 1.0, 0.0 },
					{ 0.0, heightperbutton, 0.0, 0.0 },
					{ extendedlength, 0.0, 1.0, 1.0 } };

	scrollbutton_v_buf->UpdateData(scrollbutton_quad_data, 0, 4);

	scrollbutton_i_buf = hwbmgr->GenerateIndexBuffer(6, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t scrollbutton_quad_indices[] =  {	0, 1, 2,
												0, 3, 1	};

	scrollbutton_i_buf->UpdateData(scrollbutton_quad_indices, 0, 6);

	TextureManager* texturemgr = TextureManager::GetInstance();
	scrollbuttonup_texture = texturemgr->GetTexture2D("tablebuttonup.jpg","./data/textures/GUI/");
	scrollbuttondown_texture = texturemgr->GetTexture2D("tablebuttondown.jpg","./data/textures/GUI/");

	m_scrollbuttonsused = true;
	m_length += extendedlength;
}
