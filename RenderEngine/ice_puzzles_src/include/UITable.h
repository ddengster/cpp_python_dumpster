
#ifndef _UITABLE_H
#define _UITABLE_H

#include <vector>
#include <string>
#include "UIInteractiveElement.h"
#include "Headers.h"

using namespace ddengine_RenderEngine;
using namespace std;

class UITable : public UIInteractiveElement
{
public:
	UITable();
	UITable(string name, ScreenLoc loc, OnClickCallbackFunc func, 
			unsigned int xsize_per_cell, unsigned int ysize_per_cell,
			unsigned int numrows, unsigned int numcolumns);
	virtual ~UITable();

	virtual void	Render();
	virtual void	Loop(float timeElapsed);

	vector<string>& GetStrings() { return m_textlist; }
	void	AddString(string text);

	void	SetMouseClickedPos(unsigned int mousex, unsigned int mousey);
	void	GetMouseClickedPos(unsigned int *mousex, unsigned int *mousey);
	void	GetClickedCellInfo(int *rownum, int *columnnum, string *text);

	//used if you want to traverse up and down the table
	void	AddScrollButtons(unsigned int extendedlength); 
private:
	void SetUpRenderingPlane();

	unsigned int	m_lengthpercell, m_breadthpercell;
	unsigned int	m_numrows, m_numcolumns;
	
	vector<string>	m_textlist;
	unsigned int	m_textoffset;

	VertexBuffer*	uitable_v_buf;
	IndexBuffer*	uitable_i_buf;
	Texture*		uitable_texture;

	bool			m_scrollbuttonsused;
	VertexBuffer*	scrollbutton_v_buf;
	IndexBuffer*	scrollbutton_i_buf;
	Texture*		scrollbuttonup_texture;
	Texture*		scrollbuttondown_texture;

	unsigned int m_mousex, m_mousey;
};

#endif
