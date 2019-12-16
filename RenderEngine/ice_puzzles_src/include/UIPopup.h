
#ifndef _UIPOPUP_H
#define _UIPOPUP_H

#include <map>
#include "UIElement.h"
#include "Headers.h"

using namespace std;
using namespace ddengine_RenderEngine;

class UIButton;
class UITextBox;
class UIEditableTextBox;
class UITable;

struct ScreenLoc;

class UIPopup : public UIElement
{
public:
	UIPopup();
	UIPopup(std::string name, ScreenLoc loc, unsigned int xsize, unsigned int ysize);
	virtual ~UIPopup();

	virtual void	Render();
	virtual void	Loop(float timeElapsed);

	UITextBox*			AddTextBox(string name, string text, ScreenLoc local_coords, unsigned int fontsize);
	UIEditableTextBox*	AddEditableTextBox(string name, ScreenLoc local_coords, unsigned int xsize, unsigned int ysize,
											unsigned int fontsize);
	
	UIButton*	AddButton(	string name, string text, OnClickCallbackFunc func, 
							ScreenLoc local_coords, unsigned int xsize, unsigned int ysize);
	UITable*	AddTable(	string name, ScreenLoc local_coords, OnClickCallbackFunc func, 
							unsigned int xsize_per_cell, unsigned int ysize_per_cell,
							unsigned int numrows, unsigned int numcolumns);

	bool	RemoveButton(string name);
	bool	RemoveTextBox(string name);
	bool	RemoveEditableTextBox(string name);
	bool	RemoveTable(string name);

	void	Destroy();
private:
	void	SetUpRenderingPlane();
	VertexBuffer*	popup_v_buf;
	IndexBuffer*	popup_i_buf;
	Texture*		popup_texture;
	unsigned int		m_length, m_breadth;

	map<string, UIElement*> m_UIElementmap;
};

#endif
