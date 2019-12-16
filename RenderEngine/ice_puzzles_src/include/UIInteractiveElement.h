
#ifndef _UIINTERACTIVEELEMENT_H
#define _UIINTERACTIVEELEMENT_H

#include "UIElement.h"

class UIInteractiveElement : public UIElement
{
public:
	virtual ~UIInteractiveElement() { }

	virtual void	Render() = 0;
	virtual void	Loop(float timeElapsed) = 0;
	
	void	SetResponsive(bool responsive)	{	m_responsive = responsive;	}
	bool	GetResponsive()					{	return m_responsive;	}

protected:
	friend class GUI;

	UIInteractiveElement() 
		: UIElement(), m_callbackfunc(NULL), m_length(0), m_breadth(0), m_responsive(true)
	{ }
	UIInteractiveElement(string name, ScreenLoc loc, UIELEMENT_TYPE type, OnClickCallbackFunc callbackfunc,
						unsigned int xsize, unsigned int ysize) 
		: UIElement(name, loc ,type), m_callbackfunc(callbackfunc), m_length(xsize), m_breadth(ysize), m_responsive(true)
	{ }

	void	GetDimensions(unsigned int *lower_x, unsigned int *lower_y, unsigned int *upper_x, unsigned int *upper_y)
	{
		*lower_x = m_location.x;
		*lower_y = m_location.y;
		*upper_x = m_location.x + m_length;
		*upper_y = m_location.y + m_breadth;
	}

	bool				m_responsive;

	OnClickCallbackFunc m_callbackfunc;
	unsigned int		m_length, m_breadth;

};

#endif
