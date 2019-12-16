
#ifndef _VERTEXDECLARATION_H
#define _VERTEXDECLARATION_H

#include "Prerequisites.h"
#include "RenderSystem/RendererDefines.h"
#include <vector>

DECLARE_ENGINE_NAMESPACE

struct VertexElement
{
	ELEMENT_USAGE	usage;
	ELEMENT_TYPE	type;
	uint32_t			usage_index;
};

class VertexDeclaration
{
protected:
	std::vector<VertexElement*>		mElements;
	///size of a vertex(including all elements) in bytes
	uint32_t						mVertexStride; 

public:
	VertexDeclaration();
	~VertexDeclaration();

	void	AddVertexElement(ELEMENT_USAGE usage, ELEMENT_TYPE type, uint32_t usage_index = 0);
	void	InsertVertexElement(uint32_t i, ELEMENT_USAGE usage, ELEMENT_TYPE type, uint32_t usage_index=0);

	VertexElement*	GetVertexElement(uint32_t index) {	return	mElements[index]; }
	uint32_t		GetElementIndexByUsage(ELEMENT_USAGE usg, uint32_t usg_i=0); // get element index through usage
	void			RemoveVertexElement(uint32_t index);
	void			RemoveVertexElement(VertexElement* element);
	void			RemoveAllVertexElements();

	uint32_t	GetVertexStride()				{	return mVertexStride;	}
	uint32_t	GetNumberOfElements()			{	return (uint32_t)mElements.size();	}
	/// return size of a vertex element gotten by index
	uint32_t	GetElementSize(uint32_t index)	{	return GetElementTypeSize(mElements[index]->type); }

protected:
	uint32_t	CalculateStride();
	// get the size in bytes for each usage type
	uint32_t	GetElementTypeSize(ELEMENT_TYPE type);
};

END_ENGINE_NAMESPACE

#endif