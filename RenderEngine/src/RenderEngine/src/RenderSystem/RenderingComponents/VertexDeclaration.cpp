
#include "RenderSystem/RenderingComponents/VertexDeclaration.h"

#include "LogManager/LogManager.h"

DECLARE_ENGINE_NAMESPACE

VertexDeclaration::VertexDeclaration()
:mVertexStride(0)
{
	mElements.clear();
}

VertexDeclaration::~VertexDeclaration()
{
	for (std::vector<VertexElement*>::iterator i=mElements.begin(); i!=mElements.end(); ++i)
	{
		delete (*i);
	}
	mElements.clear();
	mVertexStride = 0;
}

void VertexDeclaration::AddVertexElement(ELEMENT_USAGE usage, ELEMENT_TYPE type, uint32_t usage_index)
{
	VertexElement *new_element = new VertexElement;
	new_element->type = type;
	new_element->usage = usage;
	new_element->usage_index = usage_index;

	mElements.push_back(new_element);

	CalculateStride();
}

void VertexDeclaration::InsertVertexElement(uint32_t i, ELEMENT_USAGE usage, ELEMENT_TYPE type, uint32_t usage_index)
{
	VertexElement *new_element = new VertexElement;
	new_element->type = type;
	new_element->usage = usage;
	new_element->usage_index = usage_index;

	//todo : optimize
	std::vector<VertexElement*>::iterator iter = mElements.begin();
	for (uint32_t a = 0; a < i; ++a)
		++iter;
	
	mElements.insert(iter, new_element);

	CalculateStride();
}

uint32_t VertexDeclaration::GetElementIndexByUsage(ELEMENT_USAGE usg, uint32_t usg_i)
{
	uint32_t i = 0;
	for (; i<mElements.size(); ++i)
	{
		if (mElements[i]->usage == usg && mElements[i]->usage_index == usg_i)
			break;
	}

	if (i == mElements.size())
	{
		LogManager::GetInstance()->AppendToLog("VertexDeclaration::GetElementIndexByUsage(), usage and usage index do not exist");
	}
	return i;
}

void VertexDeclaration::RemoveVertexElement(uint32_t index)
{
	//todo : optimize
	std::vector<VertexElement*>::iterator iter = mElements.begin();
	for(uint32_t a = 0; a < index; a++)
		iter++;
	
	delete	(*iter);
	mElements.erase(iter);

	CalculateStride();
}

void VertexDeclaration::RemoveVertexElement(VertexElement *element)
{
	//todo : optimize
	std::vector<VertexElement*>::iterator iter = mElements.begin();
	for (;(*iter)!=element && iter!=mElements.end();  ++iter);

	if (iter == mElements.end())	return;

	delete	(*iter);
	mElements.erase(iter);

	CalculateStride();
}

void VertexDeclaration::RemoveAllVertexElements()
{
	for (std::vector<VertexElement*>::iterator i=mElements.begin(); i!=mElements.end(); ++i)
	{
		delete	(*i);
	}
	mElements.clear();
	mVertexStride = 0;
}

uint32_t VertexDeclaration::CalculateStride()
{
	mVertexStride = 0;

	for (std::vector<VertexElement*>::iterator i=mElements.begin(); i!=mElements.end(); ++i)
	{
		mVertexStride += GetElementTypeSize( (*i)->type );
	}

	return mVertexStride;
}

uint32_t VertexDeclaration::GetElementTypeSize(ELEMENT_TYPE type)
{
	static uint32_t s[NUM_ELEMENT_TYPES] =
	{
		sizeof(float),
		2*sizeof(float), 
		3*sizeof(float),
		4*sizeof(float),
		4
	};

	if (type>=0 && type<NUM_ELEMENT_TYPES)
		return s[type];

	return 0;
}


END_ENGINE_NAMESPACE