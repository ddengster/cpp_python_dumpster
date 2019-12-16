
#ifndef _FREELISTPOOL_H
#define _FREELISTPOOL_H

#include "Prerequisites.h"
#include <assert.h>

DECLARE_ENGINE_NAMESPACE

/** \ingroup ResourceManagement
 * @class FreeListPool
 *
 * Learnt from Game Programing Gems 4!
 * Memory pool for the templated class. Pre-allocates a number of objects, then hands them out during runtime.
 * Results in an increase in speed during runtime.
 * TODO: Add 2d capability to m_ObjData so we can extend the array of objects.
 * \Header
 */
template <class Object>
class FreeListPool
{
public:
	FreeListPool(uint32_t size)
	{
		assert(size);
		m_numElements = size;
		m_ObjData = new Object[size];
		m_PtrFreeObj = new Object*[size];

		FreeAll();
	}

	~FreeListPool()
	{
		FreeAll();
		
		if (m_ObjData)
			delete []m_ObjData;
		m_ObjData = 0;
		if (m_PtrFreeObj)
			delete []m_PtrFreeObj;
		m_PtrFreeObj = 0;
	}

	/**
	 * Fills m_PtrObj with pointers to m_ObjData, starting with the first pointer(m_PtrObj[0]) to the last object(m_ObjData[m_numElements-1]),
	 * and ending with the last pointer to the first object, and making m_Top be (m_numElements)
	 */
	void FreeAll()
	{
		uint32_t index = m_numElements - 1;
		for (m_Top=0; m_Top<m_numElements; m_Top++)
		{
			//m_ObjData[index].~Object();
			//std::cout << index << std::endl;
			m_PtrFreeObj[m_Top] = &(m_ObjData[index--]);
		}
	}

	/**
	 * Returns a pointer to the topmost object of PtrFreeObj.
	 */
	Object* NewInstance()
	{
		//make sure m_Top is non 0
		assert(m_Top);
		//TODO:: Reinitialize?
		
		return m_PtrFreeObj[--m_Top];
	}

	/**
	 * Marks the specified pointer as a free object.
	 */
	void FreeInstance(Object* ptr)
	{
		//make sure the ptr is between 1st and last elements
		assert( (ptr >= &(m_ObjData[0])) && (ptr <= &(m_ObjData[m_numElements-1])) );

		//make sure m_Top is less than the number of elements
		assert(m_Top < m_numElements);

		//put the ptr on top of the PtrFreeObj stack for reuse.
		m_PtrFreeObj[m_Top++] = ptr;

		//no need to deinitialize if u called this in the delete operator
		//ptr->~Object();
	}

protected:
	Object *m_ObjData;
	Object **m_PtrFreeObj;

	uint32_t m_Top;
	uint32_t m_numElements;
};


END_ENGINE_NAMESPACE

#endif