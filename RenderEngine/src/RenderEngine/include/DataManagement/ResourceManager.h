
#ifndef _RESOURCEMANAGER_H
#define _RESOURCEMANAGER_H

#include "Prerequisites.h"
#include "DataManagement/HashTable_Str.h"
#include <algorithm>
//for inclusion
#include "DataManagement/Resource.h"

DECLARE_ENGINE_NAMESPACE

/**
 * A Manager to manage your resources.
 * <class Res> should be derived from class Resource.
 * Recommended to use wrapper functions if you need to delete/free/release resources.
 */
template<class Res> class ResourceManager
{
protected:
	HashTableStr<Res> *m_resourceHash;

	//A std::vector to keep track of all Resource names.
	std::vector<String> m_ResourceList;

	std::vector<String>& GetResourceNameList() { return m_ResourceList; } 

	ResourceManager(int32_t hashSize = 0)
	{
		m_resourceHash = NULL;

		//sizecheck + initialize hashtable
		if(hashSize > 0)
			m_resourceHash = new HashTableStr<Res>(hashSize);
	}

	virtual ~ResourceManager()
	{
		DestroyAllResources();
		SAFE_DELETE(m_resourceHash);
	}

	/**
	 * Gets a resource using its name.
	 */
	Res* GetResource(const String &name)
	{
		HashItemStr<Res> item = m_resourceHash->Find(name);
		if (item.GetKey() != "")
			return item.GetObject();
		return NULL;
	}

	/**
	 * Add a resource. If the resource already exists, increase its reference count.
	 */
	void AddResource(Res* res)
	{
		if (res == NULL)
			return;

		String name = res->GetName();

		if (GetResource(name) == NULL)
		{
			HashItemStr<Res> item;
			item.SetKey(name);
			item.SetObj(res);

			if (m_resourceHash->Insert(item))
				m_ResourceList.push_back(name);
		}
		else
			res->IncRefCount();
	}

	/**
	 * Destroy the resource via name, no matter what the reference count is.
	 * \param restoDestroy	A pointer reference to a temporary instance of Res. If it is 
	 *						non-zero, the resource should be destroyed via your own means,
	 *						else there is no need to destroy it.
	 */
	bool DestroyResource(const String &name, Res*& restoDestroy)
	{
		if (m_ResourceList.empty())
			return false;

		Res* tempres = 0;
		if (m_resourceHash->Erase(name, tempres))
		{
			std::vector<String>::iterator result = std::find(m_ResourceList.begin(), m_ResourceList.end(), name);
			if (result != m_ResourceList.end())
				m_ResourceList.erase(result);
		}
		restoDestroy = tempres;
		return true;
	}

	/**
	 * Destroy the specified resource, no matter what the reference count is.
	 * \param restoDestroy	A pointer reference to a temporary instance of Res. If it is 
	 *						non-zero, the resource should be destroyed via your own means,
	 *						else there is no need to destroy it.
	 */
	bool DestroyResource(Res* res, Res*& restoDestroy)
	{
		if (res == NULL || m_ResourceList.empty())
			return false;
		String name = res->GetName();
		restoDestroy = 0;

		if (m_resourceHash->Erase(name, res)) //TODO: Replace with delete?
		{
			std::vector<String>::iterator result = std::find(m_ResourceList.begin(), m_ResourceList.end(), name);
			if (result != m_ResourceList.end())
				m_ResourceList.erase(result);
			restoDestroy = res;
		}
		else 
			return false;
		return true;
	}

	/**
	 * Reduces the reference count of the resource found via name. If the reference count is 0, destroy the resource.
	 * \param restoDestroy	A pointer reference to a temporary instance of Res. If it is 
	 *						non-zero, the resource should be destroyed via your own means,
	 *						else there is no need to destroy it.
	 */
	bool RemoveResource(const String &name, Res*& restoDestroy)
	{
		if (m_ResourceList.empty())
			return false;

		restoDestroy = 0;
		Res* res = GetResource(name);
		if (!res)
			return false;
		
		res->DecRefCount();

		if (res->RefCount() == 0)
		{
			m_resourceHash->Erase(name, res);

			std::vector<String>::iterator result = std::find(m_ResourceList.begin(), m_ResourceList.end(), name);
			if (result != m_ResourceList.end())
				m_ResourceList.erase(result);
			restoDestroy = res;
		}
		return true;
	}

	/**
	 * Reduces the reference count of the specified resource. If the reference count is 0, destroy the resource.
	 * \param restoDestroy	A pointer reference to a temporary instance of Res. If it is 
	 *						non-zero, the resource should be destroyed via your own means,
	 *						else there is no need to destroy it.
	 */
	bool RemoveResource(Res* res, Res*& restoDestroy)
	{
		if (res == NULL || m_ResourceList.empty())
			return false;

		restoDestroy = 0;
		res->DecRefCount();

		if (res->RefCount() == 0)
		{
			String name = res->GetName();
			if (m_resourceHash->Erase(name, res))
			{
				std::vector<String>::iterator result = std::find(m_ResourceList.begin(), m_ResourceList.end(), name);
				if (result != m_ResourceList.end())
					m_ResourceList.erase(result);
				restoDestroy = res;
			}
			else
				return false;
		}
		return true;
	}

	/**
	 * Destroy all resources. 
	 * This function should not be used unless you have no need to properly destroy your resources.
	 */
	void DestroyAllResources()
	{
		Res* temp = 0;
		for (int32_t i=0; i<(int32_t)m_ResourceList.size(); ++i)
		{
			DestroyResource(m_ResourceList[i], temp);
		}
		m_ResourceList.clear();
	}
};

END_ENGINE_NAMESPACE

#endif