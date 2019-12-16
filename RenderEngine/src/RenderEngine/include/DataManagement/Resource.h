
#ifndef _RESOURCE_H
#define _RESOURCE_H

#include "Prerequisites.h"

DECLARE_ENGINE_NAMESPACE

class Resource
{
protected:
	String mName;
	String mPath;
	String mFullName;

private:
	uint32_t mRefCount;

public:
	Resource()
	{
	}

	virtual ~Resource()
	{
	}

	/** For engine's usage of memory pools. */
	virtual void Initialize(const String &name, const String& path)
	{
		//Safety checks
		if(!name.empty())
			mName = name;
		if(!path.empty())	
			mPath = path;

		if (!name.empty() && !path.empty())
			mFullName = path + name;

		mRefCount = 1;
	}

	/** For engine's usage of memory pools. */
	virtual void Destroy()
	{
	}

	void		IncRefCount()	{	++mRefCount;	}
	void		DecRefCount()	{	if (mRefCount>0) --mRefCount;	}
	uint32_t	RefCount()		{	return mRefCount;	}

	const String&	GetName()		{	return mName;		}
	const String&	GetFullName()	{	return mFullName;	}
	const String&	GetPath()		{	return mPath;		}
};

END_ENGINE_NAMESPACE

#endif
