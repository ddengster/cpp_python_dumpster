
#ifndef _APPMAIN_H
#define _APPMAIN_H

#include "Headers.h"

class AppMain
{
public:
	
	virtual ~AppMain();

	static AppMain* GetInstance();

	void Init(ddengine_RenderEngine::RenderSetup rs);
	void Shutdown();

	void Loop(float timeElapsed);

private:
	AppMain();
};

#endif