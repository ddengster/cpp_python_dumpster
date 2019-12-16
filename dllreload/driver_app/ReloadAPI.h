
#ifndef _RELOADAPI_H
#define _RELOADAPI_H

class cUnit;

typedef void(*UpdateUnitFunc)(cUnit* m);

struct sReloadAPI
{
  UpdateUnitFunc* mFunc = nullptr;
};

void ReloadAPI(sReloadAPI* api);

#endif
