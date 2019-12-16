
#include <Windows.h>
#include <iostream>
#include "../dllreload/T1.h"
#include "../thirdypartydll/X1.h"

#include "ReloadAPI.h"
#include "cUnit.h"

using namespace std;

/*
void ReloadAPI(sReloadAPI* api)
{
  cUnit::func = api->mFunc;
}
*/

/*extern "C" */typedef void(*PureFunc)();
// void PureFunction1();
typedef void* (*P1Func)(TempSystem* w);
typedef void* (*PtrRetFunc)();
typedef void* (*RunFunc)(void* input);

typedef void (*BuildRecordsFunc)(sReloadAPI* api);

struct win32_game_code
{
  HMODULE GameCodeDLL = 0;
  FILETIME DLLLastWriteTime;

  /*
  game_update_and_render *UpdateAndRender;
  game_get_sound_samples *GetSoundSamples = nu;
  */
  PureFunc mToRun = nullptr;
  BuildRecordsFunc mBuildRecords = nullptr;
  P1Func mPtrRet = nullptr;
  RunFunc mRun = nullptr;
  bool IsValid = false;
};

int main()
{
  //run 

  auto GetLastWriteTime = [](char* filename) -> FILETIME
  {
    FILETIME LastWriteTime = {};

    WIN32_FILE_ATTRIBUTE_DATA Data;
    if (GetFileAttributesEx(filename, GetFileExInfoStandard, &Data))
      LastWriteTime = Data.ftLastWriteTime;

    return(LastWriteTime);
  };

  char lockfile[100] = "./lock.tmp";
  char sourcedll[100] = "./dllreload.dll";
  char tempsourcedll[100] = "./dllreloadtemp.dll";
  DWORD err = GetLastError();
  win32_game_code gamecode;
  auto LoadGameCode = [&lockfile, &sourcedll, &tempsourcedll, GetLastWriteTime]() -> win32_game_code
  {
    win32_game_code result;
    WIN32_FILE_ATTRIBUTE_DATA ignore;
    if (!GetFileAttributesEx(lockfile, GetFileExInfoStandard, &ignore))
    {
      DWORD err = GetLastError();
      result.DLLLastWriteTime = GetLastWriteTime(sourcedll);
      err = GetLastError();
      BOOL res = CopyFileA(sourcedll, tempsourcedll, FALSE);
      err =GetLastError();

      result.GameCodeDLL = LoadLibraryA(tempsourcedll);
      err = GetLastError();
      if (result.GameCodeDLL)
      {
        result.mToRun = (PureFunc)GetProcAddress(result.GameCodeDLL, "PureFunction1");
        result.mBuildRecords = (BuildRecordsFunc)GetProcAddress(result.GameCodeDLL, "BuildRecords");
        result.mPtrRet = (P1Func)GetProcAddress(result.GameCodeDLL, "P1");
        result.mRun = (RunFunc)GetProcAddress(result.GameCodeDLL, "Run");
        
        //result.IsValid = result.mToRun && result.mBuildRecords;
      }
    }
    return result;
  };
  sReloadAPI api;
  api.mFunc = &cUnit::func;

  void* record = nullptr;
  TempSystem tempsys;

  gamecode = LoadGameCode();
  if (gamecode.mBuildRecords)
    gamecode.mBuildRecords(&api);

  if (gamecode.mPtrRet)
  {
    record = gamecode.mPtrRet(&tempsys);
  }
  cout << "Init complete.\n";

  cUnit unit;
  
  int val = 0;
  while (1)
  {
    cout << "Hey"<< endl;

    bool exe_reloaded = false;

    FILETIME NewDLLWriteTime = GetLastWriteTime(sourcedll);
    if (CompareFileTime(&NewDLLWriteTime, &gamecode.DLLLastWriteTime) != 0) //new dll overwritten
    {
      //Win32CompleteAllWork(&HighPriorityQueue);
      //Win32CompleteAllWork(&LowPriorityQueue);

      //Win32UnloadGameCode(&Game);
      {
        //unload game code
        if (gamecode.GameCodeDLL)
        {
          FreeLibrary(gamecode.GameCodeDLL);
          gamecode.GameCodeDLL = 0;
        }
        gamecode.mToRun = nullptr;
        gamecode.IsValid = false;
      }
      gamecode = LoadGameCode();
      //NewInput->ExecutableReloaded = true;

      if (gamecode.mBuildRecords)
        gamecode.mBuildRecords(&api);
    }
    /*
    if (gamecode.mToRun)
      gamecode.mToRun();
      */
    if (gamecode.mRun)
    {
      //void* a = gamecode.mRun(record);
      PureFunc f = nullptr;
      void* a = gamecode.mRun(&f);
      //cUnit::func(&unit);
      if (f)
        f();
    }

    cout << "game version: ";
    ABCLala();
    cin >> val;
    //system("pause");
  }

  /*
  //Dll load functions

  //BuildMetaData();
  //Initialize All globals

  //build gameplay records

  while (1)
  {
    if (NeedLiveReload())
    {
    //dll load functions
    //build gameplay records
    }
    Run();
  }
  */
  return 0;
}