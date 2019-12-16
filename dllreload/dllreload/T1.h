
#pragma once
#include <vector>

#include "../driver_app/ReloadAPI.h"

class TempSystem;
extern "C" __declspec(dllexport) void PureFunction1();
extern "C" __declspec(dllexport) void* BuildRecords(sReloadAPI* api);
extern "C" __declspec(dllexport) void* PtrRetFunction();
extern "C" __declspec(dllexport) void* P1(TempSystem* w);

extern "C" __declspec(dllexport) void* Run(void* input);

class TempSystem
{
public:
  void F();
  std::vector<int> mVec;
};