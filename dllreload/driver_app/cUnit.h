
#pragma once

#include "ReloadAPI.h"

class __declspec(dllexport) cUnit
{
public:
  int val = 10;

  static UpdateUnitFunc func;

  void UnitFunction();
};