
#include "cUnit.h"
#include <iostream>
using namespace std;

UpdateUnitFunc cUnit::func = nullptr;

void cUnit::UnitFunction()
{
  cout << "Unit function!\n";
}