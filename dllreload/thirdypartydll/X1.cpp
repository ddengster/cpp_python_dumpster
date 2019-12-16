
#include <Windows.h>
#include "X1.h"

#include <iostream>

using namespace std;
int val = 33;

__declspec(dllexport) void ABCLala()
{
  cout << "ABC aa says: " << val << endl;
  val = val + 2;
}

BOOL WINAPI DllMain(
  _In_ HINSTANCE hinstDLL,
  _In_ DWORD     fdwReason,
  _In_ LPVOID    lpvReserved
  )
{
  return 1;
}
