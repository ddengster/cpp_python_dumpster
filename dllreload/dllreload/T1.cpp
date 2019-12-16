

#include <iostream>
#include <atomic>
#include "T1.h"
#include "../thirdypartydll/X1.h"
//#include "../thirdypartydll/X1.cpp"
#include "../driver_app/cUnit.h"
#include "../driver_app/cUnit.cpp"

using namespace std;

extern "C" __declspec(dllexport) void PureFunction1()
{
  cout << "YOOOOOOOOSupsss!\n";
}


static int asd = 22; //Static/globals values will be re-initialized (you may/maynot want that effect)



extern "C" __declspec(dllexport) void* P1(TempSystem* w)
{
  w->mVec.push_back(1);
  w->mVec.push_back(2);
  w->mVec.push_back(2);
  return w;
}

class ASD
{
public:
  int b = 100;
  int c = 2000000;
  static int a;
};

int ASD::a = 221; //same with the above, resets on reload

class iBase
{
public:
  virtual void ok() = 0;
};

class QBase : public iBase
{
public:
  virtual void ok() { cout << "a qbaeese\n"; }
  virtual void ok(int n) { cout << "qbase" << n << endl; }
};

class ZBase : public iBase
{
public:
  virtual void ok() { cout << "a zbase aa\n"; }
};

iBase* b = nullptr;

typedef void(*PureFunc)();
static PureFunc f = nullptr;

extern "C" __declspec(dllexport) void* BuildRecords(sReloadAPI* api)
{
  //cout << "zz:" << asd << "ADS\n";
  //asd = 3;
  *api->mFunc = [](cUnit* a)
  {
    cout << "Unit val: " << a->val + 13 << endl;
    a->UnitFunction();
    //a->UnitFunction();
  };

  cout << "Building records" << endl;
  //b = new QBase();
  b = new ZBase();

  f = []()
  {
    cout << "LAMBDA CALLED lalala" << endl;
  };

  
  return b;
}

static PureFunc f2 = nullptr;

void LibFunc()
{
  static std::atomic<int> counter = 0;
  counter++;
  cout << counter << endl;
  
}

void otherfunc()
{
  int k = 222;
  cout << "ab " << k << endl;
}

extern "C" __declspec(dllexport) void* Run(void* input)
{
  otherfunc();
  //cout << "Input Received dd aa: " << (int)input << endl;
  //cout << "b  a is: " << b << endl;
  /*
  b = new ZBase();
  b->ok();
  LibFunc();
  LibFunc();

  */
  /*
  LibFunc();
  ABCLala();
  */
  /*
  if ((int)input == 7)
    f2 = f;
  if ((int)input == 8)
  {
    if (f2)
      f2();
    else
      cout << "f2 not found";
  }
  else if ((int)input == 9)
  {
    f();
  }
  */
  //ABCLala();
  typedef void(*PureFunc)();

  PureFunc* f = (PureFunc*)input;
  *f = []()
  {
    char b[512 * 560] = { 0 };
    strcpy_s(b, "lalal");
    char a[512] = { 0 };
    strcpy_s(a, "lalal");
    cout << "test12322\n" << a << endl << b << endl;
  };
#if 0
  TempSystem* w = (TempSystem*)input;
  w->mVec.push_back(333);
  for (auto it : w->mVec)
    cout << it << " ";
  cout << endl;
  w->F();
#endif
  //iBase* b = (iBase*)input;
  //b->ok();
  //ABC();
  //new QBase();
  /*
  ASD s;
  cout << "HEYsss" << s.c << " " << ASD::a << "\n";
  //cout << "HEYsss\n";
  ASD::a = 2;
  */
  return nullptr;
  
}

void TempSystem::F()
{
  cout << "33FFF\n";
}