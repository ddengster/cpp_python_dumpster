// array_vs_ll.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <intrin.h>
#include "classes.h"
#define CPUIDMACRO \
{ \
  int cpuidarr[4]; \
  __cpuid(cpuidarr, 0); \
}

using namespace std;
#if 0
class PureEmitter
{
public:
  char dummydata[264];

  void Update()
  {
    dummydata[1] = 10 + dummydata[144];
    dummydata[224] = 210 + dummydata[144];
    float* f = ((float*)dummydata) + 2;
    *f = 4.2f;
  }
};

class EmitterLL
{
public:
  char dummydata2[16];
  EmitterLL* mNext = nullptr;
  EmitterLL* mPrev = nullptr;
  char dummydata[264];

  void Update()
  {
    dummydata2[4] = 'c' + dummydata[10];
    dummydata[1] = 10 + dummydata[144];
    dummydata[224] = 210 + dummydata[144];
    float* f = ((float*)dummydata) + 2;
    *f = 4.2f;
  }
};

class Ent
{
public:
  char dummydata[112];
  PureEmitter* mEm[10];
  char dummydata2[86];

  void Update()
  {
    dummydata[30] = dummydata[45] + 10;
    dummydata[80] = dummydata[85] + 16;

    dummydata2[80] = dummydata[85] + 16;
    auto f = (float*)dummydata2;
    *f = 4.4f + 1.f;

    for (int i = 0; i < 10; ++i)
    {
      if (mEm[i])
        mEm[i]->Update();
    }
  }
};

class EntLL
{
public:
  char dummydata[112];
  EmitterLL* mEmitter = nullptr;
  char dummydata2[86];

  void Update()
  {
    dummydata[30] = dummydata[45] + 10;
    dummydata[80] = dummydata[85] + 16;

    dummydata2[80] = dummydata[85] + 16;
    auto f = (float*)dummydata2;
    *f = 4.4f + 1.f;

    auto pe = mEmitter;
    while (pe)
    {
      pe->Update();
      pe = pe->mNext;
    }
  }
};
#endif

#define ENTCOUNT 800
#define PREALLOC ENTCOUNT * 10
#define SAME_PE_COUNT 6
int main()
{
  //prealloc
  PureEmitter* pure_pool = new PureEmitter[PREALLOC * 2];
  int allocidx = 0;
  EmitterLL* ll_pool = new EmitterLL[PREALLOC];
  int allocidx2 = 0;

  EntLL* entsll = new EntLL[ENTCOUNT];
  for (int i = 0; i < ENTCOUNT; ++i)
  {
#ifdef SAME_PE_COUNT
    int pecount = SAME_PE_COUNT;
#else
    int pecount = rand() % 10;
#endif
    for (int j = 0; j < pecount; ++j)
    {
      auto newem = &ll_pool[allocidx2];
      ++allocidx2;

      if (entsll[i].mEmitter == nullptr)
        entsll[i].mEmitter = newem;
      else
      {
        auto pe = entsll[i].mEmitter;
        while (pe)
        {
          if (pe->mNext)
            pe = pe->mNext;
          else
          {
            pe->mNext = newem;
            newem->mPrev = pe;
            break;
          }
        }
      }
    }
  }


  Ent* ents = new Ent[ENTCOUNT];
  for (int i = 0; i < ENTCOUNT; ++i)
  {
    memset(ents[i].mEm, 0, sizeof(ents[i].mEm));
#ifdef SAME_PE_COUNT
    int pecount = SAME_PE_COUNT;
#else
    int pecount = rand() % 10;
#endif
    for (int j = 0; j < pecount; ++j)
    {
      ents[i].mEm[j] = &pure_pool[allocidx];
      ++allocidx;
    }
  }

  EntDyn* entsdyn = new EntDyn[ENTCOUNT];
  for (int i = 0; i < ENTCOUNT; ++i)
  {
#ifdef SAME_PE_COUNT
    int pecount = SAME_PE_COUNT;
#else
    int pecount = rand() % 10;
#endif
    entsdyn[i].mCount = pecount;
    entsdyn[i].mEm = new PureEmitter*[entsdyn[i].mCount];
    //int pecount = 1;
    for (int j = 0; j < pecount; ++j)
    {
      entsdyn[i].mEm[j] = &pure_pool[allocidx];
      ++allocidx;
    }
  }

  EntStatic* entsstatic = new EntStatic[ENTCOUNT];
  for (int i = 0; i < ENTCOUNT; ++i)
  {
#ifdef SAME_PE_COUNT
    int pecount = SAME_PE_COUNT;
#else
    int pecount = rand() % 10;
#endif
    for (int j = 0; j < pecount; ++j)
      entsstatic[i].mEm[j].dummydata[0] = 'c';
  }

  unsigned long long start, end;

  CPUIDMACRO;
  //fixed ptr arrays
  start = __rdtsc();
  {
    for (int i = 0; i < ENTCOUNT; ++i)
      ents[i].Update();
  }
  end = __rdtsc();
  cout << end - start << " fix ptr array" << endl;

  CPUIDMACRO;
  //dyn arrays
  start = __rdtsc();
  {
    for (int i = 0; i < ENTCOUNT; ++i)
      entsdyn[i].Update();
  }
  end = __rdtsc();
  cout << end - start << " dyn array" << endl;


  CPUIDMACRO;
  //dyn arrays
  start = __rdtsc();
  {
    for (int i = 0; i < ENTCOUNT; ++i)
      entsstatic[i].Update();
  }
  end = __rdtsc();
  cout << end - start << " static array" << endl;

  CPUIDMACRO;
  //preallocated linked list
  start = __rdtsc();
  {
    for (int i = 0; i < ENTCOUNT; ++i)
      entsll[i].Update();
  }
  end = __rdtsc();
  cout << end - start << " linkedlist" << endl;

  return 0;
  /****
  @conclusions 27th/3/2018
  1) Prealloced Linked List
  - Definition: Nodes likely to be contiguous in memory, linked list style iteration. Advantages: Minimimal data wastage
  - Reason: Optimization by compiler.
  2) Static Array
  - Definition: Fixed array of non-pointer data structures. Disadvantage: data wastage
  - 
  3) Dyn Array
  - Definition: Allocated ptr array, then assign preallocated objects.
  4) Fix Ptr Array (neck in neck with #3). 
  - 
  - Reason: unable to optimize, does call Ent::Update (013F291B10h)  

  @conclusions on 25th/3/2018
  Linkedlist style trump fixed array 3, being 3 times faster.
  The compiler cannot seem to optimize ents[i].Update() past class/function boundaries, while
  you can do it with entsll.

  **/
}

