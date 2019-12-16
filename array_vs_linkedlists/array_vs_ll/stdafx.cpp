// stdafx.cpp : source file that includes just the standard includes
// array_vs_ll.pch will be the pre-compiled header
// stdafx.obj will contain the pre-compiled type information

#include "stdafx.h"
#include "classes.h"
// TODO: reference any additional headers you need in STDAFX.H
// and not in this file

__declspec(noinline) static char A(char p)
{
  static int f = 15;
  f += p;
  return (char)f;
}

void PureEmitter::Update()
{
  dummydata[1] = 10 + dummydata[144];
  dummydata[224] = 210 + dummydata[144];
  float* f = ((float*)dummydata) + 2;
  *f = 4.2f;

  /*dummydata[155] = 4;
  dummydata[13] = A(dummydata[155]);*/
}

void EmitterLL::Update()
{
  dummydata2[4] = 'c' + dummydata[10];
  dummydata[1] = 10 + dummydata[144];
  dummydata[224] = 210 + dummydata[144];
  float* f = ((float*)dummydata) + 2;
  *f = 4.2f;

  /*float* f2 = ((float*)dummydata) + 2;
  *f2 = *f + 4.2f;*/
}

void Ent::Update()
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

void EntLL::Update()
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

void EntDyn::Update()
{
  dummydata[30] = dummydata[45] + 10;
  dummydata[80] = dummydata[85] + 16;

  dummydata2[80] = dummydata[85] + 16;
  auto f = (float*)dummydata2;
  *f = 4.4f + 1.f;

  for (int i = 0; i < mCount; ++i)
  {
    mEm[i]->Update();
  }
}

void EntStatic::Update()
{
  dummydata[30] = dummydata[45] + 10;
  dummydata[80] = dummydata[85] + 16;

  dummydata2[80] = dummydata[85] + 16;
  auto f = (float*)dummydata2;
  *f = 4.4f + 1.f;

  for (int i = 0; i < 10; ++i)
  {
    if (mEm[i].dummydata[0] == 'c')
      mEm[i].Update();
  }
}
