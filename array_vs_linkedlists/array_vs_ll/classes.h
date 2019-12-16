#pragma once

class PureEmitter
{
public:
  char dummydata[564];

  void Update();
};

class EmitterLL
{
public:
  char dummydata2[16];
  EmitterLL* mNext = nullptr;
  EmitterLL* mPrev = nullptr;
  char dummydata[264];

  void Update();
};

class Ent
{
public:
  char dummydata[112];
  PureEmitter* mEm[10];
  char dummydata2[86];

  void Update();
};

class EntLL
{
public:
  char dummydata[112];
  EmitterLL* mEmitter = nullptr;
  char dummydata2[86];

  void Update();
};

class EntDyn
{
public:
  char dummydata[112];
  int mCount = 0;
  PureEmitter** mEm = nullptr;
  char dummydata2[86];

  void Update();
};

class EntStatic
{
public:
  char dummydata[112];
  PureEmitter mEm[10];
  char dummydata2[86];

  void Update();
};