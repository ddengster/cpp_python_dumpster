#pragma once

typedef unsigned long long uint64;
class GfxCmpNaive
{
public:
  uint64 mSortVal;
  int       mRenderFlags = 0;
  int mSimpleAnimation = 1;
  float mSimpleAnimTimer = 0.f;
  float mAnimSpeed1 = 0.0625f, mAnimSpeed2 = 0.125f;
  float mCullingSphere[4];
  float mTransform[16];

  uint64 mMaterial = 0;
  uint64 mTextures[8];
  uint64 mMeshes[6];
  int mMeshCount = 0;

  uint64 mAnimContext = 0;
  uint64 mRuntimeNodeHierarchy = 0;
  uint64 mPerObjCPUBuffer[6];

  uint64 mTextureAnimRange = 0;

  uint64 mInstanceBuffer = 0;
  int   mInstanceCount = 0;

  float mTextureFrameIdx = 0.0f;
  uint64 mInstStandIn[6];
  uint64 mParent = 0;
};

/**************** 4byte handles vs 64bit pointers *******************************/
class MiniCmp
{
public:
  int b = 45;
  int b6[7];
};

class PointerUser
{
public:
  int d[8];
  MiniCmp* MiniCmpPtr[8];
};

class HandleUser
{
public:
  int d[8];
  int MiniCmpHandle[8];
};

class cBatchState
{
public:
  uint64 mMaterial = 0;
  uint64 mTextures[8];
};

class GfxCmp1
{
public:
  uint64 mSortVal;
  int       mRenderFlags = 0;
  int mSimpleAnimation = 1;
  float mSimpleAnimTimer = 0.f;
  float mAnimSpeed1 = 0.0625f, mAnimSpeed2 = 0.125f;
  float mCullingSphere[4];
  float mTransform[16];

  cBatchState* mBS = nullptr;
  uint64 mMeshes[6];
  int mMeshCount = 0;
};