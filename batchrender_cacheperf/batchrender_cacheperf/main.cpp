
#include <iostream>
#include "Timer.h"
#include <vector>
#include <Windows.h>
#include <algorithm>
#include "data.h"


/*
  {
  if ((mRenderFlags & VISIBLE_RFLAG) == 0)
    return;

  if (mMeshCount == 0)
    return;
  
  bool skip_pernodetransform = false; //@note: temp hack
  if (mAnimContext) //not using skinned shader, so no bones
  {
    if (setworldmtx)
      renderer->SetMatrix(WORLD_MATRIX, mTransform);
    if (mBatchState->mMaterial->mSkinned == false)
      skip_pernodetransform = false;
    else
      skip_pernodetransform = true;
  }
  else
  {
    if (setworldmtx)
      renderer->SetMatrix(WORLD_MATRIX, mTransform);
  }

  if (mTextureAnimRange)
  { 
  }

  if (mInstanceBuffer && mMeshCount)
  {
    if (mPerObjCPUBuffer[0])
      renderer->FeedGPUBufferCPUData(mBatchState->mMaterial->GetPerObjectGPUBuffer(pass), 
        mPerObjCPUBuffer[0]);

    iVertexBuffer* vbufs[2];
    vbufs[0] = mMeshList[0]->GetVertexBuffer();
    vbufs[1] = mInstanceBuffer;
    renderer->DrawIndexedInstanced(PRIMITIVE_TRI_LIST, vbufs, mMeshList[0]->GetIndexBuffer(),
      mMeshList[0]->GetVerticesToRender(), 2, mInstanceCount);
  }
  else
  { 
    for (uint i = 0; i < mMeshCount; ++i)
    {
      //LOG("%s", mMeshList[i]->GetName().c_str());
      if (!skip_pernodetransform && mMeshList[i]->mNonBoneNodeIdx != -1)
      {
        if (setworldmtx)
        {
          Matrix4&& mtx = GetNodeWorldTransform(mMeshList[i]->mNonBoneNodeIdx);
          renderer->SetMatrix(WORLD_MATRIX, mtx);
        }
      }
      if (mPerObjCPUBuffer[i])
        renderer->FeedGPUBufferCPUData(mBatchState->mMaterial->GetPerObjectGPUBuffer(pass), 
          mPerObjCPUBuffer[i]);

      renderer->DrawMesh(mMeshList[i]);
    }
  }
}
  */
int main()
{
  TIMER_INIT;
  int a = 0, b = 5;
  a += 55;
  b = a + b - 2;
  
#define POOL_SIZE 1000
  GfxCmpNaive* cmp_pool1 = new GfxCmpNaive[POOL_SIZE];
  uint64 start, stop;
  {
    CPUIDMACRO;
    start = __rdtsc();
    for (int i = 0; i < (POOL_SIZE - 1); ++i)
    {
      //cmp_pool1->mSortVal = rand() % 100000;
      //cmp_pool1[i].mSortVal < cmp_pool1[i + 1].mSortVal
      if (cmp_pool1[i].mRenderFlags < 5)
        a += 5;
      if (cmp_pool1[i].mMeshCount < 5)
        b += 5;

      if (cmp_pool1[i].mAnimContext > 1)
        a += 154;
      if (cmp_pool1[i].mInstanceBuffer && cmp_pool1[i].mMeshCount < 5)
        b += 144;

      for (int i = 0; i < 1; ++i)
      {
        /*Matrix4&& mtx = GetNodeWorldTransform(cmp_pool1[i].mMeshList[i]->mNonBoneNodeIdx);
        renderer->SetMatrix(WORLD_MATRIX, mtx);*/

        if (cmp_pool1[i].mPerObjCPUBuffer[i])
        {
          a = cmp_pool1[i].mMaterial + 1;
          b *= cmp_pool1[i].mPerObjCPUBuffer[i];
        }

        a += cmp_pool1[i].mMeshes[i];
      }
    }
    stop = __rdtsc();
    printf("naive: %d\n", stop - start);
  }
  //std::sort(cmp_pool1, cmp_pool1 + POOL_SIZE, )


  //test 64bit pointers vs 32bit handles
  //Result 27/10/2018: Low correlation. The first test always seems to run faster, unreliable bad test?
  //Also, seems to be that if you miss just one cache line worth of stuff you get unreliable results
  MiniCmp* arr = new MiniCmp[POOL_SIZE * 2];
  PointerUser* f = new PointerUser[POOL_SIZE];
  HandleUser* f2 = new HandleUser[POOL_SIZE];
  printf("%d %d\n", sizeof(HandleUser), sizeof(PointerUser));
  for (int i = 0; i < POOL_SIZE; ++i)
  {
    f[i].MiniCmpPtr[0] = &arr[i];
    f[i].MiniCmpPtr[0]->b = 22;
    f2[i].MiniCmpHandle[0] = i + POOL_SIZE;
    int h = f2[i].MiniCmpHandle[0];
    arr[h].b = 42;
  }
  {
#if 0
    CPUIDMACRO;
    start = __rdtsc();
    for (int i = 0; i < POOL_SIZE; ++i)
    {
      f[i].MiniCmpPtr[0]->b = a + i;
    }
    uint64 stop = __rdtsc();
    printf("res1: %d\n", stop - start);
#endif
#if 1
    CPUIDMACRO;
    start = __rdtsc();
    for (int i = 0; i < POOL_SIZE; ++i)
    {
      int h = f2[i].MiniCmpHandle[0];
      arr[h].b = a + i;
    }
    stop = __rdtsc();
    printf("res2: %d\n", stop - start);
#endif
  }
  
  printf("\n\n%d %d", a, b);
  system("pause");
  return 0;
}