#pragma once

#include "VectorMath.h"
#include <vector>

typedef unsigned int uint;

//predecl
template <typename DataType>
class Map2D;

template <typename DataType>
class Map2DRectView;

// 2D Grid Map, coordinate space: [0-width, 0-height].
// - Generate frames/views and apply changes relative to those views. Excellent for procgen algos that apply to a subset of a map
// - (?) Handle views that go out of this map piece
// - (?) Underlying representation + scaling
template <typename DataType>
class Map2D
{
public:
  Map2D() { }

  Map2D(uint width, uint height, Vector2I origin = Vector2::ZERO)
    :mWidth(width), mHeight(height), mData(new DataType[width * height]), mWorldOrigin(origin)
  {
    memset(mData, 0, sizeof(DataType) * width * height);
  }
  Map2D(DataType* data, uint width, uint height, Vector2I origin = Vector2::ZERO)
    :mWidth(width), mHeight(height), mData(new DataType[width * height]), mWorldOrigin(origin)
  {
    memcpy(mData, data, sizeof(DataType) * width * height);
  }
  ~Map2D()
  {
    delete[] mData;
    mData = nullptr;
  }

  //access, coordinate space: [0-width, 0-height]
  void Blit(uint x, uint y, DataType dat) 
  {
    if (x >= mWidth || y >= mHeight)
      return;
    mData[x + y * mWidth] = dat; 
  }
  DataType Get(uint x, uint y)
  {
    if (x >= mWidth || y >= mHeight)
      return;
    return mData[x + y * mWidth];
  }
  DataType& GetRef(uint x, uint y)
  {
    if (x >= mWidth || y >= mHeight)
      return;
    return mData[x + y * mWidth];
  }
  void Blit(Vector2I pos, DataType dat) { Blit((uint)pos.x, (uint)pos.y, dat); }

  void WorldSpaceBlit(int x, int y, DataType dat)
  {
    x += -mWorldOrigin.x;
    y += -mWorldOrigin.y;
    if (x < 0 || x >= (int)mWidth || y < 0 || y >= (int)mHeight)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return;
    }
    Blit((uint)x, (uint)y, dat);
  }
  DataType WorldSpaceGet(int x, int y)
  {
    x += -mWorldOrigin.x;
    y += -mWorldOrigin.y;
    if (x < 0 || x >= (int)mWidth || y < 0 || y >= (int)mHeight)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return DataType(0);
    }
    return Get((uint)x, (uint)y);
  }
  DataType& WorldSpaceGetRef(int x, int y)
  {
    x += -mWorldOrigin.x;
    y += -mWorldOrigin.y;
    if (x < 0 || x >= (int)mWidth || y < 0 || y >= (int)mHeight)
      assert(0 && "Limits exceeded");
    //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
    return GetRef((uint)x, (uint)y);
  }
  void WorldSpaceBlit(Vector2I pt, DataType dat) { WorldSpaceBlit(pt.x, pt.y, dat); }
  DataType  WorldSpaceGet(Vector2I pt)           { return WorldSpaceGet(pt.x, pt.y); }
  DataType& WorldSpaceGetRef(Vector2I pt)        { return WorldSpaceGetRef(pt.x, pt.y); }


  //Setting up views/frames
  Map2DRectView<DataType> TempView(uint origin_x, uint origin_y, uint width, uint height)
  {
    return Map2DRectView<DataType>(Vector2I(origin_x, origin_y), width, height, this);
  }
  Map2DRectView<DataType> TempView(Vector2I origin, uint width, uint height)
  {
    return Map2DRectView<DataType>(origin, width, height, this);
  }
  Map2DRectView<DataType> WorldSpaceTempView(Vector2I world_origin, uint width, uint height)
  {
    Vector2I model_space_origin = world_origin - mWorldOrigin;
    return TempView(model_space_origin, width, height);
  }

  Map2DRectView<DataType>* View(const char* name, Vector2I origin, uint width, uint height)
  {
    for (uint i=0; i<mSavedViews.size(); ++i)
    {
      if (strcmp(mSavedViews[i].mName, name) == 0)
        return &mSavedViews[i];
    }
    SavedView sv;
    sv.mName = name;
    sv.mView = Map2DRectView<DataType>(origin, width, height, this);
    mSavedViews.push_back(sv);
    return &mSavedViews[mSavedViews.size() - 1];
  }
  Map2DRectView<DataType>* WorldSpaceView(const char* name, Vector2I world_origin, uint width, uint height)
  {
    Vector2I model_space_origin = world_origin - mWorldOrigin;
    return View(name, model_space_origin, width, height);
  }
  Map2DRectView<DataType>* GetView(const char* name)
  {
    for (uint i = 0; i < mSavedViews.size(); ++i)
    {
      if (strcmp(mSavedViews[i].mName, name) == 0)
        return &mSavedViews[i];
    }
    return nullptr;
  }

  // more advanced applications
  void BlitRect(uint patch_x, uint patch_y, uint inputwidth, uint inputheight, DataType* inputdata, bool botleft = true)
  {
    uint botleft_x = patch_x, botleft_y = patch_y;
    if (!botleft)
    {
      int shiftx = (int)inputwidth / 2, shifty = (int)inputheight / 2;
      if (shiftx > (int)botleft_x || shifty > (int)botleft_y)
      {
        assert(0 && "Out of range");
        return;
      }
      botleft_x -= shiftx;
      botleft_y -= shifty;
    }

    uint topright_x = botleft_x + inputwidth, topright_y = botleft_y + inputheight;
    if (botleft_x >= mWidth || botleft_y >= mHeight || topright_x > mWidth || topright_y > mHeight ||
      botleft_x < 0 || botleft_y < 0 || topright_x < 0 || topright_y < 0)
    {
      assert(0 && "Out of range");
      return;
    }
    for (uint j = 0; j < inputheight; ++j)
    {
      memcpy(&mData[botleft_x + (botleft_y + j) * mWidth], &inputdata[j * inputwidth],
        sizeof(DataType) * inputwidth);
    }
  }
  void BlitRect(Vector2I patch_origin, uint inputwidth, uint inputheight, DataType* inputdata, bool botleft = true)
  {
    BlitRect((uint)patch_origin.x, (uint)patch_origin.y, inputwidth, inputheight, inputdata, botleft); 
  }
  void WorldSpaceBlitRect(Vector2I worldspace_patch_origin, uint inputwidth, uint inputheight, DataType* inputdata, bool botleft = true)
  {
    Vector2I model_space_origin = worldspace_patch_origin - mWorldOrigin;
    BlitRect((uint)model_space_origin.x, (uint)model_space_origin.y, inputwidth, inputheight, inputdata, botleft);
  }

  bool ValidateLocation(Vector2I coord) { return coord.x >= mWidth || coord.y >= mHeight; }
  Vector2I ConvertToArrayLoc(Vector2I frame_loc)   { return frame_loc - mWorldOrigin; }
  Vector2I ConvertFromArrayLoc(Vector2I array_loc) { return array_loc + mWorldOrigin; }
public:
  DataType* mData = nullptr;
  uint mWidth  = 0;
  uint mHeight = 0;

  Vector2I mWorldOrigin;
  struct SavedView
  {
    const char* mName = nullptr;
    Map2DRectView<DataType> mView;
  };
  std::vector<SavedView> mSavedViews;
};

template <typename DataType>
class Map2DRectView
{
public:
  Map2DRectView(Vector2I origin, uint width, uint height, Map2D<DataType>* parent)
    :mOrigin(origin), mWidth(width), mHeight(height), mParent(parent)
  {
    //@note: guide to hitting edges: Don't hit parent's edges, but ok to hit current View's edge
    Vector2I topright = mOrigin + Vector2I(mWidth, mHeight);
    if (mOrigin.x < 0 || (uint)mOrigin.x >= mParent->mWidth || mOrigin.y < 0 || (uint)mOrigin.y >= mParent->mHeight ||
      topright.x < 0 || (uint)topright.x >= mParent->mWidth || topright.y < 0 || (uint)topright.y >= mParent->mHeight)
      assert(0 && "Out of range!");
  }

  //coordinates based off this new frame
  void Blit(uint x, uint y, DataType dat)
  {
    if (x > mWidth || y > mHeight)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return;
    }
    x += mOrigin.x;
    y += mOrigin.y;
    mParent->mData[x + y * mParent->mWidth] = dat;
  }
  DataType Get(uint x, uint y)
  {
    if (x > mWidth || y > mHeight)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return DataType(0);
    }
    x += -mOrigin.x;
    y += -mOrigin.y;
    return mParent->mData[x + y * mParent->mWidth];
  }
  DataType& GetRef(uint x, uint y)
  {
    if (x > mWidth || y > mHeight)
      assert(0 && "Limits exceeded");
    //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
    x += -mOrigin.x;
    y += -mOrigin.y;
    return mParent->mData[x + y * mParent->mWidth];
  }

  //Vector2I inputs
  void Blit(Vector2I pt, DataType dat) { Blit((uint)pt.x, (uint)pt.y, dat); }
  DataType Get(Vector2I pt)     { return Get((uint)pt.x, (uint)pt.y); }
  DataType& GetRef(Vector2I pt) { return GetRef((uint)pt.x, (uint)pt.y); }

  void WorldSpaceBlit(int x, int y, DataType dat) 
  {
    Vector2I loc = mParent->ConvertToArrayLoc(Vector2I(x, y));
    Vector2I framemin = mOrigin;
    Vector2I framemax = mOrigin + Vector2I(mWidth, mHeight);
    if (loc.x < framemin.x || loc.x > framemax.x || loc.y < framemin.y || loc.y > framemax.y)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return;
    }
    mParent->mData[loc.x + loc.y * mParent->mWidth] = dat;
  }
  void WorldSpaceBlit(Vector2I pt, DataType dat)
  { WorldSpaceBlit(pt.x, pt.y, dat); }

  // advanced
  void BlitRect(Vector2I patch_origin, uint inputwidth, uint inputheight, DataType* inputdata)
  { BlitRect((uint)patch_origin.x, (uint)patch_origin.y, inputwidth, inputheight, inputdata); }
  void BlitRect(uint patch_x, uint patch_y, uint inputwidth, uint inputheight, DataType* inputdata)
  {
    //patch within this given frame!
    uint topright_x = patch_x + inputwidth, topright_y = patch_y + inputheight;
    if (patch_x < 0 || patch_x >= mWidth || patch_y < 0 || patch_y >= mHeight ||
      topright_x > WidthPts() || topright_y > HeightPts())
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return;
    }

    mParent->BlitRect(patch_x + (uint)mOrigin.x, patch_y + (uint)mOrigin.y, inputwidth, inputheight, inputdata);
  }

  void WorldSpaceBlitRect(Vector2I worldspaceloc, uint inputwidth, uint inputheight, DataType* inputdata)
  {
    if (inputwidth <= 0 || inputheight <= 0)
      return;
    Vector2I rect_botleft = worldspaceloc - mParent->mWorldOrigin;
    Vector2I rect_topright = Vector2I(worldspaceloc.x + (inputwidth - 1), worldspaceloc.y + (inputheight - 1)) - mParent->mWorldOrigin;
    Vector2I framemin = mOrigin;
    Vector2I framemax = mOrigin + Vector2I(mWidth, mHeight);
    if (rect_botleft.x < framemin.x || rect_botleft.x > framemax.x || rect_botleft.y < framemin.y || rect_botleft.y > framemax.y ||
      rect_topright.x < framemin.x || rect_topright.x > framemax.x || rect_topright.y < framemin.y || rect_topright.y > framemax.y)
    {
      assert(0 && "Limits exceeded");
      //assert(0, "Point (%d, %d) exceeded limits (width: %d, height %d)", x, y, mWidth, mHeight);
      return;
    }

    mParent->BlitRect(rect_botleft, inputwidth, inputheight, inputdata);
  }

  bool ViableLocation(Vector2I p) { return !(p.x < 0 || p.x >= (int)mWidth || p.y < 0 || p.y >= (int)mHeight);  }
  
  
  Vector2I mOrigin = Vector2I(0, 0); //within parent's model space, range within [0, width/height of parent)
  uint mWidth = 0;  //length
  uint mHeight = 0; //length
  uint WidthPts()  { return mWidth + 1; }
  uint HeightPts() { return mHeight + 1; }
  Map2D<DataType>* mParent = nullptr;
};
