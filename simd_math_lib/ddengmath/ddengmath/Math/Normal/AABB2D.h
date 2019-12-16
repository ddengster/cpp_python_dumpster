
#ifndef _AABB2D_H
#define _AABB2D_H

#include "iBoundingVolume2D.h"

/*
Diagram of corners
1----0
|    |
2----3

^ y
|
.--->x
Coordinate system
Use FlipY to flip your box this way if you need to.
*/
enum AABB2D_CORNER_ENUM
{
  TOP_RIGHT_CORNER_2D = 0,
  TOP_LEFT_CORNER_2D,
  BOT_LEFT_CORNER_2D,
  BOT_RIGHT_CORNER_2D,
  AABB2D_CORNER_MAX
};

#if BOUNDINGVOL2D_USE_LESS_MEMORY
class AABB2D
#else
class AABB2D : public iBoundingVolume2D
#endif
{
public:
  Vector2 mMin;
  Vector2 mMax;

public:
  //Constructors
  inline AABB2D() { }
  inline AABB2D(const Vector2& min, const Vector2& max) : mMin(min), mMax(max) { }
  inline AABB2D(float minx, float miny, float maxx, float maxy) : mMin(Vector2(minx, miny)), mMax(Vector2(maxx, maxy)) { }
  AABB2D(const Vector2& position, float width, float height);
  
  //no need for copy constructor
  //no need for assignment op

  //mutators and accessors
  inline float GetWidth() const      { return mMax.x - mMin.x; }
  inline float GetHeight() const     { return mMax.y - mMin.y; }
  inline float GetHalfWidth() const  { return (mMax.x - mMin.x) * 0.5f; }
  inline float GetHalfHeight() const { return (mMax.y - mMin.y) * 0.5f; }

  // X-component = width, Y-component = height
  inline Vector2 GetSize() const     { return Vector2(GetWidth(), GetHeight()); }
  inline Vector2 GetHalfSize() const { return ((mMax - mMin) * 0.5f); }
  inline void SetExtents(const Vector2& min, const Vector2& max) {  mMin = min;  mMax = max; }
  inline void SetExtents(float minx, float miny, float maxx, float maxy) {  
    mMin.x = minx;  mMin.y = miny;  mMax.x = maxx;  mMax.y = maxy;
  }
  void SetHalfSize(const Vector2& halfsize);

  void SetCenterPosition(const Vector2& newcenterposition);
  inline Vector2 GetCenterPosition() const  { return Vector2(mMax - GetHalfSize()); }
  Vector2 GetCorner(AABB2D_CORNER_ENUM corner) const;

  //advanced
  void UpdateBoundingBody(const Vector2& newcenterposition);
  void UpdateBoundingBody(const Vector2& newcenterposition, const Vector2& newhalfsize);
  void UpdateBoundingBody(const Vector2& newcenterposition, float newwidth, float newheight);

  void Translate(const Vector2& displacement);
  void Scale(const Vector2& scale);
  void Merge(const AABB2D& aabb);  //extends the box to include another box
  void Merge(const Vector2& point); //extends the box to include another point

  //intersection
  bool Intersects(const Vector2& point) const;
  bool Intersects(const AABB2D& aabb) const;
  //bool Intersects(const Circle2D& circle);
  
  //helps with window's flipping of y-coordinates (flips the box's min/max y coords based on y-coord system)
  void FlipY(float window_maxy);

  //checks of the min and max make sense
  void CheckValidity();

  inline friend std::ostream& operator<< (std::ostream& o, const AABB2D& aabb)
	{
    o << "AABB2D(min=" << aabb.mMin << ", max=" << aabb.mMax << ")";
    return o;
  }
};

#endif
