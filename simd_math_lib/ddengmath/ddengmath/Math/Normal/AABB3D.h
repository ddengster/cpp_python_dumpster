
#ifndef _AABB3D_H
#define _AABB3D_H

#include "iBoundingVolume3D.h"
#include "IntersectionFunctions.h"
/*
Diagram of corners
  B----B
 /    /
1----0 |
|    | |
|    | B
|    | /
2----3

^y  z
|  /
| /
.--->x
Coordinate system
Use FlipZ to flip your box this way if you need to. Or, recode the enums
*/
enum AABB3D_CORNER_ENUM
{
  TOP_RIGHT_FRONT_CORNER_3D = 0,
  TOP_LEFT_FRONT_CORNER_3D,
  BOT_LEFT_FRONT_CORNER_3D,
  BOT_RIGHT_FRONT_CORNER_3D,
  TOP_RIGHT_BACK_CORNER_3D,
  TOP_LEFT_BACK_CORNER_3D,
  BOT_LEFT_BACK_CORNER_3D,
  BOT_RIGHT_BACK_CORNER_3D,
  AABB3D_CORNER_MAX
};

#if BOUNDINGVOL3D_USE_LESS_MEMORY
class AABB3D
#else
class AABB3D : public iBoundingVolume3D
#endif
{
public:
  Vector3 mMin;
  Vector3 mMax;

public:
  //Constructors
  inline AABB3D() { }
  inline AABB3D(const Vector3& min, const Vector3& max) : mMin(min), mMax(max) { }
  inline AABB3D(float minx, float miny, float minz, float maxx, float maxy, float maxz) : mMin(Vector3(minx, miny, minz)), mMax(Vector3(maxx, maxy, maxz)) { }
  AABB3D(const Vector3& position, float width, float height, float depth);
  
  //no need for copy constructor
  //no need for assignment op

  //mutators and accessors
  inline float GetWidth() const      { return mMax.x - mMin.x; }
  inline float GetHeight() const     { return mMax.y - mMin.y; }
  inline float GetDepth() const      { return mMax.z - mMin.z; }
  inline float GetHalfWidth() const  { return (mMax.x - mMin.x) * 0.5f; }
  inline float GetHalfHeight() const { return (mMax.y - mMin.y) * 0.5f; }
  inline float GetHalfDepth() const  { return (mMax.z - mMin.z) * 0.5f; }

  // X-component = width, Y-component = height, Z-component = depth
  inline Vector3 GetSize() const     { return Vector3(GetWidth(), GetHeight(), GetDepth()); }
  inline Vector3 GetHalfSize() const { return ((mMax - mMin) * 0.5f); }
  inline void SetExtents(const Vector3& min, const Vector3& max) {  mMin = min;  mMax = max; }
  inline void SetExtents(float minx, float miny, float maxx, float maxy, float minz, float maxz) {  
    mMin.x = minx;  mMin.y = miny;  mMax.x = maxx;  mMax.y = maxy;  mMin.z = minz;  mMax.z = maxz;
  }
  void SetHalfSize(const Vector3& halfsize);

  void SetCenterPosition(const Vector3& newcenterposition);
  inline Vector3 GetCenterPosition() const  { return Vector3(mMax - GetHalfSize()); }
  Vector3 GetCorner(AABB3D_CORNER_ENUM corner) const; //really need this?

  //advanced
  //inherited functions
  void UpdateBoundingBody(const Vector3& newcenterposition);
  void UpdateBoundingBody(const Vector3& newcenterposition, const Vector3& newhalfsize);
  void UpdateBoundingBody(const Vector3& newcenterposition, float newwidth, float newheight, float newdepth);

  void Translate(const Vector3& displacement);
  void Scale(const Vector3& scale);
  void Merge(const AABB3D& aabb);  //extends the box to include another box
  void Merge(const Vector3& point); //extends the box to include another point

  //intersection
  bool Intersects(const Vector3& point) const;
  bool Intersects(const AABB3D& aabb) const;
  inline bool Intersects(const Sphere& sphere) const                 { return Sphere_AABB3D_Intersection(sphere, *this); }
  inline bool Intersects(const Ray3D& ray) const                     { return Ray_AABB3D_Intersection(ray, *this); }
  inline bool Intersects(const Ray3D& ray, float& interpolant) const { return Ray_AABB3D_Intersection(ray, *this, interpolant); }
  inline bool Intersects(const Plane& plane) const                   { return Plane_AABB3D_Intersection(plane, *this); }
  inline bool Intersects(const Frustum& frustum) const               { return Frustum_AABB3D_Intersection(frustum, *this); }
  
  float GetSquaredDistanceToPoint(const Vector3& pt) const;
  //for those who needs their z-coordinates pointing the other way
  void FlipZ();
  //checks of the min and max make sense
  void CheckValidity();

  inline friend std::ostream& operator<< (std::ostream& o, const AABB3D& aabb)
	{
    o << "AABB3D(min=" << aabb.mMin << ", max=" << aabb.mMax << ")";
    return o;
  }
};

#endif
