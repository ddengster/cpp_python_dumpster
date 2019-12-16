
#ifndef _FRUSTUM_H
#define _FRUSTUM_H

#include "Plane.h"
#include "IntersectionFunctions.h"

class Frustum
{
public:
  //normals of planes all point outside frustum
  Plane mLeft;
  Plane mRight;
  Plane mTop;
  Plane mBottom;
  Plane mNear;
  Plane mFar;

public:
  inline Frustum() { }
  Frustum(const Matrix4& proj); //construct from projection matrix
  Frustum(const Plane planes[6]);

#if 0 
  //camera implementations; must have aspect ratio, near plane, far plane, field of view, etc
  Frustum(Camera* cam);
#endif

  inline void Transform(const Matrix4& mat) 
  { 
    mTop.Transform(mat);
    mBottom.Transform(mat);
    mLeft.Transform(mat);
    mRight.Transform(mat);
    mNear.Transform(mat);
    mFar.Transform(mat);
  }

  //accessors/mutators
  inline const Plane& operator[](unsigned int i) const
  {
    assert(i < 6); 
    return *(&mLeft + i);
  }

  inline Plane& operator[](unsigned int i) 
  {
    assert(i < 6); 
    return *(&mLeft + i);
  }

  //intersection/contains
  inline bool Intersects(const AABB3D& aabb) const { return Frustum_AABB3D_Intersection(*this, aabb); }
  inline bool Intersects(const Sphere& sphere) const { return Frustum_Sphere_Intersection(*this, sphere); }

  inline friend std::ostream& operator<<(std::ostream& outStream, const Frustum& f)
  {
    outStream << "Frustum:\n"
              << "Left Plane:" << f.mLeft
              << "Right Plane:" << f.mRight
              << "Bottom Plane:" << f.mBottom
              << "Top Plane:" << f.mTop
              << "Near Plane:" << f.mNear
              << "Far Plane:" << f.mFar;

    return outStream;
  }
};

#endif
