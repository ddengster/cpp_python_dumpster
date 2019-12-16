
#ifndef _RAY3D_H
#define _RAY3D_H

#include "Vector3.h"
#include "IntersectionFunctions.h"

class Matrix4;
class Sphere;
class AABB3D;

class Ray3D
{
public:
  Vector3 mOrigin;
  Vector3 mDirection;

public:
  inline Ray3D() { }
  inline Ray3D(const Vector3& origin, const Vector3& dir) : mOrigin(origin), mDirection(dir.NormalisedCopy()) { }
  inline Ray3D(const Matrix4& proj, const Matrix4& view, float viewportwidth, float viewportheight, float clickpointx, float clickpointy) 
              { GenerateWorldRayFromProjectionMatrix(proj, view, viewportwidth, viewportheight, clickpointx, clickpointy); }

  void NormalizeDirection() { mDirection.Normalise(); }

  Vector3 GetPointOnPath(float interpolant) const
  {
    return Vector3(mOrigin + interpolant * mDirection);
  }

  inline void Redefine(const Vector3& pt1, const Vector3& pt2) //direction is not normalized for this one
  {
    mOrigin = pt1;
    mDirection = pt2 - pt1;
  }
  //ray generation; assumes given clickpoints are in windows device coordinates (y is unflipped)
  void GenerateWorldRayFromProjectionMatrix(const Matrix4& proj, const Matrix4& view, float viewportwidth, float viewportheight, float clickpointx, float clickpointy);
  //void GenerateFromOGLProjectionMatrix(const Matrix4& proj, float clickpointx, float clickpointy);

  //camera should have aspect ratio, field of view, viewport width/height, near/far plane
  //pseudocode since cameras have different implementations
#if 0
  void GenerateFromCamera(Camera* cam, float clickpointx, float clickpointy);
#endif

  //intersection functions
  inline bool Intersects(const Plane& plane)                        { return Ray_Plane_Intersection(*this, plane); }
  inline bool Intersects(const Sphere& sphere)                      { return Ray_Sphere_Intersection(*this, sphere); }
  inline bool Intersects(const Sphere& sphere, float& interpolant)  { return Ray_Sphere_Intersection(*this, sphere, interpolant); }
  inline bool Intersects(const AABB3D& aabb)                        { return Ray_AABB3D_Intersection(*this, aabb); }
  inline bool Intersects(const AABB3D& aabb, float& interpolant)    { return Ray_AABB3D_Intersection(*this, aabb, interpolant); }
  inline bool Intersects(const Plane& plane, float& interpolant)    { return Ray_Plane_Intersection(*this, plane, interpolant); }
  
  inline friend std::ostream& operator<<(std::ostream& outStream, const Ray3D& ray)
  {
    outStream << "Ray: Origin - " << ray.mOrigin << " Direction: " << ray.mDirection;
    return outStream;
  }
};

#endif
