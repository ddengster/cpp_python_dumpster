
#ifndef _Sphere_H
#define _Sphere_H

#include "iBoundingVolume3D.h"
#include "IntersectionFunctions.h"

#if BOUNDINGVOL3D_USE_LESS_MEMORY
class Sphere
#else
class Sphere : public iBoundingVolume3D
#endif
{
public:
  Vector3 mCenter;
  float mRadius;

public:
  //Constructor
  inline Sphere() { }
  inline Sphere(const Vector3& position, float radius) : mCenter(position), mRadius(radius) { }

  //no need for copy constructor
  //no need for assignment op

  //advanced
  inline void Translate(const Vector3& disp) { mCenter += disp; }
  inline void Scale(float scale) { mRadius *= scale; }

  void UpdateBoundingBody(const Vector3& newcenterposition) { mCenter = newcenterposition; }

  //intersection functions
  inline bool Intersects(const Vector3& pt) const { return (mCenter.SquaredDistance(pt) <= (mRadius * mRadius)); }
  inline bool Intersects(const Sphere& sphere) const { 
    float totalradius = mRadius + sphere.mRadius;
    return (mCenter.SquaredDistance(sphere.mCenter) <= (totalradius * totalradius)); 
  }
  inline bool Intersects(const Ray3D& ray) const                           { return Ray_Sphere_Intersection(ray, *this); }
  inline bool Intersects(const AABB3D& aabb) const                         { return Sphere_AABB3D_Intersection(*this, aabb); }
  inline bool Intersects(const Plane& plane) const                         { return Plane_Sphere_Intersection(plane, *this); }
  inline bool Intersects(const Frustum& frustum) const                     { return Frustum_Sphere_Intersection(frustum, *this); }
  inline bool Intersects(const Ray3D& ray, float& interpolant) const       { return Ray_Sphere_Intersection(ray, *this, interpolant); }

  inline friend std::ostream& operator<<(std::ostream& outStream, const Sphere& sphere)
  {
    outStream << "Sphere: Center - " << sphere.mCenter << " Radius: " << sphere.mRadius << "\n";
    return outStream;
  }
};

#endif
