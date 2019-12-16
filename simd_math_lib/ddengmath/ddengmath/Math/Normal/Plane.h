
#ifndef _PLANE_H
#define _PLANE_H

#include "Vector3.h"
#include "Vector4.h"
#include "IntersectionFunctions.h"

class Matrix4;

enum PLANE_SIDE
{
  PLANE_SIDE_POSITIVE = 0, //pt on same side of plane normal
  PLANE_SIDE_NEGATIVE,
  PLANE_SIDE_ON,
  PLANE_SIDE_NONE
};

/** Semi-infinite plane **/
class Plane
{
public:
  Vector3 mNormal;
  float d;

public:
  inline Plane() { }
  inline Plane(const Vector3& normaltoplane, float _d) 
    : mNormal(normaltoplane.NormalisedCopy()), d(-_d) { }
  inline Plane(const Vector3& normaltoplane, const Vector3& pointonplane) //plane from pt on plane and a normal
    : mNormal(normaltoplane.NormalisedCopy()), d(-normaltoplane.DotProduct(pointonplane)) { }
  inline Plane(const Vector3& pt1, const Vector3& pt2, const Vector3& pt3) //plane from 3 pts
    { Redefine(pt1, pt2, pt3); }
  inline Plane(const Vector4& vec4) 
    : mNormal(vec4.x, vec4.y, vec4.z), d(vec4.w) { }

  //side determination
  PLANE_SIDE GetSide(const Vector3& pt) const;
  PLANE_SIDE GetSide(const Sphere& sphere) const;
  PLANE_SIDE GetSide(const AABB3D& aabb) const;
  float GetDistanceToPlane(const Vector3& pt) const; //note: no fabs

  //projection/reflection of vectors onto plane
  Vector3 ProjectVector(const Vector3& vec) const;
  Vector3 ReflectVector(const Vector3& vec) const;

  void Normalise(); //will also normalise d

  //mutators
  void Redefine(const Vector3& pt1, const Vector3& pt2, const Vector3& pt3);
  void Redefine(const Vector3& normal, const Vector3& ptonplane);
  void Transform(const Matrix4& mat);

  //intersection
  inline bool Intersects(const Ray3D& ray) const                         { return Ray_Plane_Intersection(ray, *this); }
  inline bool Intersects(const Sphere& sphere) const                     { return Plane_Sphere_Intersection(*this, sphere); }
  inline bool Intersects(const AABB3D& aabb) const                       { return Plane_AABB3D_Intersection(*this, aabb); }
  inline bool Intersects(const Ray3D& ray, float& interpolant) const     { return Ray_Plane_Intersection(ray, *this, interpolant); }

  inline friend std::ostream& operator<<(std::ostream& outStream, const Plane& plane)
  {
    outStream << "Plane: Normal-" << plane.mNormal << " Distance: " << plane.d << "\n";
    return outStream;
  }
};

#endif
