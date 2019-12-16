
#include "Plane.h"
#include "Matrix4.h"
#include "Sphere.h"
#include "AABB3D.h"

PLANE_SIDE Plane::GetSide(const Vector3& pt) const
{
  float dist = mNormal.DotProduct(pt) + d;
  if (dist < 0.0f)
    return PLANE_SIDE_NEGATIVE;
  else if (dist > 0.0f)
    return PLANE_SIDE_POSITIVE;
  return PLANE_SIDE_ON;
}

PLANE_SIDE Plane::GetSide(const Sphere& sphere) const
{
  float dist = mNormal.DotProduct(sphere.mCenter) + d;
  if (GenericMath::FAbs(dist) < sphere.mRadius)
    return PLANE_SIDE_ON;
  else if (dist < -sphere.mRadius)
    return PLANE_SIDE_NEGATIVE;
  else //if (dist > sphere.mRadius)
    return PLANE_SIDE_POSITIVE;
}

PLANE_SIDE Plane::GetSide(const AABB3D& aabb) const
{
  Vector3 aabbcenter(aabb.GetCenterPosition());
  Vector3 aabbhalfsize(aabb.GetHalfSize()); //should be +ve

  //project halfsize vector onto the plane
  float r = aabbhalfsize.x * GenericMath::FAbs(mNormal.x) + 
            aabbhalfsize.y * GenericMath::FAbs(mNormal.y) + 
            aabbhalfsize.z * GenericMath::FAbs(mNormal.z);

  //get shortest distance of box center to plane
  float s = mNormal.DotProduct(aabbcenter) + d;

  if (GenericMath::FAbs(s) <= r)
    return PLANE_SIDE_ON;
  else if (s < -r)
    return PLANE_SIDE_NEGATIVE;
  else //if (s > r)
    return PLANE_SIDE_POSITIVE;
}

float Plane::GetDistanceToPlane(const Vector3& pt) const
{
  return (mNormal.DotProduct(pt) + d);
}

Vector3 Plane::ProjectVector(const Vector3& vec) const
{
  return (vec - (GetDistanceToPlane(vec) * mNormal));
}

Vector3 Plane::ReflectVector(const Vector3& vec) const
{
  return (vec - (2.0f * GetDistanceToPlane(vec) * mNormal));
}

void Plane::Normalise()
{
  float length = mNormal.Length();
  d /= length;
  mNormal /= length;
}

void Plane::Redefine(const Vector3& pt1, const Vector3& pt2, const Vector3& pt3)
{
  Vector3 edge1 = pt2 - pt1;
  Vector3 edge2 = pt3 - pt1;
  mNormal = edge1.CrossProduct(edge2);
  mNormal.Normalise();
  d = -mNormal.DotProduct(pt1);
}

void Plane::Redefine(const Vector3& normal, const Vector3& ptonplane)
{
  mNormal = normal.NormalisedCopy();
  d = -mNormal.DotProduct(ptonplane);
}

void Plane::Transform(const Matrix4& mat)
{
  Matrix4 mat2 = mat.TransposeCopy();
  mat2.Inverse();
  Vector4 plane_eqn(mNormal.x, mNormal.y, mNormal.z, d);
  plane_eqn = mat2 * plane_eqn;
  mNormal = Vector3(plane_eqn.x, plane_eqn.y, plane_eqn.z);
  d = plane_eqn.w;
  Normalise();
}
