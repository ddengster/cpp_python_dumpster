
#include "IntersectionFunctions.h"
#include "GenericMath.h"
#include "Ray3D.h"
#include "Sphere.h"
#include "AABB3D.h"
#include "Plane.h"
#include "Frustum.h"

bool Ray_Sphere_Intersection(const Ray3D& ray, const Sphere& sphere)
{
  //trivial acceptence
  Vector3 rayorigin_to_spherecenter = sphere.mCenter - ray.mOrigin;
  float sqdist_rayorigin_to_spherecenter = rayorigin_to_spherecenter.SquaredLength();
  float sqradius = sphere.mRadius * sphere.mRadius;

  if (sqdist_rayorigin_to_spherecenter < sqradius) //ray origin within circle
    return true;
  //ray origin outside sphere
  
  //trivial rejection: ray dir facing the other way
  float projectedlength = rayorigin_to_spherecenter.DotProduct(ray.mDirection);
  if (projectedlength < 0.0f)
    return false;
    
  //compute perpendicular length
  float perpendicular_lengthsq = sqdist_rayorigin_to_spherecenter - (projectedlength * projectedlength);
  return (perpendicular_lengthsq <= sqradius);
}

bool Ray_AABB3D_Intersection(const Ray3D& ray, const AABB3D& aabb)
{
  float dummy;
  return Ray_AABB3D_Intersection(ray, aabb, dummy);
}

bool Ray_Plane_Intersection(const Ray3D& ray, const Plane& plane)
{
  float dist = plane.GetDistanceToPlane(ray.mOrigin);
  if (dist == 0.0f)
    return true; // on plane

  float dotdir = ray.mDirection.DotProduct(plane.mNormal);
  
  if ((dist > 0.0f && dotdir > 0.0f) || (dist < 0.0f && dotdir < 0.0f))
    return false; //ray moving away from plane
  else
    return true;
}

bool Sphere_AABB3D_Intersection(const Sphere& sphere, const AABB3D& aabb)
{
  float sqdist = aabb.GetSquaredDistanceToPoint(sphere.mCenter);
  
  if (sqdist <= (sphere.mRadius * sphere.mRadius))
    return true;
  return false;
}

bool Plane_Sphere_Intersection(const Plane& plane, const Sphere& sphere)
{
  return (plane.GetSide(sphere) == PLANE_SIDE_ON);
}

bool Plane_AABB3D_Intersection(const Plane& plane, const AABB3D& aabb)
{
  return (plane.GetSide(aabb) == PLANE_SIDE_ON);
}

bool Frustum_Sphere_Intersection(const Frustum& frustum, const Sphere& sphere)
{
  static const int planecount = 6;
  PLANE_SIDE side;
  for (int i=0; i<planecount; ++i)
  {
    side = frustum[i].GetSide(sphere);
    //touching any side of the plane means intersection
    if (side == PLANE_SIDE_ON)
      return true;
    if (side == PLANE_SIDE_POSITIVE)
      return false;
  }
  return true;
}

bool Frustum_AABB3D_Intersection(const Frustum& frustum, const AABB3D& aabb)
{
   static const int planecount = 6;
  PLANE_SIDE side;
  for (int i=0; i<planecount; ++i)
  {
    side = frustum[i].GetSide(aabb);
    //touching any side of the plane means intersection
    if (side == PLANE_SIDE_ON)
      return true;
    if (side == PLANE_SIDE_POSITIVE)
      return false;
  }
  return true;
}

/** yes/no collision with interpolation value t saved **/
bool Ray_Sphere_Intersection(const Ray3D& ray, const Sphere& sphere, float& interpolant)
{
  //trivial acceptence
  Vector3 rayorigin_to_spherecenter = sphere.mCenter - ray.mOrigin;
  float sqdist_rayorigin_to_spherecenter = rayorigin_to_spherecenter.SquaredLength();
  float sqradius = sphere.mRadius * sphere.mRadius;

  if (sqdist_rayorigin_to_spherecenter < sqradius) //ray origin within circle
  {
    interpolant = 0.0f;
    return true;
  }

  //ray origin outside sphere
  
  //trivial rejection: ray dir facing the other way
  float projectedlength = rayorigin_to_spherecenter.DotProduct(ray.mDirection);
  if (projectedlength < 0.0f)
    return false;
    
  //compute perpendicular length
  float perpendicular_lengthsq = sqdist_rayorigin_to_spherecenter - (projectedlength * projectedlength);
  if (perpendicular_lengthsq > sqradius) //closest length to ray is more than sphere radius
    return false;

  //intersection, so compute interpolant
  float lengthtosphere = projectedlength - GenericMath::Sqrt(sqradius - perpendicular_lengthsq);
  interpolant = lengthtosphere;
  return true;
}

bool Ray_AABB3D_Intersection(const Ray3D& ray, const AABB3D& aabb, float& interpolant)
{
  //from real time collision detection book; Chapter 5
  float tmin = -FLT_MAX;
  float tmax = FLT_MAX;

  static const int axis = 3;
  for (int i=0; i<axis; ++i)
  {
    if (ray.mDirection[i] == 0.0f) //can replace with epsilon
    {
      //ray not moving in this axis' direction; so check its position to see if it is outside box
      if ((ray.mOrigin[i] < aabb.mMin[i]) || (ray.mOrigin[i] > aabb.mMax[i]))
        return false;
    }
    else
    {
      //compute interpolants
      float inversedir = 1.0f / ray.mDirection[i];
      float t1 = (aabb.mMin[i] - ray.mOrigin[i]) * inversedir;
      float t2 = (aabb.mMax[i] - ray.mOrigin[i]) * inversedir;

      //make sure t1 is intersection with near plane and t2 with far plane
      if (t1 > t2)
      {
        float temp = t1;
        t1 = t2;
        t2 = temp;
      }

      //assign min/max interpolants
      if (t1 > tmin)
        tmin = t1;
      if (t2 > tmax)
        tmax = t2;
      //exit if tmin is greater than tmax
      if (tmin > tmax)
        return false;
    }
  }

  interpolant = tmin;
  //assumes infinite ray; so we adjust it such that we return false for -ve interpolants, true for +ve interpolants
  return (interpolant >= 0.0f) ? true : false;
}

bool Ray_Plane_Intersection(const Ray3D& ray, const Plane& plane, float& interpolant)
{
  float dist = plane.GetDistanceToPlane(ray.mOrigin);
  if (dist == 0.0f)
  {
    interpolant = 0.0f;
    return true; // on plane
  }

  float dotdir = ray.mDirection.DotProduct(plane.mNormal);
  if ((dist > 0.0f && dotdir > 0.0f) || (dist < 0.0f && dotdir < 0.0f))
    return false; //ray moving away from plane

  interpolant = (-plane.d - plane.mNormal.DotProduct(ray.mOrigin)) / dotdir;
  return true;
}
