
#ifndef _INTERSECTION_FUNC_H
#define _INTERSECTION_FUNC_H

class Ray3D;
class Sphere;
class AABB3D;
class Plane;
class Frustum;

//collision with 2 different items so there doesnt need to be 2 implementations of the same type
//from 2 different objects

//most implementations are from orange book, real time collision detection, chapter 5

// only 3d objects collision atm
// yes/no collision
bool Ray_Sphere_Intersection(const Ray3D& ray, const Sphere& sphere);
bool Ray_AABB3D_Intersection(const Ray3D& ray, const AABB3D& aabb);
bool Ray_Plane_Intersection(const Ray3D& ray, const Plane& plane);
bool Sphere_AABB3D_Intersection(const Sphere& sphere, const AABB3D& aabb);
bool Plane_Sphere_Intersection(const Plane& plane, const Sphere& sphere);
bool Plane_AABB3D_Intersection(const Plane& plane, const AABB3D& aabb);
bool Frustum_Sphere_Intersection(const Frustum& plane, const Sphere& sphere);
bool Frustum_AABB3D_Intersection(const Frustum& frustum, const AABB3D& aabb);

// yes/no collision with interpolation value t saved
bool Ray_Sphere_Intersection(const Ray3D& ray, const Sphere& sphere, float& interpolant);
bool Ray_AABB3D_Intersection(const Ray3D& ray, const AABB3D& aabb, float& interpolant);
bool Ray_Plane_Intersection(const Ray3D& ray, const Plane& plane, float& interpolant);
#endif
