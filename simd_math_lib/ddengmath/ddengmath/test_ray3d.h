
#ifndef _RAY3DTEST_H
#define _RAY3DTEST_H

#include "Math\Normal\MathHeaders.h"


void Ray3DConstructorTest()
{
  cout << "*****Ray3D Test*******\n";
  Ray3D ray;
  cout << ray << endl;

  Ray3D ray2(Vector3(0, 1, 0), Vector3(2, 0, 0)); //should auto normalize
  cout << ray2 << endl;

  ray2.Redefine(Vector3(0, 0, 0), Vector3(2, 0, 0));
  cout << ray2 << endl;

  ray2.NormalizeDirection();
  cout << ray2 << endl;

  //interpolant test
  Ray3D ray3(Vector3(0, 1, 0), Vector3(2, 0, 0));
  cout << ray3 << endl;
  cout << "Point 1.5 times on path: " << ray3.GetPointOnPath(1.5f) << endl;

  cout << "******************************\n\n";
}

void Ray3DIntersectTest()
{
  cout << "*****Ray3D Intersect Test*******\n";

  float interpolant;
  Sphere sphere(Vector3(0, 0, 1), 1);
  Ray3D ray(Vector3(0, 0, -1), Vector3(0, 0, 1));

  cout << ray.Intersects(sphere, interpolant) << endl;
  cout << "Interpolant: " << interpolant << endl;

  ray.mDirection = Vector3(0, 0, -1);
  cout << ray.Intersects(sphere, interpolant) << endl;

  AABB3D aabb(0, 0, 0, 1, 1, 1);
  cout << ray.Intersects(aabb, interpolant) << endl;
  ray.mOrigin = Vector3(0, 0, -2.5f);
  ray.mDirection = Vector3(0, 0, 1);
  cout << ray.Intersects(aabb, interpolant) << endl;
  cout << "Interpolant: " << interpolant << endl;

  Plane plane(Vector3(0, 1, 0), Vector3(0, -1, 0));
  ray.mOrigin = Vector3(0, 0, 0);
  ray.mDirection = Vector3(1, 1, 0);
  cout << ray.Intersects(plane, interpolant) << endl;

  ray.mDirection = Vector3(1, -1, 0);
  cout << ray.Intersects(plane, interpolant) << endl;
  cout << "Interpolant: " << interpolant << endl;

  cout << "******************************\n\n";
}

void Ray3DAllTests()
{
  Ray3DConstructorTest();
  Ray3DIntersectTest();

  Ray3D ray;
  cout << "Size of Ray3D: " << sizeof(ray) << endl;
}

#endif
