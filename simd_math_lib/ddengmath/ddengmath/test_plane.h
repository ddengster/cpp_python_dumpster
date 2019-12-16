
#ifndef _PLANE_TEST_H
#define _PLANE_TEST_H

#include "Math\Normal\MathHeaders.h"

void PlaneConstructorTest()
{
  cout << "*****Plane Constructor Test*******\n";
  Plane plane;
  cout << plane << endl;

  Plane plane2(Vector4(1));
  cout << plane2 << endl;

  Plane plane3(Vector3(0, 1, 0), -2);
  cout << plane3 << endl;

  Plane plane4(Vector3(0, 1, 0), Vector3(0, 1, 0));
  cout << plane4 << endl;

  Plane plane5(Vector3(0, 0, 0), Vector3(1, 0, 0), Vector3(0, 1, 0)); //x-y plane
  cout << plane5 << endl;
  
  cout << "******************************\n\n";
}

void PlaneSideDeterminationTest()
{
  cout << "*****Plane Side Determination Test*******\n";

  Plane plane(Vector3(0, 1, 0), Vector3(0, 1, 0));
  cout << plane << endl;

  cout << "1 for negative, 0 for positive, 2 for on.\n";
  cout << "\nPoint: \n";
  Vector3 pt(0.0f);
  cout << "Side: " << plane.GetSide(pt) << endl;
  Vector3 pt2(0, 4, 0);
  cout << "Side: " << plane.GetSide(pt2) << endl;
  cout << "Distance: " << plane.GetDistanceToPlane(pt) << endl;

  cout << "\nSphere: \n";
  Sphere sphere(Vector3(0, 0, 0), 0.1f);
  cout << "Side: " << plane.GetSide(sphere) << endl;
  sphere.mRadius = 2.0f;
  cout << "Side: " << plane.GetSide(sphere) << endl;
  sphere.mCenter = Vector3(0, 4, 0);
  cout << "Side: " << plane.GetSide(sphere) << endl;

  cout << "\nAABB3D: \n";
  AABB3D aabb(0, 0, 0, 2, 2, 2);
  cout << "Side: " << plane.GetSide(aabb) << endl;
  aabb.SetCenterPosition(Vector3(0, 4, 0));
  cout << "Side: " << plane.GetSide(aabb) << endl;
  aabb.SetCenterPosition(Vector3(0, -4, 0));
  cout << "Side: " << plane.GetSide(aabb) << endl;

  cout << "******************************\n\n";
}

void PlaneAdvancedTest()
{
  cout << "*****Plane Advanced Test*******\n";
  Plane plane(Vector3(0, 1, 0), Vector3(0, -1, 0));
  cout << plane << endl;

  cout << "Projected vector: " << plane.ProjectVector(Vector3(3, 10, 0)) << endl;
  cout << "Reflected vector: " << plane.ReflectVector(Vector3(3, 10, 0)) << endl;

  Matrix4 mat;
  mat.MakeTranslateMatrix(Vector3(0, 4, 0));
  plane.Transform(mat);
  cout << "Transformed plane: " << plane << endl;
  
  cout << "******************************\n\n";
}

void PlaneAllTests()
{
  PlaneConstructorTest();
  PlaneSideDeterminationTest();
  PlaneAdvancedTest();

  Plane plane;
  cout << "Size of Plane: " << sizeof(plane) << endl;
}


#endif
