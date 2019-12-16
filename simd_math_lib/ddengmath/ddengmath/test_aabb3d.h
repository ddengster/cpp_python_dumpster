
#ifndef _TEST_AABB3D_H
#define _TEST_AABB3D_H

#include "Math\Normal\MathHeaders.h"

void AABB3DConstructorTest()
{
  cout << "*****AABB3D Constructor Test*******\n";
  AABB3D box;
  cout << box << endl;

  AABB3D box2(5, 6, 7, 7, 8, 9);
  cout << box2 << endl;

  AABB3D box3(Vector3(3, 3, 3), 10, 10, 10);
  cout << box3 << endl;

  AABB3D box4(Vector3(3, 3, 3), Vector3(10, 10, 10));
  cout << box4 << endl;

  AABB3D box5(box2);
  cout << box5 << endl;

  cout << "******************************\n\n";
}

void AABB3DAccessorAndMutatorTest()
{
  cout << "*****AABB3D Accessor and mutator Test*******\n";
  AABB3D box(Vector3(1, 1, 1), Vector3(15, 12, 10));

  cout << "Width: " << box.GetWidth() << endl;
  cout << "Height: " << box.GetHeight() << endl;
  cout << "Depth: " << box.GetDepth() << endl;
  cout << "HalfWidth: " << box.GetHalfWidth() << endl;
  cout << "HalfHeight: " << box.GetHalfHeight() << endl;
  cout << "HalfDepth: " << box.GetHalfDepth() << endl;

  cout << "Size: " << box.GetSize() << endl;
  cout << "HalfSize: " << box.GetHalfSize() << endl;
  cout << "Center position: " << box.GetCenterPosition() << endl;

  cout << "Corners" << endl;
  cout << "Top right front: " << box.GetCorner(TOP_RIGHT_FRONT_CORNER_3D) << endl;
  cout << "Top left front: " << box.GetCorner(TOP_LEFT_FRONT_CORNER_3D) << endl;
  cout << "Bot left front: " << box.GetCorner(BOT_LEFT_FRONT_CORNER_3D) << endl;
  cout << "Bot right front: " << box.GetCorner(BOT_RIGHT_FRONT_CORNER_3D) << endl;
  cout << "Top right back: " << box.GetCorner(TOP_RIGHT_BACK_CORNER_3D) << endl;
  cout << "Top left back: " << box.GetCorner(TOP_LEFT_BACK_CORNER_3D) << endl;
  cout << "Bot left back: " << box.GetCorner(BOT_LEFT_BACK_CORNER_3D) << endl;
  cout << "Bot right back: " << box.GetCorner(BOT_RIGHT_BACK_CORNER_3D) << endl;

  box.SetExtents(Vector3(10, 10, 10), Vector3(20, 20, 20));
  cout << "SetExtents via 2 vectors: " << box << endl;
  box.SetExtents(11, 12, 13, 14, 15, 16);
  cout << "SetExtents via 6 float values: " << box << endl;

  cout << "******************************\n\n";
}

void AABB3DAdvancedFuncTest()
{
  cout << "*****AABB3D Advanced func Test*******\n";
  AABB3D box(Vector3(0, 0, 0), Vector3(10, 10, 10));

  cout << "Center position: " << box.GetCenterPosition() << endl;
  box.UpdateBoundingBody(Vector3(10, 10, 10));
  cout << "After setting position: \n" << box << endl;
  box.UpdateBoundingBody(Vector3(15, 15, 15), 4, 4, 4);
  cout << "After updating position and width/height : \n" << box << endl;

  box.Translate(Vector3(3, -3, 3));
  cout << "Translate: " << box << endl;
  box.Scale(Vector3(3, 2, 2));
  cout << "Scale: " << box << endl;

  box.Merge(Vector3(25, -25, 25));
  cout << "Merge: " << box << endl;
  box.Merge(AABB3D(-10, -10, -10, 33, 33, 33));
  cout << "Merge: " << box << endl;

  Vector3 pt(13, 13, 13);
  bool intersect = box.Intersects(pt);
  if (intersect)
    cout << box << " intersects " << pt << endl;
  else
    cout << box << " does not intersect " << pt << endl;

  AABB3D box2(45, 45, 45, 55, 55, 55);
  intersect = box.Intersects(box2);
  if (intersect)
    cout << box << " intersects " << box2 << endl;
  else
    cout << box << " does not intersect " << box2 << endl;

  AABB3D box3(10, 10, 10, 55, 55, 55);
  intersect = box.Intersects(box3);
  if (intersect)
    cout << box << " intersects " << box3 << endl;
  else
    cout << box << " does not intersect " << box3 << endl;

  Sphere sphere(Vector3(10, 0, 10), 9.9f);
  intersect = box3.Intersects(sphere);
  if (intersect)
    cout << sphere << " intersects " << box3 << endl;
  else
    cout << sphere << " does not intersect " << box3 << endl;

  Plane plane(Vector3(0, 1, 0), Vector3(0, 10.1f, 0));
  intersect = box3.Intersects(plane);
  if (intersect)
    cout << plane << " intersects " << box3 << endl;
  else
    cout << plane << " does not intersect " << box3 << endl;

  box.CheckValidity();
  cout << "******************************\n\n";
}

void AABB3DAllTests()
{
  AABB3DConstructorTest();
  AABB3DAccessorAndMutatorTest();
  AABB3DAdvancedFuncTest();

  AABB3D box;
  cout << "Size of AABB3D: " << sizeof(box) << endl;
}

#endif
