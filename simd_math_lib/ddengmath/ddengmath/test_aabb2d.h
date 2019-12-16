
#ifndef _TEST_AABB2D_H
#define _TEST_AABB2D_H

#include "Math\Normal\MathHeaders.h"

void AABB2DConstructorTest()
{
  cout << "*****AABB2D Constructor Test*******\n";
  AABB2D box;
  cout << box << endl;

  AABB2D box2(5, 6, 7, 8);
  cout << box2 << endl;

  AABB2D box3(Vector2(3, 3), 10, 10);
  cout << box3 << endl;

  AABB2D box4(Vector2(3, 3), Vector2(10, 10));
  cout << box4 << endl;

  AABB2D box5(box2);
  cout << box5 << endl;

  cout << "******************************\n\n";
}

void AABB2DAccessorAndMutatorTest()
{
  cout << "*****AABB2D Accessor and mutator Test*******\n";
  AABB2D box(Vector2(1, 1), Vector2(15, 12));

  cout << "Width: " << box.GetWidth() << endl;
  cout << "Height: " << box.GetHeight() << endl;
  cout << "HalfWidth: " << box.GetHalfWidth() << endl;
  cout << "HalfHeight: " << box.GetHalfHeight() << endl;

  cout << "Size: " << box.GetSize() << endl;
  cout << "HalfSize: " << box.GetHalfSize() << endl;
  cout << "Center position: " << box.GetCenterPosition() << endl;

  cout << "Corners" << endl;
  cout << "Top right: " << box.GetCorner(TOP_RIGHT_CORNER_2D) << endl;
  cout << "Top left: " << box.GetCorner(TOP_LEFT_CORNER_2D) << endl;
  cout << "Bot left: " << box.GetCorner(BOT_LEFT_CORNER_2D) << endl;
  cout << "Bot right: " << box.GetCorner(BOT_RIGHT_CORNER_2D) << endl;

  box.SetExtents(Vector2(10, 10), Vector2(20, 20));
  cout << "SetExtents via 2 vectors: " << box << endl;
  box.SetExtents(11, 12, 13, 14);
  cout << "SetExtents via 4 float values: " << box << endl;

  cout << "******************************\n\n";
}

void AABB2DAdvancedFuncTest()
{
  cout << "*****AABB2D Advanced func Test*******\n";
  AABB2D box(Vector2(0, 0), Vector2(10, 10));

  box.UpdateBoundingBody(Vector2(10, 10));
  cout << "After updating position: \n" << box << endl;
  box.UpdateBoundingBody(Vector2(15, 15), 4, 4);
  cout << "After updating position and width/height : \n" << box << endl;

  box.Translate(Vector2(3, -3));
  cout << "Translate: " << box << endl;
  box.Scale(Vector2(3, 2));
  cout << "Scale: " << box << endl;

  box.Merge(Vector2(25, -25));
  cout << "Merge: " << box << endl;
  box.Merge(AABB2D(-10, -10, 33, 33));
  cout << "Merge: " << box << endl;

  Vector2 pt(13, 13);
  bool intersect = box.Intersects(pt);
  if (intersect)
    cout << box << " intersects " << pt << endl;
  else
    cout << box << " does not intersect " << pt << endl;

  AABB2D box2(45, 45, 55, 55);
  intersect = box.Intersects(box2);
  if (intersect)
    cout << box << " intersects " << box2 << endl;
  else
    cout << box << " does not intersect " << box2 << endl;

  AABB2D box3(10, 10, 55, 55);
  intersect = box.Intersects(box3);
  if (intersect)
    cout << box << " intersects " << box3 << endl;
  else
    cout << box << " does not intersect " << box3 << endl;

  box.CheckValidity();
  cout << "******************************\n\n";
}

void AABB2DAllTests()
{
  AABB2DConstructorTest();
  AABB2DAccessorAndMutatorTest();
  AABB2DAdvancedFuncTest();

  AABB2D box;
  cout << "Size of aabb2d: " << sizeof(box) << endl;
}

#endif
