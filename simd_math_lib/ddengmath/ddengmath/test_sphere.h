
#ifndef _SPHERETEST_H
#define _SPHERETEST_H

#include "Math\Normal\MathHeaders.h"

void SphereConstructorTest()
{
  cout << "*****Sphere Test*******\n";
  Sphere sphere;
  cout << sphere << endl;

  Sphere sphere2(Vector3(1, 1, 1), 10);
  cout << sphere2 << endl;

  sphere2.Translate(Vector3(10, 0, 0));
  cout << sphere2 << endl;

  sphere2.Scale(30);
  cout << sphere2 << endl;

  cout << "******************************\n\n";
}

void SphereAllTests()
{
  SphereConstructorTest();

  Sphere sphere;
  cout << "Size of Sphere: " << sizeof(sphere) << endl;
}


#endif
