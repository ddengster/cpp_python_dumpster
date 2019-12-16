
#ifndef _TEST_FRUSTUM_H
#define _TEST_FRUSTUM_H

#include "Math\Normal\MathHeaders.h"

void FrustumConstructorTest()
{
  cout << "*****Frustum Test*******\n";
  Frustum f;
  cout << f << endl;

 
  cout << "******************************\n\n";
}

void FrustumAllTests()
{
  FrustumConstructorTest();

  Frustum f;
  cout << "Size of Frustum: " << sizeof(f) << endl;
}


#endif
