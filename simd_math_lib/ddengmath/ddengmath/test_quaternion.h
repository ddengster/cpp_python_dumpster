
#ifndef _TEST_QUATERNION_H
#define _TEST_QUATERNION_H


#include "Math\Normal\MathHeaders.h"
#include <iostream>
using namespace std;

void QuaternionConstructorTest()
{
  cout << "*****Quaternion Constructor Test*******\n";
  Quaternion q1;
  cout << q1 << endl;

  Quaternion q2(1.0f, 2.0f, 3.0f, 4.0f);
  cout << q2 << endl;

  float arr[4] = { 11, 12, 13, 14 };
  Quaternion q3(arr);
  cout << q3 << endl;

  Quaternion q4(Degree(90), Vector3(0, 0, 1));
  cout << q4 << endl;

  Quaternion q5(q2);
  cout << q5 << endl;
  cout << "******************************\n\n";
}

void QuaternionAccessAndAssignmentTest()
{
  cout << "*****Quaternion Access And Assignment Test*******\n";
  Quaternion q(1.0f, 2.0f, 3.0f, 4.0f);

  cout << "[] test: " << q[0] << " " << q[1] << " " << q[2] << " " << q[3] << endl;
  //q[4]; //assert

  Quaternion q2(1.5f, 2.5f, 3.5f, 4.5f);
  q = q2;
  cout << q << endl;

  cout << "******************************\n\n";
}

void QuaternionMutatorAndAccessorTest()
{
  cout << "*****Quaternion Mutator And Accessor Test*******\n";
  Quaternion q(Degree(90), Vector3(0, 0, 1));
  Matrix3 mat;
  mat.MakeTransform(Vector2(0,0), Vector2(1, 1), Degree(90));

  Matrix3 mat2 = q.GetRotationMatrix3();
  cout << "Conversion to matrix3: \n";
  cout << mat2 << endl;
  cout << "Rotated matrix: \n";
  cout << mat << endl;

  Degree deg; Vector3 vec;
  q.GetRotationToAxis(deg, vec); 
  cout << "Rotation to axis: " << deg.GetValue() << " degrees, about axis: " << vec << endl;

  Quaternion q2(Degree(45), Vector3(0, 1, 0));
  
  q2.GetRotationToAxis(deg, vec);
  cout << "Rotation to axis: " << deg.GetValue() << " degrees, about axis: " << vec << endl;

  q.SetRotationToAxis(deg, vec);
  q.GetRotationToAxis(deg, vec);
  cout << "Rotation to axis: " << deg.GetValue() << " degrees, about axis: " << vec << endl;

  cout << "******************************\n\n";
}

void QuaternionComparisonTest()
{
  cout << "*****Quaternion Comparison Test*******\n";
  Quaternion q(1.0f, 2.0f, 3.0f, 4.0f);

  Quaternion q2(q);
  if (q == q2)
    cout << "Quaternions are equal" << endl;
  else
    cout << "Quaternions are not equal" << endl;

  q.w = 10.0f;
  if (q2 != q)
    cout << "Quaternions are not equal" << endl;
  else
    cout << "Quaternions are equal" << endl;

  cout << "******************************\n\n";
}

void QuaternionBasicArithmeticTest()
{
  cout << "*****Quaternion Basic Arithmetic Test*******\n";
  Quaternion q(1.0f, 2.0f, 3.0f, 4.0f);
  Quaternion q2(11.0f, 12.0f, 13.0f, 14.0f);
  Quaternion q3(Degree(90), Vector3(0, 0, 1));
  Quaternion q4(Degree(-90), Vector3(0, 0, 1));
  Vector3 vec(1.0f, 0, 0);
  float scalar = 0.5f;

  cout << "Plus: \n" << q + q2 << endl;
  cout << "Minus: \n" << q - q2 << endl;
  cout << "Multiply(Quaternion concatenation): \n" << q3 * q4 << endl;
  cout << "Matrix3 After quaternion concat:\n" << (q3 * q4).GetRotationMatrix3() << endl;
  cout << "Multiply(Rotating vector by quaternion): \n" << q3 * vec << endl;
  cout << "Negation: \n" << -q << endl;
  cout << "Multiply(scalar): \n" << q * scalar << endl;

  cout << "Multiply(scalar*q): \n" << scalar * q << endl;
  
  q += q2;
  cout << "+= :\n" << q << endl;
  q -= q2;
  cout << "-= :\n" << q << endl;
  q3 *= q4;
  cout << "*= :\n" << q3 << endl;

  cout << "******************************\n\n";
}

void QuaternionAdvancedOPTest()
{
  cout << "*****Quaternion Advanced OP Test*******\n";
  
  Quaternion q3(Degree(45), Vector3(0, 0, 1));
  Quaternion q4(Degree(-90), Vector3(0, 0, 1));
  Quaternion q5(Degree(30), Vector3(1, 0, 0));

  float degval;
  q3 *= q4;

  q3.GetRoll(degval);
  cout << "Roll(rotation about x axis(pointing forwards)  in degrees): " << degval << endl;
  q3.GetPitch(degval);
  cout << "Pitch(rotation about y axis(pointing to right) in degrees): " << degval << endl;
  q3.GetYaw(degval);
  cout << "Yaw(rotation about z axis(pointing downwards)  in degrees): " << degval << endl;
  
  Degree deg; Vector3 vec;
  q3.GetRotationToAxis(deg, vec);
  cout << "Rotation to axis: " << deg.GetValue() << " degrees, about axis: " << vec << endl;

  q5.GetRoll(degval);
  cout << "Roll(rotation about x axis(pointing forwards)  in degrees): " << degval << endl;
  q5.GetPitch(degval);
  cout << "Pitch(rotation about y axis(pointing to right) in degrees): " << degval << endl;
  q5.GetYaw(degval);
  cout << "Yaw(rotation about z axis(pointing downwards)  in degrees): " << degval << endl;

  q5.GetRotationToAxis(deg, vec);
  cout << "Rotation to axis: " << deg.GetValue() << " degrees, about axis: " << vec << endl;

  cout << "******************************\n\n";
}

const int qcount = 1000000;

void QuaternionProfileTest()
{
  Quaternion q(Degree(90), Vector3(1, 0, 0));

  while ((float)clock() / (float)CLOCKS_PER_SEC < 5.f)
  {
    /*
    //used in matrix4 maketransform
    {
      Profiler profile("Quaternion extract 3x3 mat profiler");
      for (int i=0; i<qcount; ++i)
        q.GetRotationMatrix3();
    }
    */
    //constructor
    {
      Profiler profile("Quaternion constructor profiler");
      for (int i=0; i<qcount; ++i)
        Quaternion q(Degree(90), Vector3(1, 0, 0));
    }
  }
}

void QuaternionAllTests()
{
  QuaternionConstructorTest();
  QuaternionAccessAndAssignmentTest();
  QuaternionMutatorAndAccessorTest();
  QuaternionComparisonTest();
  QuaternionBasicArithmeticTest();
  QuaternionAdvancedOPTest();
  /*QuaternionGeomPropTest();*/
  QuaternionProfileTest();

  Quaternion q(Quaternion::IDENTITY);
  cout << "Size: " << sizeof(q) << endl;
}

#endif
