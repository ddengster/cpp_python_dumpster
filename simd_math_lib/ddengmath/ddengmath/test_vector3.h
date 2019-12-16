
#ifndef TEST_VECTOR3_H
#define TEST_VECTOR3_H

#include "Math\Normal\MathHeaders.h"
#include "profiler\Profiler.h"
#include <iostream>
using namespace std;

void Vector3ConstructorTest()
{
  cout << "*****Vector3 Constructor Test*******\n";
  Vector3 vec;
  cout << vec << endl;

  Vector3 vec2(10.0f);
  cout << vec2 << endl;

  Vector3 vec3(1, 2, 3);
  cout << vec3 << endl;

  float coord[3] = { 3.2f, -4.2f, 99.04f };
  Vector3 vec4(coord);
  cout << vec4 << endl;

  Vector3 vec5(vec2);
  cout << vec5 << endl;

  Vector2 vec22(1);
  Vector3 vec6(vec22);
  cout << vec6 << endl;
  
  cout << "******************************\n\n";
}

void Vector3AccessTest()
{
  cout << "*****Vector3 Access Test*******\n";
  Vector3 vec3(1.2f, 2.4f, 3.6f);
  cout << vec3 << endl;

  cout << vec3[0] << endl;
  cout << vec3[1] << endl;
  cout << vec3[2] << endl;

  vec3.ptr()[0] = vec3.ptr()[2];
  cout << vec3.ptr()[0] << endl;
  cout << vec3.ptr()[1] << endl;
  cout << vec3.ptr()[2] << endl;

  const Vector3 vec4(3.4f, 4.5f, 5.6f);
  cout << vec4[0] << endl;
  cout << vec4[1] << endl;
  cout << vec4[2] << endl;

  //vec4.ptr()[0] = vec4.ptr()[1]; //assert
  cout << vec4.ptr()[0] << endl;
  cout << vec4.ptr()[1] << endl;
  cout << vec4.ptr()[2] << endl;
  //vec3[3]; //assert
  cout << "******************************\n\n";
}

void Vector3AssignmentTest()
{
  cout << "*****Vector3 Assignment Test*******\n";
  Vector3 vec3(1.2f, 2.4f, 3.6f);
  vec3 = 1.0f;
  cout << vec3 << endl;
  vec3 = Vector3(10.2f, -44.0f, 554.3f);
  cout << vec3 << endl;

  Vector2 vec22(2.4f, 3.3f);
  vec3 = vec22;
  cout << vec3 << endl;
  cout << "******************************\n\n";
}

void Vector3ComparisonTest()
{
  cout << "*****Vector3 Comparison Test*******\n";
  Vector3 vec(1.2f, 2.4f, 3.6f);
  const Vector3 vec2(3.4f, 4.5f, 5.6f);
  Vector3 vec3(3.4f, 4.5f, 5.6f);
  Vector3 vec4(3.1f, 20.0f, 55);

  if (vec == vec2)
    cout << vec << " is equal to " << vec2 << endl;
  else
    cout << vec << " is not equal to " << vec2 << endl;

  if (vec3 != vec2)
    cout << vec3 << " is not equal to " << vec2 << endl;
  else
    cout << vec3 << " is equal to " << vec2 << endl;

  if (vec < vec2)
    cout << vec << " is less than " << vec2 << endl;
  if (vec2 > vec)
    cout << vec2 << " is more than " << vec << endl;
  
  if (vec4 < vec2)
    cout << vec4 << " is less than " << vec2 << endl;
  else if (vec4 > vec2)
    cout << vec4 << " is more than " << vec2 << endl;
  else 
    cout << "This is supposed to print because " << vec4 << " is neither less than nor more than " << vec2 << endl;

  cout << "IsEqual with error tolerance 3.0f: " << vec.IsEqual(vec2, 3.0f) << endl;
  cout << "******************************\n\n";
}

void Vector3BasicMemberArithmeticTest()
{
  cout << "*****Vector3 BasicMemberArithmetic Test*******\n";
  Vector3 vec(1.2f, 2.4f, 3.6f);
  Vector3 vec2(2.3f, 4.6f, 5.6f);
  float scalar = 2.0f;

  cout << "Plus: " << vec + vec2 << endl;
  cout << "Minus: " << vec - vec2 << endl;
  cout << "Multiply(Vector): " << vec * vec2 << endl;
  cout << "Divide(Vector): " << vec / vec2 << endl;
  cout << "Multiply(Scalar): " << vec * scalar << endl;
  cout << "Divide(Scalar): " << vec / scalar << endl;
  cout << "Negate: " << -vec << endl;

  //Vector3 result = vec / 0.0f; // assert

  Vector3 vec3(20, 20, 20);
  vec3 += vec;
  cout << "Vector + - * / \n";
  cout << "+= : " << vec3 << endl;
  vec3 -= vec;
  cout << "-= : " << vec3 << endl;
  vec3 *= vec;
  cout << "*= : " << vec3 << endl;
  vec3 /= vec;
  cout << "/= : " << vec3 << endl;
  
  cout << "Scalar + - * / \n";
  vec3 += scalar;
  cout << "+= : " << vec3 << endl;
  vec3 -= scalar;
  cout << "-= : " << vec3 << endl;
  vec3 *= scalar;
  cout << "*= : " << vec3 << endl;
  vec3 /= scalar;
  cout << "/= : " << vec3 << endl;

  vec3 + 3.0f;

  cout << "******************************\n\n";
}

void Vector3GlobalFriendFuncTest()
{
  cout << "*****Vector3 GlobalFriendFunc Test*******\n";
  Vector3 vec(1.2f, 2.4f, 3.6f);
  float scalar = 2.0f;

  cout << "Plus: " << scalar + vec << endl;
  cout << "Vec Minus Scalar: " << vec - scalar << endl;
  cout << "Scalar Minus Vec: " << scalar - vec << endl;
  cout << "Scalar Multiply Vec: " << scalar * vec << endl;
  cout << "Scalar Divide Vec: " << scalar / vec << endl;

  cout << "******************************\n\n";
}

void Vector3StaticFunc()
{
  cout << "*****Vector3 Static Func Test*******\n";
  Vector3 vec(1.2f, 2.4f, 3.6f);
  Vector3 vec2(3.4f, 4.5f, 5.6f);
  float scalar = 2.0f;
  Vector3 result;

  Vector3::Add(result, vec, vec2);
  cout << "Static Add: " << result << endl;
  Vector3::Subtract(result, vec, vec2);
  cout << "Static Subtract: " << result << endl;
  Vector3::Multiply(result, vec, vec2);
  cout << "Static Multiply: " << result << endl;
  Vector3::Multiply(result, scalar, vec);
  cout << "Static Multiply: " << result << endl;
  Vector3::Multiply(result, vec, scalar);
  cout << "Static Multiply: " << result << endl;
  Vector3::Divide(result, vec, vec2);
  cout << "Static Divide: " << result << endl;
  Vector3::Divide(result, vec, scalar);
  cout << "Static Divide: " << result << endl;
  Vector3::Divide(result, scalar, vec);
  cout << "Static Divide: " << result << endl;

  cout << "******************************\n\n";
}

void Vector3AdvancedFunc()
{
  cout << "*****Vector3 AdvancedFunc Test*******\n";
  Vector3 vec(1.2f, 2.4f, 3.6f);
  Vector3 vec2(3.0f, 3.0f, 3.0f);

  cout << "Length: " << vec.Length() << endl;
  cout << "Squared Length: " << vec.SquaredLength() << endl;

  cout << "Distance to: " << vec.Distance(vec2) << endl;
  cout << "Squared Distance to: " << vec.SquaredDistance(vec2) << endl;

  cout << "Dot Product: " << vec.DotProduct(vec2) << endl;
  cout << "Midpoint: " << vec.MidPoint(vec2) << endl;

  cout << "NormalisedCopy: " << vec.NormalisedCopy() << endl;

  vec2.MakeCeil(vec);
  cout << "MakeCeil with vec: " << vec2 << endl;
  vec2.MakeFloor(vec);
  cout << "MakeFloor with vec: " << vec2 << endl;

  vec2 = Vector3(3.0f, 3.0f, 3.0f);
  cout << "Cross Product: " << vec.CrossProduct(vec2) << endl;

  vec2 = Vector3(0.0f, 0.0f, 0.0f);
  cout << "Vec2 set to 0,0,0. Is 0 length: " << vec2.IsZeroLength() << endl;

  vec2 = Vector3(1.0f, 0.0f, 0.0f);
  cout << "Reflect vec about (1, 0, 0) : " << vec.Reflect(vec2) << endl;

  vec2 = Vector3(55.0f, 0.0f, 55.0f);
  Vector3 vec3(1.0f, 0.0f, 1.0f);
  cout << vec3 << " and " << vec2 << "same direction? " << vec3.DirectionEquals(vec2) << endl;

  vec3 = Vector3(1.1f, -0.1f, 0.0f);
  //vec3.Clamp(Vector3(0,0,0), Vector3(1,1,1));
  vec3.Clamp(0, 1);
  cout << "Vec3 clamped: " << vec3 << endl;
  cout << "******************************\n\n";
}

void Vector3StaticVar()
{
  cout << "*****Vector3 StaticVar Test*******\n";
  Vector3 vec(Vector3::ZERO);
  cout << "ZERO: " << vec << endl;
  vec = Vector3::UNIT_X;
  cout << "UNIT_X: " << vec << endl;
  vec = Vector3::UNIT_Y;
  cout << "UNIT_Y: " << vec << endl;
  vec = Vector3::UNIT_Z;
  cout << "UNIT_Z: " << vec << endl;
  vec = Vector3::NEGATIVE_UNIT_X;
  cout << "NEGATIVE_UNIT_X: " << vec << endl;
  vec = Vector3::NEGATIVE_UNIT_Y;
  cout << "NEGATIVE_UNIT_Y: " << vec << endl;
  vec = Vector3::NEGATIVE_UNIT_Z;
  cout << "NEGATIVE_UNIT_Z: " << vec << endl;
  vec = Vector3::UNIT_SCALE;
  cout << "UNIT_SCALE: " << vec << endl;

  cout << "******************************\n\n";
}

void Vector3ProfileTest()
{
  Vector3 vec(1, 2, 3);
  Vector3 vec2(5, 2, 7);
  const int count = 100000000;
  
  while ((float)clock() / (float)CLOCKS_PER_SEC < 5.f)
  {
    /*
    {
      Profiler profile("Vector3 dot product profiler");
      for (int i=0; i<count; ++i)
        vec.DotProduct(vec);
    }
    */
    /*
    {
      Profiler profile("Vector3 cross product profiler");
      for (int i=0; i<count; ++i)
        vec.CrossProduct(vec);
    }*/
    {
      Profiler profile("Vector3 addition profiler");
      for (int i=0; i<count; ++i)
        vec += vec2;
    }
  }
}

void Vector3AllTests()
{
  Vector3ConstructorTest();
  Vector3AccessTest();
  Vector3AssignmentTest();
  Vector3ComparisonTest();
  Vector3BasicMemberArithmeticTest();
  Vector3GlobalFriendFuncTest();
  Vector3StaticFunc();
  Vector3AdvancedFunc();
  Vector3StaticVar();

  Vector3ProfileTest();

  //Vector3 vec;
  //cin >> vec ;
  //cout << "instream: " << vec << endl;
}
#endif