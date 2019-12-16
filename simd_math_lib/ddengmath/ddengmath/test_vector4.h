
#ifndef TEST_Vector4_H
#define TEST_Vector4_H

#include "Math\Normal\MathHeaders.h"
#include <iostream>
using namespace std;

void Vector4ConstructorTest()
{
  cout << "*****Vector4 Constructor Test*******\n";
  Vector4 vec;
  cout << vec << endl;

  Vector4 vec2(10.0f);
  cout << vec2 << endl;

  Vector4 vec3(1, 2, 3, 4);
  cout << vec3 << endl;

  float coord[4] = { 3.2f, -4.2f, 99.04f, -44.2f };
  Vector4 vec4(coord);
  cout << vec4 << endl;

  Vector4 vec5(vec2);
  cout << vec5 << endl;

  Vector3 vec22(1);
  Vector4 vec6(vec22);
  cout << vec6 << endl;
  
  cout << "******************************\n\n";
}

void Vector4AccessTest()
{
  cout << "*****Vector4 Access Test*******\n";
  Vector4 vec3(1.2f, 2.4f, 3.6f, 4.8f);
  cout << vec3 << endl;

  cout << vec3[0] << endl;
  cout << vec3[1] << endl;
  cout << vec3[2] << endl;
  cout << vec3[3] << endl;

  vec3.ptr()[0] = vec3.ptr()[2];
  cout << vec3.ptr()[0] << endl;
  cout << vec3.ptr()[1] << endl;
  cout << vec3.ptr()[2] << endl;
  cout << vec3.ptr()[3] << endl;

  const Vector4 vec4(3.4f, 4.5f, 5.6f, 6.7f);
  cout << vec4[0] << endl;
  cout << vec4[1] << endl;
  cout << vec4[2] << endl;
  cout << vec4[3] << endl;

  //vec4.ptr()[0] = vec4.ptr()[1]; //assert
  cout << vec4.ptr()[0] << endl;
  cout << vec4.ptr()[1] << endl;
  cout << vec4.ptr()[2] << endl;
  cout << vec4.ptr()[3] << endl;
  //vec3[3]; //assert
  cout << "******************************\n\n";
}

void Vector4AssignmentTest()
{
  cout << "*****Vector4 Assignment Test*******\n";
  Vector4 vec3(1.2f, 2.4f, 3.6f, 4.8f);
  vec3 = 1.0f;
  cout << vec3 << endl;
  vec3 = Vector4(10.2f, -44.0f, 554.3f, -2121.2f);
  cout << vec3 << endl;

  Vector3 vec22(2.4f, 3.3f, 4.2f);
  vec3 = vec22;
  cout << vec3 << endl;
  cout << "******************************\n\n";
}

void Vector4ComparisonTest()
{
  cout << "*****Vector4 Comparison Test*******\n";
  Vector4 vec(1.2f, 2.4f, 3.6f, 4.8f);
  const Vector4 vec2(3.4f, 4.5f, 5.6f, 6.7f);
  Vector4 vec3(3.4f, 4.5f, 5.6f, 6.7f);
  Vector4 vec4(3.1f, 20.0f, 55, 88);

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

void Vector4BasicMemberArithmeticTest()
{
  cout << "*****Vector4 BasicMemberArithmetic Test*******\n";
  Vector4 vec(1.2f, 2.4f, 3.6f, 4.8f);
  Vector4 vec2(2.3f, 4.6f, 5.6f, 6.7f);
  float scalar = 2.0f;

  cout << "Plus: " << vec + vec2 << endl;
  cout << "Minus: " << vec - vec2 << endl;
  cout << "Multiply(Vector): " << vec * vec2 << endl;
  cout << "Divide(Vector): " << vec / vec2 << endl;
  cout << "Multiply(Scalar): " << vec * scalar << endl;
  cout << "Divide(Scalar): " << vec / scalar << endl;
  cout << "Negate: " << -vec << endl;

  //Vector4 result = vec / 0.0f; // assert

  Vector4 vec3(20, 20, 20, 20);
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

void Vector4GlobalFriendFuncTest()
{
  cout << "*****Vector4 GlobalFriendFunc Test*******\n";
  Vector4 vec(1.2f, 2.4f, 3.6f, 4.8f);
  float scalar = 2.0f;

  cout << "Plus: " << scalar + vec << endl;
  cout << "Vec Minus Scalar: " << vec - scalar << endl;
  cout << "Scalar Minus Vec: " << scalar - vec << endl;
  cout << "Scalar Multiply Vec: " << scalar * vec << endl;
  cout << "Scalar Divide Vec: " << scalar / vec << endl;

  cout << "******************************\n\n";
}

void Vector4StaticFunc()
{
  cout << "*****Vector4 Static Func Test*******\n";
  Vector4 vec(1.2f, 2.4f, 3.6f, 4.8f);
  Vector4 vec2(3.4f, 4.5f, 5.6f, 6.7f);
  float scalar = 2.0f;
  Vector4 result;

  Vector4::Add(result, vec, vec2);
  cout << "Static Add: " << result << endl;
  Vector4::Subtract(result, vec, vec2);
  cout << "Static Subtract: " << result << endl;
  Vector4::Multiply(result, vec, vec2);
  cout << "Static Multiply: " << result << endl;
  Vector4::Multiply(result, scalar, vec);
  cout << "Static Multiply: " << result << endl;
  Vector4::Multiply(result, vec, scalar);
  cout << "Static Multiply: " << result << endl;
  Vector4::Divide(result, vec, vec2);
  cout << "Static Divide: " << result << endl;
  Vector4::Divide(result, vec, scalar);
  cout << "Static Divide: " << result << endl;
  Vector4::Divide(result, scalar, vec);
  cout << "Static Divide: " << result << endl;

  cout << "******************************\n\n";
}

void Vector4AdvancedFunc()
{
  cout << "*****Vector4 AdvancedFunc Test*******\n";
  Vector4 vec(1.2f, 2.4f, 3.6f, 4.8f);
  Vector4 vec2(3.0f, 3.0f, 3.0f, 3.0f);

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

  vec2 = Vector4(3.0f, 3.0f, 3.0f, 3.0f);
  cout << "Cross Product: " << vec.CrossProduct(vec2) << endl;

  vec2 = Vector4(0.0f, 0.0f, 0.0f, 0.0f);
  cout << "Vec2 set to 0,0,0. Is 0 length: " << vec2.IsZeroLength() << endl;

  vec2 = Vector4(1.0f, 0.0f, 0.0f, 0.0f);
  cout << "Reflect vec about (1, 0, 0) : " << vec.Reflect(vec2) << endl;

  Vector4 vec3(1.0f, 1.0f, 1.0f, 1.0f);
  cout << vec3 << " and " << vec2 << "same direction? " << vec3.DirectionEquals(vec2, Radian()) << endl;

  vec3 = Vector4(1.1f, -0.1f, 0.0f, -55.0f);
  //vec3.Clamp(Vector4(0,0,0), Vector4(1,1,1));
  vec3.Clamp(0, 1);
  cout << "Vec3 clamped: " << vec3 << endl;
  cout << "******************************\n\n";
}

void Vector4StaticVar()
{
  cout << "*****Vector4 StaticVar Test*******\n";
  Vector4 vec(Vector4::ZERO);
  cout << "ZERO: " << vec << endl;
  vec = Vector4::UNIT_X;
  cout << "UNIT_X: " << vec << endl;
  vec = Vector4::UNIT_Y;
  cout << "UNIT_Y: " << vec << endl;
  vec = Vector4::UNIT_Z;
  cout << "UNIT_Z: " << vec << endl;
  vec = Vector4::UNIT_W;
  cout << "UNIT_W: " << vec << endl;
  vec = Vector4::NEGATIVE_UNIT_X;
  cout << "NEGATIVE_UNIT_X: " << vec << endl;
  vec = Vector4::NEGATIVE_UNIT_Y;
  cout << "NEGATIVE_UNIT_Y: " << vec << endl;
  vec = Vector4::NEGATIVE_UNIT_Z;
  cout << "NEGATIVE_UNIT_Z: " << vec << endl;
  vec = Vector4::NEGATIVE_UNIT_W;
  cout << "NEGATIVE_UNIT_W: " << vec << endl;
  vec = Vector4::UNIT_SCALE;
  cout << "UNIT_SCALE: " << vec << endl;

  cout << "******************************\n\n";
}

void Vector4AllTests()
{
  Vector4ConstructorTest();
  Vector4AccessTest();
  Vector4AssignmentTest();
  Vector4ComparisonTest();
  Vector4BasicMemberArithmeticTest();
  Vector4GlobalFriendFuncTest();
  Vector4StaticFunc();
  Vector4AdvancedFunc();
  Vector4StaticVar();

  Vector4 vec;
  cin >> vec ;
  cout << "instream: " << vec << endl;
}
#endif
