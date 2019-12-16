
#ifndef TEST_VECTOR2_H
#define TEST_VECTOR2_H

#include "Math\Normal\MathHeaders.h"
#include <iostream>
using namespace std;

void Vector2ConstructorTest()
{
  cout << "*****Vector2 Constructor Test*******\n";
  Vector2 vec;
  cout << vec << endl;

  Vector2 vec2(10.0f);
  cout << vec2 << endl;

  Vector2 vec3(1, 2);
  cout << vec3 << endl;

  float coord[2] = { 3.2f, -4.2f };
  Vector2 vec4(coord);
  cout << vec4 << endl;

  Vector2 vec5(vec2);
  cout << vec5 << endl;

  cout << "******************************\n\n";
}

void Vector2AccessTest()
{
  cout << "*****Vector2 Access Test*******\n";
  Vector2 vec3(1.2f, 2.4f);
  cout << vec3 << endl;

  cout << vec3[0] << endl;
  cout << vec3[1] << endl;

  vec3.ptr()[0] = vec3.ptr()[1];
  cout << vec3.ptr()[0] << endl;
  cout << vec3.ptr()[1] << endl;

  const Vector2 vec4(3.4f, 4.5f);
  cout << vec4[0] << endl;
  cout << vec4[1] << endl;

  //vec4.ptr()[0] = vec4.ptr()[1]; //assert
  cout << vec4.ptr()[0] << endl;
  cout << vec4.ptr()[1] << endl;
  //vec3[2]; //assert
  cout << "******************************\n\n";
}

void Vector2AssignmentTest()
{
  cout << "*****Vector2 Assignment Test*******\n";
  Vector2 vec3(1.2f, 2.4f);
  vec3 = 1.0f;
  cout << vec3 << endl;
  vec3 = Vector2(10.2f, -44.0f);
  cout << vec3 << endl;

  cout << "******************************\n\n";
}

void Vector2ComparisonTest()
{
  cout << "*****Vector2 Comparison Test*******\n";
  Vector2 vec(1.2f, 2.4f);
  const Vector2 vec2(3.4f, 4.5f);
  Vector2 vec3(3.4f, 4.5f);
  Vector2 vec4(3.1f, 20.0f);

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

void Vector2BasicMemberArithmeticTest()
{
  cout << "*****Vector2 BasicMemberArithmetic Test*******\n";
  Vector2 vec(1.2f, 2.4f);
  Vector2 vec2(2.3f, 4.6f);
  float scalar = 2.0f;

  cout << "Plus: " << vec + vec2 << endl;
  cout << "Minus: " << vec - vec2 << endl;
  cout << "Multiply(Vector): " << vec * vec2 << endl;
  cout << "Divide(Vector): " << vec / vec2 << endl;
  cout << "Multiply(Scalar): " << vec * scalar << endl;
  cout << "Divide(Scalar): " << vec / scalar << endl;
  cout << "Negate: " << -vec << endl;

  //Vector2 result = vec / 0.0f; // assert

  Vector2 vec3(20, 20);
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

void Vector2GlobalFriendFuncTest()
{
  cout << "*****Vector2 GlobalFriendFunc Test*******\n";
  Vector2 vec(1.2f, 2.4f);
  float scalar = 2.0f;

  cout << "Plus: " << scalar + vec << endl;
  cout << "Vec Minus Scalar: " << vec - scalar << endl;
  cout << "Scalar Minus Vec: " << scalar - vec << endl;
  cout << "Scalar Multiply Vec: " << scalar * vec << endl;
  cout << "Scalar Divide Vec: " << scalar / vec << endl;

  cout << "******************************\n\n";
}

void Vector2StaticFunc()
{
  cout << "*****Vector2 Static Func Test*******\n";
  Vector2 vec(1.2f, 2.4f);
  Vector2 vec2(3.4f, 4.5f);
  float scalar = 2.0f;
  Vector2 result;

  Vector2::Add(result, vec, vec2);
  cout << "Static Add: " << result << endl;
  Vector2::Subtract(result, vec, vec2);
  cout << "Static Subtract: " << result << endl;
  Vector2::Multiply(result, vec, vec2);
  cout << "Static Multiply: " << result << endl;
  Vector2::Multiply(result, scalar, vec);
  cout << "Static Multiply: " << result << endl;
  Vector2::Multiply(result, vec, scalar);
  cout << "Static Multiply: " << result << endl;
  Vector2::Divide(result, vec, vec2);
  cout << "Static Divide: " << result << endl;
  Vector2::Divide(result, vec, scalar);
  cout << "Static Divide: " << result << endl;
  Vector2::Divide(result, scalar, vec);
  cout << "Static Divide: " << result << endl;

  cout << "******************************\n\n";
}

void Vector2AdvancedFunc()
{
  cout << "*****Vector2 AdvancedFunc Test*******\n";
  Vector2 vec(1.2f, 2.4f);
  Vector2 vec2(3.0f, 3.0f);

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

  cout << "Perpendicular: " << vec.Perpendicular() << endl;

  vec2 = Vector2(3.0f, 3.0f);
  cout << "Cross Product: " << vec.CrossProduct(vec2) << endl;

  vec2 = Vector2(0.0f, 0.0f);
  cout << "Vec2 set to 0,0. Is 0 length: " << vec2.IsZeroLength() << endl;

  vec2 = Vector2(1.0f, 0.0f);
  cout << "Reflect vec about (1, 0) : " << vec.Reflect(vec2) << endl;

  cout << "******************************\n\n";
}

void Vector2StaticVar()
{
  cout << "*****Vector2 StaticVar Test*******\n";
  Vector2 vec(Vector2::ZERO);
  cout << "ZERO: " << vec << endl;
  vec = Vector2::UNIT_X;
  cout << "UNIT_X: " << vec << endl;
  vec = Vector2::UNIT_Y;
  cout << "UNIT_Y: " << vec << endl;
  vec = Vector2::NEGATIVE_UNIT_X;
  cout << "NEGATIVE_UNIT_X: " << vec << endl;
  vec = Vector2::NEGATIVE_UNIT_Y;
  cout << "NEGATIVE_UNIT_Y: " << vec << endl;
  vec = Vector2::UNIT_SCALE;
  cout << "UNIT_SCALE: " << vec << endl;

  cout << "******************************\n\n";
}

void Vector2AllTests()
{
  Vector2ConstructorTest();
  Vector2AccessTest();
  Vector2AssignmentTest();
  Vector2ComparisonTest();
  Vector2BasicMemberArithmeticTest();
  Vector2GlobalFriendFuncTest();
  Vector2StaticFunc();
  Vector2AdvancedFunc();
  Vector2StaticVar();

  Vector2 vec;
  cin >> vec ;
  cout << "instream: " << vec << endl;
}
#endif