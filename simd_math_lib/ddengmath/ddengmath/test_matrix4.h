
#ifndef _Matrix4TEST_H
#define _Matrix4TEST_H

#include "Math\Normal\MathHeaders.h"
#include "profiler\Profiler.h"
#include <iostream>
using namespace std;

void Matrix4ConstructorTest()
{
  cout << "*****Matrix4 Constructor Test*******\n";
  Matrix4 mat;
  cout << mat << endl;

  Matrix4 mat2(1.0f, 2.0f, 3.0f, 4.0f,
               5.0f, 6.0f, 7.0f, 8.0f,
               9.0f, 10.0f, 11.0f, 12.0f,
               13.0f, 14.0f, 15.0f, 16.0f);
  cout << mat2 << endl;

  float arr[4][4] = { { 11, 12, 13, 14 },
                      { 11, 12, 13, 14 },
                      { 11, 12, 13, 14 },
                      { 11, 12, 13, 14 }
                    };
  Matrix4 mat3(arr);
  cout << mat3 << endl;

  Matrix4 mat4(Vector4(21, 22, 23, 24), Vector4(24, 25, 26, 27), Vector4(27, 28, 29, 30), Vector4(31, 32, 33, 34));
  cout << mat4 << endl;

  Matrix4 mat5(mat2);
  cout << mat5 << endl;
  cout << "******************************\n\n";
}

void Matrix4AccessAndAssignmentTest()
{
  cout << "*****Vector2 Access And Assignment Test*******\n";
  Matrix4 mat(1.0f, 2.0f, 3.0f, 4.0f,
              5.0f, 6.0f, 7.0f, 8.0f,
              9.0f, 10.0f, 11.0f, 12.0f,
              13.0f, 14.0f, 15.0f, 16.0f);

  cout << "Row 0: " << mat[0][0] << " " << mat[0][1] << " " << mat[0][2] << " " << mat[0][3] << endl;
  //mat[4][0]; //assert
  
  float *m = mat.ptr();
  cout << "Row 0: " << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << endl;

  Matrix4 mat2(1.5f, 2.5f, 3.5f, 4.5f,
               5.5f, 6.5f, 7.5f, 8.5f,
               9.5f, 10.5f, 11.5f, 12.5f,
               13.5f, 14.5f, 15.5f, 16.5f);
  mat = mat2;
  cout << mat << endl;

  cout << "******************************\n\n";
}

void Matrix4MutatorAndAccessorTest()
{
  cout << "*****Vector2 Mutator And Accessor Test*******\n";
  Matrix4 mat(1.0f, 2.0f, 3.0f, 4.0f,
              5.0f, 6.0f, 7.0f, 8.0f,
              9.0f, 10.0f, 11.0f, 12.0f,
              13.0f, 14.0f, 15.0f, 16.0f);

  //mat.GetRow(4); //assert
  cout << "Row 0: " << mat.GetRow(0) << endl;
  cout << "Row 1: " << mat.GetRow(1) << endl;
  cout << "Row 2: " << mat.GetRow(2) << endl;
  cout << "Row 3: " << mat.GetRow(3) << endl;

  //mat.GetColumn(4); //assert
  cout << "Column 0: " << mat.GetColumn(0) << endl;
  cout << "Column 1: " << mat.GetColumn(1) << endl;
  cout << "Column 2: " << mat.GetColumn(2) << endl;
  cout << "Column 3: " << mat.GetColumn(3) << endl;

  Vector4 vec(33.3f, 66.6f, 99.9f, 122.2f);
  Matrix4 mat2(mat);

  mat2.SetRow(vec, 0);
  //mat2.SetRow(vec, 4); //assert
  cout << "Set row 0 with Vector4 " << vec << ": " << endl << mat2 << endl;

  mat2.SetColumn(vec, 2);
  cout << "Set column 2 with Vector4 " << vec << ": " << endl << mat2 << endl;

  cout << "******************************\n\n";
}

void Matrix4ComparisonTest()
{
  cout << "*****Matrix4 Mutator And Accessor Test*******\n";
  Matrix4 mat(1.0f, 2.0f, 3.0f, 4.0f,
              5.0f, 6.0f, 7.0f, 8.0f,
              9.0f, 10.0f, 11.0f, 12.0f,
              13.0f, 14.0f, 15.0f, 16.0f);

  Matrix4 mat2(mat);
  if (mat2 == mat)
    cout << "Matrices are equal" << endl;
  else
    cout << "Matrices are not equal" << endl;

  mat2.m_2d[0][0] = 10.0f;
  if (mat2 != mat)
    cout << "Matrices are not equal" << endl;
  else
    cout << "Matrices are equal" << endl;

  cout << "******************************\n\n";
}

void Matrix4BasicArithmeticTest()
{
  cout << "*****Matrix4 Basic Arithmetic Test*******\n";
  Matrix4 mat(1.0f, 2.0f, 3.0f, 4.0f,
              5.0f, 6.0f, 7.0f, 8.0f,
              9.0f, 10.0f, 11.0f, 12.0f,
              13.0f, 14.0f, 15.0f, 16.0f);
  Matrix4 mat2(11.0f, 12.0f, 13.0f, 14.0f,
               15.0f, 16.0f, 17.0f, 18.0f, 
               19.0f, 20.0f, 21.0f, 22.0f,
               23.0f, 24.0f, 25.0f, 26.0f);
  Vector4 vec(20.0f);
  float scalar = 0.5f;

  cout << "Plus: \n" << mat + mat2 << endl;
  cout << "Minus: \n" << mat - mat2 << endl;
  cout << "Multiply(matrix concatenation): \n" << mat * mat2 << endl;
  cout << "Multiply(vector concatenation): \n" << mat * vec << endl;
  cout << "Negation: \n" << -mat << endl;
  cout << "Multiply(scalar): \n" << mat * scalar << endl;
  cout << "Divide(scalar): \n" << mat / scalar << endl;

  cout << "Add(scalar+mat): \n" << scalar + mat << endl;
  cout << "Subtract(scalar-mat): \n" << scalar - mat << endl;
  cout << "Multiply(scalar*mat): \n" << scalar * mat << endl;
  cout << "Divide(scalar/mat): \n" << scalar / mat << endl;

  mat += mat2;
  cout << "+= :\n" << mat << endl;
  mat -= mat2;
  cout << "-= :\n" << mat << endl;
  mat *= mat2;
  cout << "*= :\n" << mat << endl;
  mat /= 100.0f;
  cout << "/= :\n" << mat << endl;

  cout << "******************************\n\n";
}

void Matrix4AdvancedArithmeticFuncTest()
{
  cout << "*****Matrix4 Basic Arithmetic Test*******\n";
  Matrix4 mat(1.0f, 2.0f, 3.0f, 4.0f,
              5.0f, 6.0f, 7.0f, 8.0f,
              9.0f, 10.0f, 12.0f, 12.0f,
              13.0f, 14.0f, 15.0f, 17.0f);
  Matrix4 mat2(11.0f, 12.0f, 13.0f, 14.0f,
               15.0f, 16.0f, 17.0f, 18.0f, 
               19.0f, 20.0f, 21.0f, 22.0f,
               23.0f, 24.0f, 25.0f, 26.0f);
  Vector4 vec(20.0f);

  cout << "Determinant: " << mat.Determinant() << endl;
  cout << "Transpose:\n" << mat.TransposeCopy() << endl;
  cout << "Inverse: \n" << mat.InverseCopy() << endl;
  cout << "NormalizeCopy: \n" << mat.NormaliseCopy() << endl;

  cout << "******************************\n\n";
}

void Matrix4GeomPropTest()
{
  cout << "*****Matrix4 Geometric Properties Test*******\n";
  Matrix4 mat(Matrix4::IDENTITY);

  Vector3 vec(20.0f);

  mat.SetTranslate(vec);
  mat.SetScale(vec);

  cout << "Matrix: \n" << mat << endl;
  cout << "Scale: " << mat.GetScale() << endl;
  cout << "Translate: " << mat.GetTranslate() << endl;

  Vector4 position(1, 0, 0, 1);
  mat.MakeTransform(Vector3(0, 0, 0), Vector3(3, 3, 3), Quaternion(Degree(90), Vector3(1, 0, 0)));
  cout << mat << endl;
  cout << mat * position << endl;

  cout << "******************************\n\n";
}

void Matrix4QuaternionTest()
{
  cout << "*****Matrix4 Quaternion Test*******\n";
  Quaternion q(Degree(90), Vector3(0, 0, 1));
  Quaternion q2(Degree(90), Vector3(0, 1, 0));
  Quaternion q3(Degree(90), Vector3(1, 0, 0));
  Matrix4 mat(q);
  

  cout << "Matrix generated from quaternion:\n" << mat << endl;
  mat = q2;
  cout << "Matrix generated from quaternion:\n" << mat << endl;
  mat = q3;
  cout << "Matrix generated from quaternion:\n" << mat << endl;

  Quaternion q4 = mat.ExtractQuaternion();
  cout << "Quaternion extracted: " << q4 << "same as: \n" << q3 << endl;

  cout << "******************************\n\n";
}

void Matrix4ProfileTest()
{
  Matrix4 mat(Matrix4::IDENTITY);
  const int count = 100000;
  const int count2 = 10000000;

  Matrix4 mat2(2, 3, 4, 5, 
               33, 32, 35, 35,
               56, 65, 34, 23,
               346, 23, 35, 11);

  while ((float)clock() / (float)CLOCKS_PER_SEC < 5.f)
  {
    
    {
      Profiler profile("Matrix4 maketransform profiler");
      for (int i=0; i<count; ++i)
        mat.MakeTransform(Vector3(0, 0, 0), Vector3(3, 3, 3), Quaternion(Degree(22), Vector3(1, 0, 0)));
    }
    
    /*
    {
      Profiler profile("Matrix4 constructor profiler");
      for (int i=0; i<count2; ++i)
        Matrix4 mat4(Matrix4::IDENTITY);
    }
    */
    /*
    {
      Profiler profile("Matrix4 inverse profiler");
      for (int i=0; i<count2; ++i)
        mat2.Inverse();
    }
    */
  }
}

void Matrix4AllTests()
{
  Matrix4ConstructorTest();
  Matrix4AccessAndAssignmentTest();
  Matrix4MutatorAndAccessorTest();
  Matrix4ComparisonTest();
  Matrix4BasicArithmeticTest();
  Matrix4AdvancedArithmeticFuncTest();
  Matrix4GeomPropTest();
  Matrix4QuaternionTest();

  Matrix4ProfileTest();

  Matrix4 mat(Matrix4::IDENTITY);
  cout << "Size: " << sizeof(mat) << endl;
}

#endif
