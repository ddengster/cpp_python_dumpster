
#ifndef _MATRIX3TEST_H
#define _MATRIX3TEST_H

#include "Math\Normal\MathHeaders.h"
#include "profiler\Profiler.h"
#include <iostream>
using namespace std;

void Matrix3ConstructorTest()
{
  cout << "*****Matrix3 Constructor Test*******\n";
  Matrix3 mat;
  cout << mat << endl;

  Matrix3 mat2(1.0f, 2.0f, 3.0f, 
               4.0f, 5.0f, 6.0f,
               7.0f, 8.0f, 9.0f);
  cout << mat2 << endl;

  float arr[3][3] = { { 11, 12, 13 },
                      { 11, 12, 13 },
                      { 11, 12, 13 }
                    };
  Matrix3 mat3(arr);
  cout << mat3 << endl;

  Matrix3 mat4(Vector3(21, 22, 23) , Vector3(24, 25, 26), Vector3(27, 28, 29));
  cout << mat4 << endl;

  Matrix3 mat5(mat2);
  cout << mat5 << endl;
  cout << "******************************\n\n";
}

void Matrix3AccessAndAssignmentTest()
{
  cout << "*****Matrix3 Access And Assignment Test*******\n";
  Matrix3 mat(1.0f, 2.0f, 3.0f, 
              4.0f, 5.0f, 6.0f,
              7.0f, 8.0f, 9.0f);

  cout << "Row 0: " << mat[0][0] << " " << mat[0][1] << " " << mat[0][2] << endl;
  //mat[3][0]; //assert
  
  float *m = mat.ptr();
  cout << "Row 0: " << m[0] << " " << m[1] << " " << m[2] << endl;

  Matrix3 mat2(1.5f, 2.5f, 3.5f, 
               4.5f, 5.5f, 6.5f,
               7.5f, 8.5f, 9.5f);
  mat = mat2;
  cout << mat << endl;

  cout << "******************************\n\n";
}

void Matrix3MutatorAndAccessorTest()
{
  cout << "*****Matrix3 Mutator And Accessor Test*******\n";
  Matrix3 mat(1.0f, 2.0f, 3.0f, 
              4.0f, 5.0f, 6.0f,
              7.0f, 8.0f, 9.0f);

  //mat.GetRow(3); //assert
  cout << "Row 0: " << mat.GetRow(0) << endl;
  cout << "Row 1: " << mat.GetRow(1) << endl;
  cout << "Row 2: " << mat.GetRow(2) << endl;

  //mat.GetColumn(3); //assert
  cout << "Column 0: " << mat.GetColumn(0) << endl;
  cout << "Column 1: " << mat.GetColumn(1) << endl;
  cout << "Column 2: " << mat.GetColumn(2) << endl;

  Vector3 vec(33.3f, 66.6f, 99.9f);
  Matrix3 mat2(mat);

  mat2.SetRow(vec, 0);
  //mat2.SetRow(vec, 3); //assert
  cout << "Set row 0 with vector3 " << vec << ": " << endl << mat2 << endl;

  mat2.SetColumn(vec, 2);
  cout << "Set column 2 with vector3 " << vec << ": " << endl << mat2 << endl;

  cout << "******************************\n\n";
}

void Matrix3ComparisonTest()
{
  cout << "*****Matrix3 Mutator And Accessor Test*******\n";
  Matrix3 mat(1.0f, 2.0f, 3.0f, 
              4.0f, 5.0f, 6.0f,
              7.0f, 8.0f, 9.0f);

  Matrix3 mat2(mat);
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

void Matrix3BasicArithmeticTest()
{
  cout << "*****Matrix3 Basic Arithmetic Test*******\n";
  Matrix3 mat(1.0f, 2.0f, 3.0f, 
              4.0f, 5.0f, 6.0f,
              7.0f, 8.0f, 9.0f);
  Matrix3 mat2(11.0f, 12.0f, 13.0f, 
               14.0f, 15.0f, 16.0f,
               17.0f, 18.0f, 19.0f);
  Vector3 vec(20.0f);
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

void Matrix3AdvancedArithmeticFuncTest()
{
  cout << "*****Matrix3 Basic Arithmetic Test*******\n";
  Matrix3 mat(1.0f, 2.0f, 3.0f, 
              4.0f, 5.0f, 6.0f,
              7.0f, 8.0f, 10.0f);
  Matrix3 mat2(11.0f, 12.0f, 13.0f, 
               14.0f, 15.0f, 16.0f,
               17.0f, 18.0f, 13.0f);
  Vector3 vec(20.0f);

  cout << "Determinant: " << mat.Determinant() << endl;
  cout << "Transpose:\n" << mat.TransposeCopy() << endl;
  cout << "Inverse: \n" << mat.InverseCopy() << endl;
  cout << "NormalizeCopy: \n" << mat.NormaliseCopy() << endl;

  cout << "******************************\n\n";
}

void Matrix3GeomPropTest()
{
  cout << "*****Matrix3 Geometric Properties Test*******\n";
  Matrix3 mat(Matrix3::IDENTITY);

  Vector2 vec(20.0f);

  mat.SetTranslate(vec);
  mat.SetScale(vec);

  cout << "Matrix: \n" << mat << endl;
  cout << "Scale: " << mat.GetScale() << endl;
  cout << "Translate: " << mat.GetTranslate() << endl;

  cout << "******************************\n\n";
}

void Matrix3ProfileTest()
{
  Matrix3 mat(Matrix3::IDENTITY);
  const int count = 100000;

  while ((float)clock() / (float)CLOCKS_PER_SEC < 5.f)
  {
    
    {
      Profiler profile("Matrix3 maketransform profiler");
      for (int i=0; i<count; ++i)
        mat.MakeTransform(Vector2(10, 20), Vector2(3, 3), Radian(45));
    }
  }
}

void Matrix3AllTests()
{
  Matrix3ConstructorTest();
  Matrix3AccessAndAssignmentTest();
  Matrix3MutatorAndAccessorTest();
  Matrix3ComparisonTest();
  Matrix3BasicArithmeticTest();
  Matrix3AdvancedArithmeticFuncTest();
  Matrix3GeomPropTest();
  Matrix3ProfileTest();

  Matrix3 mat(Matrix3::IDENTITY);
  cout << "Size: " << sizeof(mat) << endl;
}

#endif
