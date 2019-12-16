
#ifndef _MATRIX3_H
#define _MATRIX3_H

#include "GenericMath.h"
#include "Vector3.h"
#include "Vector2.h"

/** Column major 3x3 matrix. ie. XYZ Translation to the rightmost column **/
/** Top left element has row index 0, column index 0 and bottom right has row index 2, column index 2 **/
class Matrix3
{
public:
  union
  {
    float m[9];
    float m_2d[3][3]; // [row] [column]
  };

  static const Matrix3 ZERO;
  static const Matrix3 IDENTITY;

public:
  //Constructors
  inline Matrix3() { }
  inline Matrix3(const Matrix3& rhs)             { memcpy(m, rhs.m, 9 * sizeof(float)); }
  inline explicit Matrix3(const float arr[3][3]) { memcpy(m, arr, 9 * sizeof(float));   }
  Matrix3(float fEntry00, float fEntry01, float fEntry02,
				  float fEntry10, float fEntry11, float fEntry12,
				  float fEntry20, float fEntry21, float fEntry22) // no forced inline
  { 
    m_2d[0][0] = fEntry00; m_2d[0][1] = fEntry01; m_2d[0][2] = fEntry02;
    m_2d[1][0] = fEntry10; m_2d[1][1] = fEntry11; m_2d[1][2] = fEntry12;
    m_2d[2][0] = fEntry20; m_2d[2][1] = fEntry21; m_2d[2][2] = fEntry22;
  }
  inline Matrix3(const Vector3& row0, const Vector3& row1, const Vector3& row2) 
  { 
    m_2d[0][0] = row0.x; m_2d[0][1] = row0.y; m_2d[0][2] = row0.z;
    m_2d[1][0] = row1.x; m_2d[1][1] = row1.y; m_2d[1][2] = row1.z;
    m_2d[2][0] = row2.x; m_2d[2][1] = row2.y; m_2d[2][2] = row2.z;
  }

  //access op
  inline float* operator[](unsigned int row) const { assert(row < 3); return (float*)m_2d[row]; }
  inline const float* ptr() const { return m; }
  inline float* ptr()             { return m; }

  //assignment op
  inline Matrix3& operator=(const Matrix3& rhs) { memcpy(m, rhs.m, 9 * sizeof(float)); return *this;}

  //mutators and accessors
  inline Vector3 GetColumn(unsigned int column) const { assert(column < 3); return Vector3(m_2d[0][column], m_2d[1][column], m_2d[2][column]); }
  inline void SetColumn(const Vector3& vec, unsigned int column) {
    assert(column < 3); 
    m_2d[0][column] = vec.x; m_2d[1][column] = vec.y; m_2d[2][column] = vec.z; 
  }
  inline Vector3 GetRow(unsigned int row) const { 
    assert(row < 3); 
    return Vector3(m_2d[row][0], m_2d[row][1], m_2d[row][2]);
  }
  inline void SetRow(const Vector3& vec, unsigned int row) {
    assert(row < 3); 
    m_2d[row][0] = vec.x; m_2d[row][1] = vec.y; m_2d[row][2] = vec.z;
  }
  inline void ResetBottomRow();

  //comparison op
  inline bool operator==(const Matrix3& rhs) const { 
    for (unsigned int i=0; i<9; ++i)
      if (m[i] != rhs.m[i])
        return false;
    return true;
  }
  inline bool operator!=(const Matrix3& rhs) const { return !(*this == rhs); }

  //basic member arithmetic op
  inline Matrix3 operator+(const Matrix3& rhs) const;
  inline Matrix3 operator-(const Matrix3& rhs) const;
  inline Matrix3 operator*(const Matrix3& rhs) const; //concatenation
  inline Matrix3 operator-() const;
  inline Vector3 operator*(const Vector3& rhs) const;
  inline Matrix3 operator+(float scalar) const;
  inline Matrix3 operator-(float scalar) const;
  inline Matrix3 operator*(float scalar) const;
  inline Matrix3 operator/(float scalar) const;

  inline Matrix3& operator+=(const Matrix3& rhs) { *this = *this + rhs; return *this; }
  inline Matrix3& operator-=(const Matrix3& rhs) { *this = *this - rhs; return *this; }
  inline Matrix3& operator*=(const Matrix3& rhs) { *this = *this * rhs; return *this; }
  inline Matrix3& operator+=(float scalar) { *this = *this + scalar; return *this; }
  inline Matrix3& operator-=(float scalar) { *this = *this - scalar; return *this; }
  inline Matrix3& operator*=(float scalar) { *this = *this * scalar; return *this; }
  inline Matrix3& operator/=(float scalar) { *this = *this / scalar; return *this; }
  
  //static arithmetic op
  static inline void Add(Matrix3& result, const Matrix3& lhs, const Matrix3& rhs) { result = lhs + rhs; }
  static inline void Subtract(Matrix3& result, const Matrix3& lhs, const Matrix3& rhs) { result = lhs - rhs; }
  static inline void Multiply(Matrix3& result, const Matrix3& lhs, const Matrix3& rhs) { result = lhs * rhs; }
  static inline void Multiply(Vector3& result, const Matrix3& lhs, const Vector3& vec) { result = lhs * vec; }
  static inline void Multiply(Matrix3& result, const Matrix3& lhs, float scalar) { result = lhs * scalar; }
  static inline void Divide(Matrix3& result, const Matrix3& lhs, float scalar) { result = lhs / scalar; }

  //static friend func
  friend inline Matrix3 operator+(float scalar, const Matrix3& rhs);
  friend inline Matrix3 operator-(float scalar, const Matrix3& rhs);
  friend inline Matrix3 operator*(float scalar, const Matrix3& rhs);
  friend inline Matrix3 operator/(float scalar, const Matrix3& rhs);

  //advanced member arithmetic op
  float Determinant() const;
  Matrix3& Inverse();
  Matrix3 InverseCopy() const;
  Matrix3& Transpose();
  Matrix3 TransposeCopy() const;
  Matrix3& Normalise(); //per-vector normalize
  Matrix3 NormaliseCopy() const;

  //geometric properties
  inline void SetTranslate(const Vector2& vec);
  inline Vector2 GetTranslate() const;
  inline void SetScale(const Vector2& vec); //todo: will not work with rotation
  inline Vector2 GetScale() const;

  //Reset->Scale->Rotate->Translate
  inline void MakeTransform(const Vector2& translation = Vector2(0, 0), const Vector2& scale = Vector2(1.0f, 1.0f), Degree rotationdeg = Degree(0)); 
  inline void MakeTransform(const Vector2& translation = Vector2(0, 0), const Vector2& scale = Vector2(1.0f, 1.0f), Radian rotationrad = Radian(0)); 

  inline friend std::ostream& operator<<(std::ostream& outStream, const Matrix3& matrix)
  {
    outStream << "(" << matrix.m[0] << ", " << matrix.m[1] << ", " << matrix.m[2] << ") \n";
    outStream << "(" << matrix.m[3] << ", " << matrix.m[4] << ", " << matrix.m[5] << ") \n";
    outStream << "(" << matrix.m[6] << ", " << matrix.m[7] << ", " << matrix.m[8] << ") \n";

    return outStream;
  }
};

#include "Matrix3.inl"

#endif
