
#ifndef _MATRIX4_H
#define _MATRIX4_H

#include "GenericMath.h"
#include "Matrix3.h"
#include "Vector4.h"
#include "Quaternion.h"

/** Column major 4x4 matrix. ie. XYZW Translation to the rightmost column **/
/** Top left element has row index 0, column index 0 and bottom right has row index 3, column index 3 **/
class Matrix4
{
public:
  union
  {
    float m[16];
    float m_2d[4][4]; // [row] [column]
  };

  static const Matrix4 ZERO;
  static const Matrix4 IDENTITY;

public:
  //Constructors
  inline Matrix4() { }
  inline Matrix4(const Matrix4& rhs)             { memcpy(m, rhs.m, 16 * sizeof(float)); }
  inline explicit Matrix4(const float arr[4][4]) { memcpy(m, arr, 16 * sizeof(float));   }
  Matrix4(float fEntry00, float fEntry01, float fEntry02, float fEntry03,
				  float fEntry10, float fEntry11, float fEntry12, float fEntry13,
				  float fEntry20, float fEntry21, float fEntry22, float fEntry23,
          float fEntry30, float fEntry31, float fEntry32, float fEntry33) // no forced inline
  { 
    m_2d[0][0] = fEntry00; m_2d[0][1] = fEntry01; m_2d[0][2] = fEntry02; m_2d[0][3] = fEntry03;
    m_2d[1][0] = fEntry10; m_2d[1][1] = fEntry11; m_2d[1][2] = fEntry12; m_2d[1][3] = fEntry13;
    m_2d[2][0] = fEntry20; m_2d[2][1] = fEntry21; m_2d[2][2] = fEntry22; m_2d[2][3] = fEntry23;
    m_2d[3][0] = fEntry30; m_2d[3][1] = fEntry31; m_2d[3][2] = fEntry32; m_2d[3][3] = fEntry33;
  }
  inline Matrix4(const Vector4& row0, const Vector4& row1, const Vector4& row2, const Vector4& row3) 
  { 
    m_2d[0][0] = row0.x; m_2d[0][1] = row0.y; m_2d[0][2] = row0.z; m_2d[0][3] = row0.w;
    m_2d[1][0] = row1.x; m_2d[1][1] = row1.y; m_2d[1][2] = row1.z; m_2d[1][3] = row1.w;
    m_2d[2][0] = row2.x; m_2d[2][1] = row2.y; m_2d[2][2] = row2.z; m_2d[2][3] = row2.w;
    m_2d[3][0] = row3.x; m_2d[3][1] = row3.y; m_2d[3][2] = row3.z; m_2d[3][3] = row3.w;
  }
  inline Matrix4(const Matrix3& rhs)
  {
    m[0] = rhs.m[0]; m[1] = rhs.m[1]; m[2] = rhs.m[2];
    m[4] = rhs.m[3]; m[5] = rhs.m[4]; m[6] = rhs.m[5];
    m[8] = rhs.m[6]; m[9] = rhs.m[7]; m[10] = rhs.m[8]; 
    m[3] = m[7] = m[11] = m[12] = m[13] = m[14] = 0; 
    m[15] = 1;
  }
  inline Matrix4(const Quaternion& rot)
	{
		Matrix3 m3x3(rot.GetRotationMatrix3());
		operator=(m3x3);
	}

  //access op
  inline float* operator[](unsigned int row) const { assert(row < 4); return (float*)m_2d[row]; }
  inline const float* ptr() const { return m; }
  inline float* ptr()             { return m; }

  //assignment op
  inline Matrix4& operator=(const Matrix4& rhs) { memcpy(m, rhs.m, 16 * sizeof(float)); return *this; }
  inline Matrix4& operator=(const Matrix3& rhs) {
    m[0] = rhs.m[0]; m[1] = rhs.m[1]; m[2] = rhs.m[2];
    m[4] = rhs.m[3]; m[5] = rhs.m[4]; m[6] = rhs.m[5];
    m[8] = rhs.m[6]; m[9] = rhs.m[7]; m[10] = rhs.m[8]; 
    m[3] = m[7] = m[11] = m[12] = m[13] = m[14] = 0; 
    m[15] = 1;
    return *this;
  }

  //mutators and accessors
  inline Vector4 GetColumn(unsigned int column) const {
    assert(column < 4); 
    return Vector4(m_2d[0][column], m_2d[1][column], m_2d[2][column], m_2d[3][column]); 
  }
  inline void SetColumn(const Vector4& vec, unsigned int column) {
    assert(column < 4); 
    m_2d[0][column] = vec.x; m_2d[1][column] = vec.y; m_2d[2][column] = vec.z; m_2d[3][column] = vec.w; 
  }
  inline Vector4 GetRow(unsigned int row) const { 
    assert(row < 4); 
    return Vector4(m_2d[row][0], m_2d[row][1], m_2d[row][2], m_2d[row][3]);
  }
  inline void SetRow(const Vector4& vec, unsigned int row) {
    assert(row < 4); 
    m_2d[row][0] = vec.x; m_2d[row][1] = vec.y; m_2d[row][2] = vec.z; m_2d[row][3] = vec.w;
  }
  inline void ResetBottomRow();
  inline Matrix3 Extract3x3Matrix();
  inline Quaternion ExtractQuaternion();

  //comparison op
  inline bool operator==(const Matrix4& rhs) const { 
    for (unsigned int i=0; i<16; ++i)
      if (m[i] != rhs.m[i])
        return false;
    return true;
    //todo: profile memcmp?
    //memcmp(
  }
  inline bool operator!=(const Matrix4& rhs) const { return !(*this == rhs); }
  
  //basic member arithmetic op
  inline Matrix4 operator+(const Matrix4& rhs) const;
  inline Matrix4 operator-(const Matrix4& rhs) const;
  inline Matrix4 operator*(const Matrix4& rhs) const; //concatenation
  inline Matrix4 operator-() const;
  inline Vector4 operator*(const Vector4& rhs) const;
  inline Matrix4 operator+(float scalar) const;
  inline Matrix4 operator-(float scalar) const;
  inline Matrix4 operator*(float scalar) const;
  inline Matrix4 operator/(float scalar) const;

  inline Matrix4& operator+=(const Matrix4& rhs) { *this = *this + rhs; return *this; }
  inline Matrix4& operator-=(const Matrix4& rhs) { *this = *this - rhs; return *this; }
  inline Matrix4& operator*=(const Matrix4& rhs) { *this = *this * rhs; return *this; }
  inline Matrix4& operator+=(float scalar) { *this = *this + scalar; return *this; }
  inline Matrix4& operator-=(float scalar) { *this = *this - scalar; return *this; }
  inline Matrix4& operator*=(float scalar) { *this = *this * scalar; return *this; }
  inline Matrix4& operator/=(float scalar) { *this = *this / scalar; return *this; }
  
  //static arithmetic op
  static inline void Add(Matrix4& result, const Matrix4& lhs, const Matrix4& rhs) { result = lhs + rhs; }
  static inline void Subtract(Matrix4& result, const Matrix4& lhs, const Matrix4& rhs) { result = lhs - rhs; }
  static inline void Multiply(Matrix4& result, const Matrix4& lhs, const Matrix4& rhs) { result = lhs * rhs; }
  static inline void Multiply(Vector4& result, const Matrix4& lhs, const Vector4& vec) { result = lhs * vec; }
  static inline void Multiply(Matrix4& result, const Matrix4& lhs, float scalar) { result = lhs * scalar; }
  static inline void Divide(Matrix4& result, const Matrix4& lhs, float scalar) { result = lhs / scalar; }

  //static friend func
  friend inline Matrix4 operator+(float scalar, const Matrix4& rhs);
  friend inline Matrix4 operator-(float scalar, const Matrix4& rhs);
  friend inline Matrix4 operator*(float scalar, const Matrix4& rhs);
  friend inline Matrix4 operator/(float scalar, const Matrix4& rhs);

  //advanced member arithmetic op
  float Determinant() const;
  Matrix4& Inverse();
  Matrix4 InverseCopy() const;
  Matrix4& Transpose();
  Matrix4 TransposeCopy() const;
  Matrix4& Normalise(); //per-vector normalize
  Matrix4 NormaliseCopy() const;

  //geometric properties
  inline void Identity();
  inline void SetTranslate(const Vector3& vec);
  inline Vector3 GetTranslate() const;
  inline void MakeTranslateMatrix(const Vector3& vec); //Reset->SetTranslate
  inline void SetScale(const Vector3& vec); //todo: will not work with rotation
  inline Vector3 GetScale() const;
  inline void MakeScaleMatrix(const Vector3& vec); //Reset->SetScale
  
  //Reset->Scale->Rotate->Translate
  inline void MakeTransform(const Vector3& translation = Vector3(0, 0, 0), const Vector3& scale = Vector3(1.0f, 1.0f, 1.0f), const Quaternion& orientation = Quaternion()); 

  inline friend std::ostream& operator<<(std::ostream& outStream, const Matrix4& matrix)
  {
    outStream << "(" << matrix.m[0] << ", " << matrix.m[1] << ", " << matrix.m[2] << ", " << matrix.m[3] << ") \n";
    outStream << "(" << matrix.m[4] << ", " << matrix.m[5] << ", " << matrix.m[6] << ", " << matrix.m[7] << ") \n";
    outStream << "(" << matrix.m[8] << ", " << matrix.m[9] << ", " << matrix.m[10] << ", " << matrix.m[11] << ") \n";
    outStream << "(" << matrix.m[12] << ", " << matrix.m[13] << ", " << matrix.m[14] << ", " << matrix.m[15] << ") \n";

    return outStream;
  }
};

#include "Matrix4.inl"

#endif
