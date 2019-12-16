
#include "Matrix4.h"

const Matrix4 Matrix4::ZERO    (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
const Matrix4 Matrix4::IDENTITY(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f);

//helper function to get determinant of 4x4 matrix
inline static float MINOR(const Matrix4& m, unsigned int r0, unsigned int r1, unsigned int r2, 
						              unsigned int c0, unsigned int c1, unsigned int c2)
{
    return	m.m_2d[r0][c0] * (m.m_2d[r1][c1] * m.m_2d[r2][c2] - m.m_2d[r2][c1] * m.m_2d[r1][c2]) -
			      m.m_2d[r0][c1] * (m.m_2d[r1][c0] * m.m_2d[r2][c2] - m.m_2d[r2][c0] * m.m_2d[r1][c2]) +
			      m.m_2d[r0][c2] * (m.m_2d[r1][c0] * m.m_2d[r2][c1] - m.m_2d[r2][c0] * m.m_2d[r1][c1]);
}

float Matrix4::Determinant() const
{
   return	m_2d[0][0] * MINOR(*this, 1, 2, 3, 1, 2, 3) -
			    m_2d[0][1] * MINOR(*this, 1, 2, 3, 0, 2, 3) +
			    m_2d[0][2] * MINOR(*this, 1, 2, 3, 0, 1, 3) -
			    m_2d[0][3] * MINOR(*this, 1, 2, 3, 0, 1, 2);
}

Matrix4& Matrix4::Inverse()
{
  float m00 = m_2d[0][0], m01 = m_2d[0][1], m02 = m_2d[0][2], m03 = m_2d[0][3];
  float m10 = m_2d[1][0], m11 = m_2d[1][1], m12 = m_2d[1][2], m13 = m_2d[1][3];
  float m20 = m_2d[2][0], m21 = m_2d[2][1], m22 = m_2d[2][2], m23 = m_2d[2][3];
  float m30 = m_2d[3][0], m31 = m_2d[3][1], m32 = m_2d[3][2], m33 = m_2d[3][3];

  float v0 = m20 * m31 - m21 * m30;
  float v1 = m20 * m32 - m22 * m30;
  float v2 = m20 * m33 - m23 * m30;
  float v3 = m21 * m32 - m22 * m31;
  float v4 = m21 * m33 - m23 * m31;
  float v5 = m22 * m33 - m23 * m32;

  float t00 = + (v5 * m11 - v4 * m12 + v3 * m13);
  float t10 = - (v5 * m10 - v2 * m12 + v1 * m13);
  float t20 = + (v4 * m10 - v2 * m11 + v0 * m13);
  float t30 = - (v3 * m10 - v1 * m11 + v0 * m12);

  float invDet = 1 / (t00 * m00 + t10 * m01 + t20 * m02 + t30 * m03);

  float d00 = t00 * invDet;
  float d10 = t10 * invDet;
  float d20 = t20 * invDet;
  float d30 = t30 * invDet;

  float d01 = - (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
  float d11 = + (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
  float d21 = - (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
  float d31 = + (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

  v0 = m10 * m31 - m11 * m30;
  v1 = m10 * m32 - m12 * m30;
  v2 = m10 * m33 - m13 * m30;
  v3 = m11 * m32 - m12 * m31;
  v4 = m11 * m33 - m13 * m31;
  v5 = m12 * m33 - m13 * m32;

  float d02 = + (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
  float d12 = - (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
  float d22 = + (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
  float d32 = - (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

  v0 = m21 * m10 - m20 * m11;
  v1 = m22 * m10 - m20 * m12;
  v2 = m23 * m10 - m20 * m13;
  v3 = m22 * m11 - m21 * m12;
  v4 = m23 * m11 - m21 * m13;
  v5 = m23 * m12 - m22 * m13;

  float d03 = - (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
  float d13 = + (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
  float d23 = - (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
  float d33 = + (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

  m[0] = d00; m[1] = d01; m[2] = d02; m[3] = d03;
  m[4] = d10; m[5] = d11; m[6] = d12; m[7] = d13;
  m[8] = d20; m[9] = d21; m[10] = d22; m[11] = d23;
  m[12] = d30; m[13] = d31; m[14] = d32; m[15] = d33;

  return *this;
}

Matrix4 Matrix4::InverseCopy() const
{
  Matrix4 mat(*this);
  mat.Inverse();
  return mat;
}

Matrix4& Matrix4::Transpose()
{
  float temp;
  temp = m_2d[0][1]; m_2d[0][1] = m_2d[1][0]; m_2d[1][0] = temp;
  temp = m_2d[0][2]; m_2d[0][2] = m_2d[2][0]; m_2d[2][0] = temp;
  temp = m_2d[1][2]; m_2d[1][2] = m_2d[2][1]; m_2d[2][1] = temp;
  temp = m_2d[0][3]; m_2d[0][3] = m_2d[3][0]; m_2d[3][0] = temp;
  temp = m_2d[1][3]; m_2d[1][3] = m_2d[3][1]; m_2d[3][1] = temp;
  temp = m_2d[2][3]; m_2d[2][3] = m_2d[3][2]; m_2d[3][2] = temp;

  return *this;
}

Matrix4 Matrix4::TransposeCopy() const
{
  Matrix4 mat(*this);
  mat.Transpose();
  return mat;
}

Matrix4& Matrix4::Normalise()
{
  Vector4 r0 = GetColumn(0);
  Vector4 r1 = GetColumn(1);
  Vector4 r2 = GetColumn(2);
  Vector4 r3 = GetColumn(3);
  r0.Normalise();
  r1.Normalise();
  r2.Normalise();
  *this = Matrix4(r0, r1, r2, r3);
  return *this;
}

Matrix4 Matrix4::NormaliseCopy() const
{
  Matrix4 mat(*this);
  mat.Normalise();
  return mat;
}
