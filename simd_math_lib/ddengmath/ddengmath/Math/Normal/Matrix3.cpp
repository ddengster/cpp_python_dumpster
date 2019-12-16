
#include "Matrix3.h"

const Matrix3 Matrix3::ZERO(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
const Matrix3 Matrix3::IDENTITY(1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f);

float Matrix3::Determinant() const
{
  float factor1 = m_2d[1][1] * m_2d[2][2] - m_2d[1][2] * m_2d[2][1];
  float factor2 = m_2d[1][2] * m_2d[2][0] - m_2d[1][0] * m_2d[2][2];
  float factor3 = m_2d[1][0] * m_2d[2][1] - m_2d[1][1] * m_2d[2][0];

  float det = m[0]*factor1 + m[1]*factor2 + m[2]*factor3;

  return det;
}

Matrix3& Matrix3::Inverse()
{
  float det = Determinant();
  assert (GenericMath::FAbs(det) > ZERO_ERROR_RANGE);

  //transpose already included
  m_2d[0][0] = m_2d[1][1]*m_2d[2][2] - m_2d[1][2]*m_2d[2][1];
  m_2d[0][1] = m_2d[0][2]*m_2d[2][1] - m_2d[0][1]*m_2d[2][2];
  m_2d[0][2] = m_2d[0][1]*m_2d[1][2] - m_2d[0][2]*m_2d[1][1];

  m_2d[1][0] = m_2d[1][2]*m_2d[2][0] - m_2d[1][0]*m_2d[2][2];
  m_2d[1][1] = m_2d[0][0]*m_2d[2][2] - m_2d[0][2]*m_2d[2][0];
  m_2d[1][2] = m_2d[0][2]*m_2d[1][0] - m_2d[0][0]*m_2d[1][2];

  m_2d[2][0] = m_2d[1][0]*m_2d[2][1] - m_2d[1][1]*m_2d[2][0];
  m_2d[2][1] = m_2d[0][1]*m_2d[2][0] - m_2d[0][0]*m_2d[2][1];
  m_2d[2][2] = m_2d[0][0]*m_2d[1][1] - m_2d[0][1]*m_2d[1][0];

  float fInv = 1.0f / det;
  *this *= fInv;
  return *this;
}

Matrix3 Matrix3::InverseCopy() const
{
  Matrix3 mat(*this);
  mat.Inverse();
  return mat;
}

Matrix3& Matrix3::Transpose()
{
  float temp;
  temp = m_2d[0][1]; m_2d[0][1] = m_2d[1][0]; m_2d[1][0] = temp;
  temp = m_2d[0][2]; m_2d[0][2] = m_2d[2][0]; m_2d[2][0] = temp;
  temp = m_2d[1][2]; m_2d[1][2] = m_2d[2][1]; m_2d[2][1] = temp;

  return *this;
}

Matrix3 Matrix3::TransposeCopy() const
{
  Matrix3 mat(*this);
  mat.Transpose();
  return mat;
}

Matrix3& Matrix3::Normalise()
{
  Vector3 r0 = GetColumn(0);
  Vector3 r1 = GetColumn(1);
  Vector3 r2 = GetColumn(2);
  r0.Normalise();
  r1.Normalise();
  r2.Normalise();
  *this = Matrix3(r0, r1, r2);
  return *this;
}

Matrix3 Matrix3::NormaliseCopy() const
{
  Matrix3 mat(*this);
  mat.Normalise();
  return mat;
}
