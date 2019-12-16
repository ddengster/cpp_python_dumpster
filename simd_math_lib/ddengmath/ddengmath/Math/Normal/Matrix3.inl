
inline Matrix3 Matrix3::operator+(const Matrix3& rhs) const
{
  Matrix3 res;
  res.m[0] = m[0] + rhs.m[0];  res.m[1] = m[1] + rhs.m[1];  res.m[2] = m[2] + rhs.m[2];
  res.m[3] = m[3] + rhs.m[3];  res.m[4] = m[4] + rhs.m[4];  res.m[5] = m[5] + rhs.m[5];
  res.m[6] = m[6] + rhs.m[6];  res.m[7] = m[7] + rhs.m[7];  res.m[8] = m[8] + rhs.m[8];
  return res;
}

inline Matrix3 Matrix3::operator-(const Matrix3& rhs) const
{
  Matrix3 res;
  res.m[0] = m[0] - rhs.m[0];  res.m[1] = m[1] - rhs.m[1];  res.m[2] = m[2] - rhs.m[2];
  res.m[3] = m[3] - rhs.m[3];  res.m[4] = m[4] - rhs.m[4];  res.m[5] = m[5] - rhs.m[5];
  res.m[6] = m[6] - rhs.m[6];  res.m[7] = m[7] - rhs.m[7];  res.m[8] = m[8] - rhs.m[8];
  return res;
}

inline Matrix3 Matrix3::operator*(const Matrix3& rhs) const
{
  Matrix3 res;
  res.m[0] = m_2d[0][0] * rhs.m_2d[0][0] + m_2d[0][1] * rhs.m_2d[1][0] + m_2d[0][2] * rhs.m_2d[2][0];
  res.m[1] = m_2d[0][0] * rhs.m_2d[0][1] + m_2d[0][1] * rhs.m_2d[1][1] + m_2d[0][2] * rhs.m_2d[2][1];
  res.m[2] = m_2d[0][0] * rhs.m_2d[0][2] + m_2d[0][1] * rhs.m_2d[1][2] + m_2d[0][2] * rhs.m_2d[2][2];

  res.m[3] = m_2d[1][0] * rhs.m_2d[0][0] + m_2d[1][1] * rhs.m_2d[1][0] + m_2d[1][2] * rhs.m_2d[2][0];
  res.m[4] = m_2d[1][0] * rhs.m_2d[0][1] + m_2d[1][1] * rhs.m_2d[1][1] + m_2d[1][2] * rhs.m_2d[2][1];
  res.m[5] = m_2d[1][0] * rhs.m_2d[0][2] + m_2d[1][1] * rhs.m_2d[1][2] + m_2d[1][2] * rhs.m_2d[2][2];

  res.m[6] = m_2d[2][0] * rhs.m_2d[0][0] + m_2d[2][1] * rhs.m_2d[1][0] + m_2d[2][2] * rhs.m_2d[2][0];
  res.m[7] = m_2d[2][0] * rhs.m_2d[0][1] + m_2d[2][1] * rhs.m_2d[1][1] + m_2d[2][2] * rhs.m_2d[2][1];
  res.m[8] = m_2d[2][0] * rhs.m_2d[0][2] + m_2d[2][1] * rhs.m_2d[1][2] + m_2d[2][2] * rhs.m_2d[2][2];
  return res;
}

inline Matrix3 Matrix3::operator-() const
{
  Matrix3 res(*this);
  res.m[0] = -res.m[0]; res.m[1] = -res.m[1]; res.m[2] = -res.m[2];
  res.m[3] = -res.m[3]; res.m[4] = -res.m[4]; res.m[5] = -res.m[5];
  res.m[6] = -res.m[6]; res.m[7] = -res.m[7]; res.m[8] = -res.m[8];
  return res;
}

inline Vector3 Matrix3::operator*(const Vector3& rhs) const
{
  return Vector3(
    m_2d[0][0] * rhs[0] + m_2d[0][1] * rhs[1] + m_2d[0][2] * rhs[2],
    m_2d[1][0] * rhs[0] + m_2d[1][1] * rhs[1] + m_2d[1][2] * rhs[2],
    m_2d[2][0] * rhs[0] + m_2d[2][1] * rhs[1] + m_2d[2][2] * rhs[2]);
}

inline Matrix3 Matrix3::operator+(float scalar) const
{
  Matrix3 res;
  res.m[0] = m[0] + scalar;  res.m[1] = m[1] + scalar;  res.m[2] = m[2] + scalar;
  res.m[3] = m[3] + scalar;  res.m[4] = m[4] + scalar;  res.m[5] = m[5] + scalar;
  res.m[6] = m[6] + scalar;  res.m[7] = m[7] + scalar;  res.m[8] = m[8] + scalar;
  return res;
}

inline Matrix3 Matrix3::operator-(float scalar) const
{
  Matrix3 res;
  res.m[0] = m[0] - scalar;  res.m[1] = m[1] - scalar;  res.m[2] = m[2] - scalar;
  res.m[3] = m[3] - scalar;  res.m[4] = m[4] - scalar;  res.m[5] = m[5] - scalar;
  res.m[6] = m[6] - scalar;  res.m[7] = m[7] - scalar;  res.m[8] = m[8] - scalar;
  return res;
}

inline Matrix3 Matrix3::operator*(float scalar) const
{
  Matrix3 res;
  res.m[0] = m[0] * scalar;  res.m[1] = m[1] * scalar;  res.m[2] = m[2] * scalar;
  res.m[3] = m[3] * scalar;  res.m[4] = m[4] * scalar;  res.m[5] = m[5] * scalar;
  res.m[6] = m[6] * scalar;  res.m[7] = m[7] * scalar;  res.m[8] = m[8] * scalar;
  return res;
}

inline Matrix3 Matrix3::operator/(float scalar) const
{
  Matrix3 res;
  float fInv = 1.0f / scalar;
  res.m[0] = m[0] * fInv;  res.m[1] = m[1] * fInv;  res.m[2] = m[2] * fInv;
  res.m[3] = m[3] * fInv;  res.m[4] = m[4] * fInv;  res.m[5] = m[5] * fInv;
  res.m[6] = m[6] * fInv;  res.m[7] = m[7] * fInv;  res.m[8] = m[8] * fInv;
  return res;
}

Matrix3 operator+(float scalar, const Matrix3& rhs)
{
  return (rhs + scalar);
}

Matrix3 operator-(float scalar, const Matrix3& rhs)
{
  Matrix3 res;
  res.m[0] = scalar - rhs.m[0];  res.m[1] = scalar - rhs.m[1];  res.m[2] = scalar - rhs.m[2];
  res.m[3] = scalar - rhs.m[3];  res.m[4] = scalar - rhs.m[4];  res.m[5] = scalar - rhs.m[5];
  res.m[6] = scalar - rhs.m[6];  res.m[7] = scalar - rhs.m[7];  res.m[8] = scalar - rhs.m[8];
  return res;
}

Matrix3 operator*(float scalar, const Matrix3& rhs)
{
  return (rhs * scalar);
}

Matrix3 operator/(float scalar, const Matrix3& rhs)
{
  Matrix3 res;
  res.m[0] = scalar / rhs.m[0];  res.m[1] = scalar / rhs.m[1];  res.m[2] = scalar / rhs.m[2];
  res.m[3] = scalar / rhs.m[3];  res.m[4] = scalar / rhs.m[4];  res.m[5] = scalar / rhs.m[5];
  res.m[6] = scalar / rhs.m[6];  res.m[7] = scalar / rhs.m[7];  res.m[8] = scalar / rhs.m[8];
  return res;
}

void Matrix3::ResetBottomRow()
{
  m_2d[2][0] = m_2d[2][1] = 0; 
  m_2d[2][2] = 1;
}

void Matrix3::SetTranslate(const Vector2& vec)
{
  m_2d[0][2] = vec.x; m_2d[1][2] = vec.y; 
}

Vector2 Matrix3::GetTranslate() const
{
  return Vector2(m_2d[0][2], m_2d[1][2]);
}

void Matrix3::SetScale(const Vector2& vec)
{
  m_2d[0][0] = vec.x; m_2d[1][1] = vec.y;
}

Vector2 Matrix3::GetScale() const
{
  return Vector2(m_2d[0][0], m_2d[1][1]);
}

void Matrix3::MakeTransform(const Vector2& translation, const Vector2& scale, Degree rotationdeg)
{
  MakeTransform(translation, scale, rotationdeg.ConvertToRadian());
}

void Matrix3::MakeTransform(const Vector2& translation, const Vector2& scale, Radian rotationrad)
{
  //memset(m, 0, 9 * sizeof(float));

  float cosineval = GenericMath::Cos(rotationrad.GetValue());
  //cosineval = (GenericMath::FAbs(cosineval) < ZERO_ERROR_RANGE) ? 0.0f : cosineval;
  float sineval = GenericMath::Sin(rotationrad.GetValue());
  //sineval = (GenericMath::FAbs(sineval) < ZERO_ERROR_RANGE) ? 0.0f : sineval;
  m_2d[0][0] = scale.x * cosineval;
  m_2d[0][1] = scale.x * -sineval;
  m_2d[1][0] = scale.y * sineval;
  m_2d[1][1] = scale.y * cosineval;

  m_2d[0][2] = translation.x;
  m_2d[1][2] = translation.y;
  m_2d[2][2] = 1.0f;

  m_2d[1][0] = m_2d[2][0] = 0.0f;
}
