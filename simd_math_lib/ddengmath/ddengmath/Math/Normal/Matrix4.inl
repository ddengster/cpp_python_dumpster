

inline Matrix4 Matrix4::operator+(const Matrix4& rhs) const
{
  Matrix4 res;
  res.m[0] = m[0] + rhs.m[0];  res.m[1] = m[1] + rhs.m[1];  res.m[2] = m[2] + rhs.m[2]; res.m[3] = m[3] + rhs.m[3];
  res.m[4] = m[4] + rhs.m[4];  res.m[5] = m[5] + rhs.m[5];  res.m[6] = m[6] + rhs.m[6]; res.m[7] = m[7] + rhs.m[7];
  res.m[8] = m[8] + rhs.m[8];  res.m[9] = m[9] + rhs.m[9];  res.m[10] = m[10] + rhs.m[10]; res.m[11] = m[11] + rhs.m[11];
  res.m[12] = m[12] + rhs.m[12];  res.m[13] = m[13] + rhs.m[13];  res.m[14] = m[14] + rhs.m[14]; res.m[15] = m[15] + rhs.m[15];

  return res;
}

inline Matrix4 Matrix4::operator-(const Matrix4& rhs) const
{
  Matrix4 res;
  res.m[0] = m[0] - rhs.m[0];  res.m[1] = m[1] - rhs.m[1];  res.m[2] = m[2] - rhs.m[2]; res.m[3] = m[3] - rhs.m[3];
  res.m[4] = m[4] - rhs.m[4];  res.m[5] = m[5] - rhs.m[5];  res.m[6] = m[6] - rhs.m[6]; res.m[7] = m[7] - rhs.m[7];
  res.m[8] = m[8] - rhs.m[8];  res.m[9] = m[9] - rhs.m[9];  res.m[10] = m[10] - rhs.m[10]; res.m[11] = m[11] - rhs.m[11];
  res.m[12] = m[12] - rhs.m[12];  res.m[13] = m[13] - rhs.m[13];  res.m[14] = m[14] - rhs.m[14]; res.m[15] = m[15] - rhs.m[15];
  return res;
}

inline Matrix4 Matrix4::operator*(const Matrix4& rhs) const
{
  Matrix4 res;
  res.m[0] = m_2d[0][0] * rhs.m_2d[0][0] + m_2d[0][1] * rhs.m_2d[1][0] + m_2d[0][2] * rhs.m_2d[2][0] + m_2d[0][3] * rhs.m_2d[3][0];
  res.m[1] = m_2d[0][0] * rhs.m_2d[0][1] + m_2d[0][1] * rhs.m_2d[1][1] + m_2d[0][2] * rhs.m_2d[2][1] + m_2d[0][3] * rhs.m_2d[3][1];
  res.m[2] = m_2d[0][0] * rhs.m_2d[0][2] + m_2d[0][1] * rhs.m_2d[1][2] + m_2d[0][2] * rhs.m_2d[2][2] + m_2d[0][3] * rhs.m_2d[3][2];
  res.m[3] = m_2d[0][0] * rhs.m_2d[0][3] + m_2d[0][1] * rhs.m_2d[1][3] + m_2d[0][2] * rhs.m_2d[2][3] + m_2d[0][3] * rhs.m_2d[3][3];

  res.m[4] = m_2d[1][0] * rhs.m_2d[0][0] + m_2d[1][1] * rhs.m_2d[1][0] + m_2d[1][2] * rhs.m_2d[2][0] + m_2d[1][3] * rhs.m_2d[3][0];
  res.m[5] = m_2d[1][0] * rhs.m_2d[0][1] + m_2d[1][1] * rhs.m_2d[1][1] + m_2d[1][2] * rhs.m_2d[2][1] + m_2d[1][3] * rhs.m_2d[3][1];
  res.m[6] = m_2d[1][0] * rhs.m_2d[0][2] + m_2d[1][1] * rhs.m_2d[1][2] + m_2d[1][2] * rhs.m_2d[2][2] + m_2d[1][3] * rhs.m_2d[3][2];
  res.m[7] = m_2d[1][0] * rhs.m_2d[0][3] + m_2d[1][1] * rhs.m_2d[1][3] + m_2d[1][2] * rhs.m_2d[2][3] + m_2d[1][3] * rhs.m_2d[3][3];

  res.m[8] = m_2d[2][0]  * rhs.m_2d[0][0] + m_2d[2][1] * rhs.m_2d[1][0] + m_2d[2][2] * rhs.m_2d[2][0] + m_2d[2][3] * rhs.m_2d[3][0];
  res.m[9] = m_2d[2][0]  * rhs.m_2d[0][1] + m_2d[2][1] * rhs.m_2d[1][1] + m_2d[2][2] * rhs.m_2d[2][1] + m_2d[2][3] * rhs.m_2d[3][1];
  res.m[10] = m_2d[2][0] * rhs.m_2d[0][2] + m_2d[2][1] * rhs.m_2d[1][2] + m_2d[2][2] * rhs.m_2d[2][2] + m_2d[2][3] * rhs.m_2d[3][2];
  res.m[11] = m_2d[2][0] * rhs.m_2d[0][3] + m_2d[2][1] * rhs.m_2d[1][3] + m_2d[2][2] * rhs.m_2d[2][3] + m_2d[2][3] * rhs.m_2d[3][3];

  res.m[12] = m_2d[3][0]  * rhs.m_2d[0][0] + m_2d[3][1] * rhs.m_2d[1][0] + m_2d[3][2] * rhs.m_2d[2][0] + m_2d[3][3] * rhs.m_2d[3][0];
  res.m[13] = m_2d[3][0]  * rhs.m_2d[0][1] + m_2d[3][1] * rhs.m_2d[1][1] + m_2d[3][2] * rhs.m_2d[2][1] + m_2d[3][3] * rhs.m_2d[3][1];
  res.m[14] = m_2d[3][0]  * rhs.m_2d[0][2] + m_2d[3][1] * rhs.m_2d[1][2] + m_2d[3][2] * rhs.m_2d[2][2] + m_2d[3][3] * rhs.m_2d[3][2];
  res.m[15] = m_2d[3][0]  * rhs.m_2d[0][3] + m_2d[3][1] * rhs.m_2d[1][3] + m_2d[3][2] * rhs.m_2d[2][3] + m_2d[3][3] * rhs.m_2d[3][3];
  return res;
}

inline Matrix4 Matrix4::operator-() const
{
  Matrix4 res(*this);
  res.m[0] = -res.m[0]; res.m[1] = -res.m[1]; res.m[2] = -res.m[2]; res.m[3] = -res.m[3];
  res.m[4] = -res.m[4]; res.m[5] = -res.m[5]; res.m[6] = -res.m[6]; res.m[7] = -res.m[7];
  res.m[8] = -res.m[8]; res.m[9] = -res.m[9]; res.m[10] = -res.m[10]; res.m[11] = -res.m[11];
  res.m[12] = -res.m[12]; res.m[13] = -res.m[13]; res.m[14] = -res.m[14]; res.m[15] = -res.m[15];
  return res;
}

inline Vector4 Matrix4::operator*(const Vector4& rhs) const
{
  return Vector4(
    m_2d[0][0] * rhs.x + m_2d[0][1] * rhs.y + m_2d[0][2] * rhs.z + m_2d[0][3] * rhs.w,
    m_2d[1][0] * rhs.x + m_2d[1][1] * rhs.y + m_2d[1][2] * rhs.z + m_2d[1][3] * rhs.w,
    m_2d[2][0] * rhs.x + m_2d[2][1] * rhs.y + m_2d[2][2] * rhs.z + m_2d[2][3] * rhs.w,
    m_2d[3][0] * rhs.x + m_2d[3][1] * rhs.y + m_2d[3][2] * rhs.z + m_2d[3][3] * rhs.w);
}

inline Matrix4 Matrix4::operator+(float scalar) const
{
  Matrix4 res;
  res.m[0] = m[0] + scalar;   res.m[1] = m[1] + scalar;   res.m[2] = m[2] + scalar;   res.m[3] = m[3] + scalar;
  res.m[4] = m[4] + scalar;   res.m[5] = m[5] + scalar;   res.m[6] = m[6] + scalar;   res.m[7] = m[7] + scalar;
  res.m[8] = m[8] + scalar;   res.m[9] = m[9] + scalar;   res.m[10] = m[10] + scalar; res.m[11] = m[11] + scalar;
  res.m[12] = m[12] + scalar; res.m[13] = m[13] + scalar; res.m[14] = m[14] + scalar; res.m[15] = m[15] + scalar;
  return res;
}

inline Matrix4 Matrix4::operator-(float scalar) const
{
  Matrix4 res;
  res.m[0] = m[0] - scalar;   res.m[1] = m[1] - scalar;   res.m[2] = m[2] - scalar;   res.m[3] = m[3] - scalar;
  res.m[4] = m[4] - scalar;   res.m[5] = m[5] - scalar;   res.m[6] = m[6] - scalar;   res.m[7] = m[7] - scalar;
  res.m[8] = m[8] - scalar;   res.m[9] = m[9] - scalar;   res.m[10] = m[10] - scalar; res.m[11] = m[11] - scalar;
  res.m[12] = m[12] - scalar; res.m[13] = m[13] - scalar; res.m[14] = m[14] - scalar; res.m[15] = m[15] - scalar;
  return res;
}

inline Matrix4 Matrix4::operator*(float scalar) const
{
  Matrix4 res;
  res.m[0] = m[0] * scalar;   res.m[1] = m[1] * scalar;   res.m[2] = m[2] * scalar;   res.m[3] = m[3] * scalar;
  res.m[4] = m[4] * scalar;   res.m[5] = m[5] * scalar;   res.m[6] = m[6] * scalar;   res.m[7] = m[7] * scalar;
  res.m[8] = m[8] * scalar;   res.m[9] = m[9] * scalar;   res.m[10] = m[10] * scalar; res.m[11] = m[11] * scalar;
  res.m[12] = m[12] * scalar; res.m[13] = m[13] * scalar; res.m[14] = m[14] * scalar; res.m[15] = m[15] * scalar;
  return res;
}

inline Matrix4 Matrix4::operator/(float scalar) const
{
  Matrix4 res;
  float fInv = 1.0f / scalar;
  res.m[0] = m[0] * fInv;   res.m[1] = m[1] * fInv;   res.m[2] = m[2] * fInv;   res.m[3] = m[3] * fInv;
  res.m[4] = m[4] * fInv;   res.m[5] = m[5] * fInv;   res.m[6] = m[6] * fInv;   res.m[7] = m[7] * fInv;
  res.m[8] = m[8] * fInv;   res.m[9] = m[9] * fInv;   res.m[10] = m[10] * fInv; res.m[11] = m[11] * fInv;
  res.m[12] = m[12] * fInv; res.m[13] = m[13] * fInv; res.m[14] = m[14] * fInv; res.m[15] = m[15] * fInv;
  return res;
}

Matrix4 operator+(float scalar, const Matrix4& rhs)
{
  return (rhs + scalar);
}

Matrix4 operator-(float scalar, const Matrix4& rhs)
{
  Matrix4 res;
  res.m[0] = scalar - rhs.m[0];   res.m[1] = scalar - rhs.m[1];   res.m[2] = scalar - rhs.m[2];   res.m[3] = scalar - rhs.m[3];  
  res.m[4] = scalar - rhs.m[4];   res.m[5] = scalar - rhs.m[5];   res.m[6] = scalar - rhs.m[6];   res.m[7] = scalar - rhs.m[7];
  res.m[8] = scalar - rhs.m[8];   res.m[9] = scalar - rhs.m[9];   res.m[10] = scalar - rhs.m[10]; res.m[11] = scalar - rhs.m[11];
  res.m[12] = scalar - rhs.m[12]; res.m[13] = scalar - rhs.m[13]; res.m[14] = scalar - rhs.m[14]; res.m[15] = scalar - rhs.m[15];
  return res;
}

Matrix4 operator*(float scalar, const Matrix4& rhs)
{
  return (rhs * scalar);
}

Matrix4 operator/(float scalar, const Matrix4& rhs)
{
  Matrix4 res;
  res.m[0] = scalar / rhs.m[0];   res.m[1] = scalar / rhs.m[1];   res.m[2] = scalar / rhs.m[2];   res.m[3] = scalar / rhs.m[3];  
  res.m[4] = scalar / rhs.m[4];   res.m[5] = scalar / rhs.m[5];   res.m[6] = scalar / rhs.m[6];   res.m[7] = scalar / rhs.m[7];
  res.m[8] = scalar / rhs.m[8];   res.m[9] = scalar / rhs.m[9];   res.m[10] = scalar / rhs.m[10]; res.m[11] = scalar / rhs.m[11];
  res.m[12] = scalar / rhs.m[12]; res.m[13] = scalar / rhs.m[13]; res.m[14] = scalar / rhs.m[14]; res.m[15] = scalar / rhs.m[15];
  return res;
}

void Matrix4::ResetBottomRow()
{
  m_2d[3][0] = m_2d[3][1] =m_2d[3][2] = 0;
  m_2d[3][3] = 1;
}

void Matrix4::Identity()
{
  memset(m, 0, 16 * sizeof(float));
  m_2d[0][0] = m_2d[1][1] = m_2d[2][2] = m_2d[3][3] = 1;
}

void Matrix4::SetTranslate(const Vector3& vec)
{
  m_2d[0][3] = vec.x; m_2d[1][3] = vec.y; m_2d[2][3] = vec.z; 
}

Vector3 Matrix4::GetTranslate() const
{
  return Vector3(m_2d[0][3], m_2d[1][3], m_2d[2][3]);
}

void Matrix4::MakeTranslateMatrix(const Vector3& vec) //Reset->SetTranslate
{
  Identity();
  SetTranslate(vec);
}

void Matrix4::SetScale(const Vector3& vec)
{
  m_2d[0][0] = vec.x; m_2d[1][1] = vec.y; m_2d[2][2] = vec.z;
}

Vector3 Matrix4::GetScale() const
{
  return Vector3(m_2d[0][0], m_2d[1][1], m_2d[2][2]);
}

void Matrix4::MakeScaleMatrix(const Vector3& vec)
{
  Identity();
  SetScale(vec);
}

void Matrix4::MakeTransform(const Vector3& translation, const Vector3& scale, const Quaternion& orientation)
{ 
  Matrix3 rot3(orientation.GetRotationMatrix3());
  m_2d[0][0] = scale.x * rot3.m_2d[0][0];  m_2d[0][1] = scale.x * rot3.m_2d[0][1]; m_2d[0][2] = scale.x * rot3.m_2d[0][2];
  m_2d[1][0] = scale.y * rot3.m_2d[1][0];  m_2d[1][1] = scale.y * rot3.m_2d[1][1]; m_2d[1][2] = scale.y * rot3.m_2d[1][2];
  m_2d[2][0] = scale.z * rot3.m_2d[2][0];  m_2d[2][1] = scale.z * rot3.m_2d[2][1]; m_2d[2][2] = scale.z * rot3.m_2d[2][2];

  // Set up final matrix with scale, rotation and translation
  SetTranslate(translation);

  // No projection term
  m_2d[3][0] = m_2d[3][1] = m_2d[3][2] = 0; m_2d[3][3] = 1;
}

Matrix3 Matrix4::Extract3x3Matrix()
{
  return Matrix3(m_2d[0][0], m_2d[0][1], m_2d[0][2],
                 m_2d[1][0], m_2d[1][1], m_2d[1][2],
                 m_2d[2][0], m_2d[2][1], m_2d[2][2]);
}

Quaternion Matrix4::ExtractQuaternion()
{
  Matrix3 mat(Extract3x3Matrix());
  return Quaternion(mat);
}
