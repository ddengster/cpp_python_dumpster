
Quaternion Quaternion::operator+(const Quaternion& rhs) const
{
  Quaternion quat;
  quat.w += w + rhs.w;
  quat.x += x + rhs.x;
  quat.y += y + rhs.y;
  quat.z += z + rhs.z;
  return quat;
}

Quaternion Quaternion::operator-(const Quaternion& rhs) const
{
  Quaternion quat;
  quat.w = w - rhs.w;
  quat.x = x - rhs.x;
  quat.y = y - rhs.y;
  quat.z = z - rhs.z;
  return quat;
}

Quaternion Quaternion::operator*(const Quaternion& rhs) const
{
  Quaternion quat;
  quat.w = w * rhs.w - x * rhs.x - y * rhs.y - z * rhs.z;
  quat.x = w * rhs.x + x * rhs.w + y * rhs.z - z * rhs.y;
  quat.y = w * rhs.y + y * rhs.w + z * rhs.x - x * rhs.z;
  quat.z = w * rhs.z + z * rhs.w + x * rhs.y - y * rhs.x;
  return quat;
}

Quaternion Quaternion::operator*(float scalar) const
{
  Quaternion quat;
  quat.w = w * scalar;
  quat.x = x * scalar;
  quat.y = y * scalar;
  quat.z = z * scalar;
  return quat;
}

Vector3 Quaternion::operator*(const Vector3& rhs) const
{
  // nVidia SDK implementation
	Vector3 uv, uuv;
	Vector3 qvec(x, y, z);
	uv = qvec.CrossProduct(rhs);
	uuv = qvec.CrossProduct(uv);
	uv *= (2.0f * w);
	uuv *= 2.0f;

	Vector3 res = rhs + uv + uuv;
  /*
  res.x = (GenericMath::FAbs(res.x) < ZERO_ERROR_RANGE) ? 0.0f : res.x;
  res.y = (GenericMath::FAbs(res.y) < ZERO_ERROR_RANGE) ? 0.0f : res.y;
  res.z = (GenericMath::FAbs(res.z) < ZERO_ERROR_RANGE) ? 0.0f : res.z;
  */
  return res;
}

Quaternion Quaternion::operator-() const
{
  Quaternion quat;
  quat.w = -w;
  quat.x = -x;
  quat.y = -y;
  quat.z = -z;
  return quat;
}

Quaternion& Quaternion::operator+=(const Quaternion& rhs)
{
  *this = *this + rhs;
  return *this;
}

Quaternion& Quaternion::operator-=(const Quaternion& rhs)
{
  *this = *this - rhs;
  return *this;
}

Quaternion& Quaternion::operator*=(const Quaternion& rhs)
{
  *this = *this * rhs;
  return *this;
}

Quaternion& Quaternion::operator*=(float scalar)
{
  *this = *this * scalar;
  return *this;
}

void Quaternion::Add(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs)
{
  result.w += lhs.w + rhs.w;
	result.x += lhs.x + rhs.x;
	result.y += lhs.y + rhs.y;
	result.z += lhs.z + rhs.z;
}

void Quaternion::Subtract(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs)
{
  result.w -= lhs.w + rhs.w;
	result.x -= lhs.x + rhs.x;
	result.y -= lhs.y + rhs.y;
	result.z -= lhs.z + rhs.z;
}

void Quaternion::Multiply(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs)
{
  result = lhs * rhs;
}

void Quaternion::Multiply(Quaternion& result, const Quaternion& lhs, float scalar)
{
  result = lhs * scalar;
}

Quaternion operator* (float scalar, const Quaternion& rkQ)
{
  Quaternion quat;
  quat = rkQ * scalar;
  return quat;
}

void Quaternion::SetRotationToAxis(const Radian& anglerad, const Vector3& axisvector)
{
  Radian fHalfAngle ( 0.5f * anglerad.GetValue() );
  float fSin = GenericMath::Sin(fHalfAngle.GetValue());
  w = GenericMath::Cos(fHalfAngle.GetValue());
  x = fSin * axisvector.x;
  y = fSin * axisvector.y;
  z = fSin * axisvector.z;
}

void Quaternion::SetRotationToAxis(const Degree& angledeg, const Vector3& axisvector)
{
  SetRotationToAxis(angledeg.ConvertToRadian(), axisvector);
}

void Quaternion::GetRoll(float& degval)
{
  degval = Radian(GenericMath::ArcTan2(2*(x*y + w*z), w*w + x*x - y*y - z*z)).ConvertToDegree().GetValue();
}

void Quaternion::GetPitch(float& degval)
{
  degval = Radian(GenericMath::ArcTan2(2*(y*z + w*x), w*w - x*x - y*y + z*z)).ConvertToDegree().GetValue();
}

void Quaternion::GetYaw(float& degval)
{
  degval = Radian(GenericMath::ArcSin(-2*(x*z - w*y))).ConvertToDegree().GetValue();
}
