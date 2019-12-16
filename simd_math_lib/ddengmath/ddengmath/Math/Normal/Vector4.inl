

Vector4& Vector4::operator+=(const Vector4& rhs)
{
  x += rhs.x;
  y += rhs.y;
  z += rhs.z;
  w += rhs.w;
  return *this;
}

Vector4& Vector4::operator-=(const Vector4& rhs)
{
  x -= rhs.x;
  y -= rhs.y;
  z -= rhs.z;
  w -= rhs.w;
  return *this;
}

Vector4& Vector4::operator*=(const Vector4& rhs)
{
  x *= rhs.x;
  y *= rhs.y;
  z *= rhs.z;
  w *= rhs.w;
  return *this;
}

Vector4& Vector4::operator/=(const Vector4& rhs)
{
  x /= rhs.x;
  y /= rhs.y;
  z /= rhs.z;
  w /= rhs.w;
  return *this;
}

Vector4& Vector4::operator+=(float scalar)
{
  x += scalar;
  y += scalar;
  z += scalar;
  w += scalar;
  return *this;
}

Vector4& Vector4::operator-=(float scalar)
{
  x -= scalar;
  y -= scalar;
  z -= scalar;
  w -= scalar;
  return *this;
}

Vector4& Vector4::operator*=(float scalar)
{
  x *= scalar;
  y *= scalar;
  z *= scalar;
  w *= scalar;
  return *this;
}

Vector4& Vector4::operator/=(float scalar)
{
  assert( scalar != 0.0 );

	float fInv = 1.0f / scalar;

	x *= fInv;
	y *= fInv;
  z *= fInv;
  w *= fInv;

	return *this;
}

float Vector4::Length() const
{
  return GenericMath::Sqrt(x * x + y * y + z * z + w * w);
}

float Vector4::SquaredLength() const
{
  return x * x + y * y + z * z + w * w;
}

float Vector4::Distance(const Vector4& vec) const
{
	return (*this - vec).Length();
}

float Vector4::SquaredDistance(const Vector4& vec) const
{
  return (*this - vec).SquaredLength();
}

float Vector4::DotProduct(const Vector4& rhs) const
{
  return x * rhs.x + y * rhs.y + z * rhs.z + w * rhs.w;
}

Vector4 Vector4::MidPoint(const Vector4& vec) const
{
  return Vector4((x + vec.x) * 0.5f, (y + vec.y) * 0.5f, (z + vec.z) * 0.5f, (w + vec.w) * 0.5f);
}

void Vector4::Normalise()
{
  float fLength = GenericMath::Sqrt(x * x + y * y + z * z + w * w);

	// Will also work for zero-sized vectors
	if (fLength > ZERO_ERROR_RANGE)
	{
		float fInvLength = 1.0f / fLength;
		x *= fInvLength;
		y *= fInvLength;
    z *= fInvLength;
    w *= fInvLength;
	}
}

Vector4 Vector4::NormalisedCopy(void) const
{
  Vector4 ret = *this;
  ret.Normalise();
  return ret;
}

void Vector4::MakeFloor(const Vector4& cmp)
{
  if (cmp.x < x) x = cmp.x;
	if (cmp.y < y) y = cmp.y;
  if (cmp.z < z) z = cmp.z;
  if (cmp.w < w) z = cmp.w;
}

void Vector4::MakeCeil(const Vector4& cmp)
{
  if (cmp.x > x) x = cmp.x;
	if (cmp.y > y) y = cmp.y;
  if (cmp.z > z) z = cmp.z;
  if (cmp.w > w) z = cmp.w;
}

Vector4 Vector4::CrossProduct(const Vector4& rhs) const
{
  return Vector4( y * rhs.z - z * rhs.y,
					        z * rhs.x - x * rhs.z,
					        x * rhs.y - y * rhs.x,
                  0 );
}

bool Vector4::IsZeroLength(void) const
{
  float sqlen = (x * x) + (y * y) + (z * z) + (w * w);
  return (sqlen < (ZERO_ERROR_RANGE * ZERO_ERROR_RANGE));
}

Vector4 Vector4::Reflect(const Vector4& normal) const
{
  return Vector4(*this - (2 * this->DotProduct(normal) * normal));
}

bool Vector4::DirectionEquals(const Vector4& rhs,	const Radian& tolerance) const
{
  float dot = NormalisedCopy().DotProduct(rhs.NormalisedCopy());
	Radian angle(GenericMath::ArcCos(dot));

	return (GenericMath::FAbs(angle.GetValue()) <= tolerance.GetValue());
}

void Vector4::Clamp(const Vector4 &min, const Vector4 &max)
{
  x = x < min.x ? min.x : ( x > max.x ) ? max.x : x; 
  y = y < min.y ? min.y : ( y > max.y ) ? max.y : y; 
  z = z < min.z ? min.z : ( z > max.z ) ? max.z : z; 
  w = w < min.w ? min.w : ( w > max.w ) ? max.w : w; 
}

void Vector4::Clamp(float min, float max)
{
  x = x < min ? min : ( x > max ) ? max : x; 
  y = y < min ? min : ( y > max ) ? max : y; 
  z = z < min ? min : ( z > max ) ? max : z; 
  w = w < min ? min : ( w > max ) ? max : w; 
}

void Vector4::Add(Vector4& results, const Vector4& lhs, const Vector4& rhs)
{
  results.x = lhs.x + rhs.x;
	results.y = lhs.y + rhs.y;
  results.z = lhs.z + rhs.z;
  results.w = lhs.w + rhs.w;
}

void Vector4::Subtract(Vector4& results, const Vector4& lhs, const Vector4& rhs)
{
  results.x = lhs.x - rhs.x;
	results.y = lhs.y - rhs.y;
  results.z = lhs.z - rhs.z;
  results.w = lhs.w - rhs.w;
}

void Vector4::Multiply(Vector4& results, const Vector4& lhs, float scalar)
{
  results.x = lhs.x * scalar;
	results.y = lhs.y * scalar;
  results.z = lhs.z * scalar;
  results.w = lhs.w * scalar;
}

void Vector4::Multiply(Vector4& results, float scalar, const Vector4& rhs)
{
  results.x = rhs.x * scalar;
	results.y = rhs.y * scalar;
  results.z = rhs.z * scalar;
  results.w = rhs.w * scalar;
}

void Vector4::Multiply(Vector4& results, const Vector4& lhs, const Vector4& rhs)
{
  results.x = lhs.x * rhs.x;
	results.y = lhs.y * rhs.y;
  results.z = lhs.z * rhs.z;
  results.w = lhs.w * rhs.w;
}

void Vector4::Divide(Vector4& results, float scalar, const Vector4& rhs)
{
  results.x = scalar / rhs.x;
	results.y = scalar / rhs.y;
  results.z = scalar / rhs.z;
  results.w = scalar / rhs.w;
}

void Vector4::Divide(Vector4& results, const Vector4& lhs, float scalar)
{
  results.x = lhs.x / scalar;
	results.y = lhs.y / scalar;
  results.z = lhs.z / scalar;
  results.w = lhs.w / scalar;
}

void Vector4::Divide(Vector4& results, const Vector4& lhs, const Vector4& rhs)
{
  results.x = lhs.x / rhs.x;
	results.y = lhs.y / rhs.y;
  results.z = lhs.z / rhs.z;
  results.w = lhs.w / rhs.w;
}
