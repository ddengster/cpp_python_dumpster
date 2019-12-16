

Vector3& Vector3::operator+=(const Vector3& rhs)
{
  x += rhs.x;
  y += rhs.y;
  z += rhs.z;
  return *this;
}

Vector3& Vector3::operator-=(const Vector3& rhs)
{
  x -= rhs.x;
  y -= rhs.y;
  z -= rhs.z;
  return *this;
}

Vector3& Vector3::operator*=(const Vector3& rhs)
{
  x *= rhs.x;
  y *= rhs.y;
  z *= rhs.z;
  return *this;
}

Vector3& Vector3::operator/=(const Vector3& rhs)
{
  x /= rhs.x;
  y /= rhs.y;
  z /= rhs.z;
  return *this;
}

Vector3& Vector3::operator+=(float scalar)
{
  x += scalar;
  y += scalar;
  z += scalar;
  return *this;
}

Vector3& Vector3::operator-=(float scalar)
{
  x -= scalar;
  y -= scalar;
  z -= scalar;
  return *this;
}

Vector3& Vector3::operator*=(float scalar)
{
  x *= scalar;
  y *= scalar;
  z *= scalar;
  return *this;
}

Vector3& Vector3::operator/=(float scalar)
{
  assert( scalar != 0.0 );

	float fInv = 1.0f / scalar;

	x *= fInv;
	y *= fInv;
  z *= fInv;

	return *this;
}

float Vector3::Length() const
{
  return GenericMath::Sqrt(x * x + y * y + z * z);
}

float Vector3::SquaredLength() const
{
  return x * x + y * y + z * z;
}

float Vector3::Distance(const Vector3& vec) const
{
	return (*this - vec).Length();
}

float Vector3::SquaredDistance(const Vector3& vec) const
{
  return (*this - vec).SquaredLength();
}

float Vector3::DotProduct(const Vector3& rhs) const
{
  return x * rhs.x + y * rhs.y + z * rhs.z;
}

Vector3 Vector3::MidPoint(const Vector3& vec) const
{
  return Vector3((x + vec.x) * 0.5f, (y + vec.y) * 0.5f, (z + vec.z) * 0.5f);
}

void Vector3::Normalise()
{
  float fLength = GenericMath::Sqrt(x * x + y * y + z * z);

	// Will also work for zero-sized vectors
	if (fLength > ZERO_ERROR_RANGE)
	{
		float fInvLength = 1.0f / fLength;
		x *= fInvLength;
		y *= fInvLength;
    z *= fInvLength;
	}
}

Vector3 Vector3::NormalisedCopy(void) const
{
  Vector3 ret = *this;
  ret.Normalise();
  return ret;
}

void Vector3::MakeFloor(const Vector3& cmp)
{
  if (cmp.x < x) x = cmp.x;
	if (cmp.y < y) y = cmp.y;
  if (cmp.z < z) z = cmp.z;
}

void Vector3::MakeCeil(const Vector3& cmp)
{
  if (cmp.x > x) x = cmp.x;
	if (cmp.y > y) y = cmp.y;
  if (cmp.z > z) z = cmp.z;
}

Vector3 Vector3::CrossProduct(const Vector3& rhs) const
{
  return Vector3( y * rhs.z - z * rhs.y,
					        z * rhs.x - x * rhs.z,
					        x * rhs.y - y * rhs.x );
}

bool Vector3::IsZeroLength(void) const
{
  float sqlen = (x * x) + (y * y) + (z * z);
  return (sqlen < (ZERO_ERROR_RANGE * ZERO_ERROR_RANGE));
}

Vector3 Vector3::Reflect(const Vector3& normal) const
{
  return Vector3(*this - (2 * this->DotProduct(normal) * normal));
}

bool Vector3::DirectionEquals(const Vector3& rhs,	const Radian& tolerance) const
{
  float dot = NormalisedCopy().DotProduct(rhs.NormalisedCopy());
	Radian angle(GenericMath::ArcCos(dot));

	return (GenericMath::FAbs(angle.GetValue()) <= tolerance.GetValue());
}

void Vector3::Clamp(const Vector3 &min, const Vector3 &max)
{
  x = x < min.x ? min.x : ( x > max.x ) ? max.x : x; 
  y = y < min.y ? min.y : ( y > max.y ) ? max.y : y; 
  z = z < min.z ? min.z : ( z > max.z ) ? max.z : z; 
}

void Vector3::Clamp(float min, float max)
{
  x = x < min ? min : ( x > max ) ? max : x; 
  y = y < min ? min : ( y > max ) ? max : y; 
  z = z < min ? min : ( z > max ) ? max : z; 
}

void Vector3::Add(Vector3& results, const Vector3& lhs, const Vector3& rhs)
{
  results.x = lhs.x + rhs.x;
	results.y = lhs.y + rhs.y;
  results.z = lhs.z + rhs.z;
}

void Vector3::Subtract(Vector3& results, const Vector3& lhs, const Vector3& rhs)
{
  results.x = lhs.x - rhs.x;
	results.y = lhs.y - rhs.y;
  results.z = lhs.z - rhs.z;
}

void Vector3::Multiply(Vector3& results, const Vector3& lhs, float scalar)
{
  results.x = lhs.x * scalar;
	results.y = lhs.y * scalar;
  results.z = lhs.z * scalar;
}

void Vector3::Multiply(Vector3& results, float scalar, const Vector3& rhs)
{
  results.x = rhs.x * scalar;
	results.y = rhs.y * scalar;
  results.z = rhs.z * scalar;
}

void Vector3::Multiply(Vector3& results, const Vector3& lhs, const Vector3& rhs)
{
  results.x = lhs.x * rhs.x;
	results.y = lhs.y * rhs.y;
  results.z = lhs.z * rhs.z;
}

void Vector3::Divide(Vector3& results, float scalar, const Vector3& rhs)
{
  results.x = scalar / rhs.x;
	results.y = scalar / rhs.y;
  results.z = scalar / rhs.z;
}

void Vector3::Divide(Vector3& results, const Vector3& lhs, float scalar)
{
  results.x = lhs.x / scalar;
	results.y = lhs.y / scalar;
  results.z = lhs.z / scalar;
}

void Vector3::Divide(Vector3& results, const Vector3& lhs, const Vector3& rhs)
{
  results.x = lhs.x / rhs.x;
	results.y = lhs.y / rhs.y;
  results.z = lhs.z / rhs.z;
}


