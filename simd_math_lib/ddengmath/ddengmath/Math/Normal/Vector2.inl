
Vector2& Vector2::operator+=(const Vector2& rhs)
{
  x += rhs.x;
  y += rhs.y;
  return *this;
}

Vector2& Vector2::operator-=(const Vector2& rhs)
{
  x -= rhs.x;
  y -= rhs.y;
  return *this;
}

Vector2& Vector2::operator*=(const Vector2& rhs)
{
  x *= rhs.x;
  y *= rhs.y;
  return *this;
}

Vector2& Vector2::operator/=(const Vector2& rhs)
{
  x /= rhs.x;
  y /= rhs.y;
  return *this;
}

Vector2& Vector2::operator+=(float scalar)
{
  x += scalar;
  y += scalar;
  return *this;
}

Vector2& Vector2::operator-=(float scalar)
{
  x -= scalar;
  y -= scalar;
  return *this;
}

Vector2& Vector2::operator*=(float scalar)
{
  x *= scalar;
  y *= scalar;
  return *this;
}

Vector2& Vector2::operator/=(float scalar)
{
  assert( scalar != 0.0 );

	float fInv = 1.0f / scalar;

	x *= fInv;
	y *= fInv;

	return *this;
}

float Vector2::Length() const
{
  return GenericMath::Sqrt(x * x + y * y);
}

float Vector2::SquaredLength() const
{
  return x * x + y * y;
}

float Vector2::Distance(const Vector2& vec) const
{
	return (*this - vec).Length();
}

float Vector2::SquaredDistance(const Vector2& vec) const
{
  return (*this - vec).SquaredLength();
}

float Vector2::DotProduct(const Vector2& rhs) const
{
  return x * rhs.x + y * rhs.y;
}

Vector2 Vector2::MidPoint(const Vector2& vec) const
{
  return Vector2((x + vec.x) * 0.5f, (y + vec.y) * 0.5f);
}

void Vector2::Normalise()
{
  float fLength = GenericMath::Sqrt(x * x + y * y);

	// Will also work for zero-sized vectors
	if (fLength > ZERO_ERROR_RANGE)
	{
		float fInvLength = 1.0f / fLength;
		x *= fInvLength;
		y *= fInvLength;
	}
}

Vector2 Vector2::NormalisedCopy(void) const
{
  Vector2 ret = *this;
  ret.Normalise();
  return ret;
}

void Vector2::MakeFloor(const Vector2& cmp)
{
  if(cmp.x < x) x = cmp.x;
	if(cmp.y < y) y = cmp.y;
}

void Vector2::MakeCeil(const Vector2& cmp)
{
  if(cmp.x > x) x = cmp.x;
	if(cmp.y > y) y = cmp.y;
}

Vector2 Vector2::Perpendicular(void) const
{
  return Vector2(-y, x);
}

float Vector2::CrossProduct(const Vector2& rhs) const
{
  return x * rhs.y - y * rhs.x;
}

bool Vector2::IsZeroLength(void) const
{
  float sqlen = (x * x) + (y * y);
  return (sqlen < (ZERO_ERROR_RANGE * ZERO_ERROR_RANGE));
}

Vector2 Vector2::Reflect(const Vector2& normal) const
{
  return Vector2(*this - (2 * this->DotProduct(normal) * normal));
}

void Vector2::Add(Vector2& results, const Vector2& lhs, const Vector2& rhs)
{
  results.x = lhs.x + rhs.x;
	results.y = lhs.y + rhs.y;
}

void Vector2::Subtract(Vector2& results, const Vector2& lhs, const Vector2& rhs)
{
  results.x = lhs.x - rhs.x;
	results.y = lhs.y - rhs.y;
}

void Vector2::Multiply(Vector2& results, const Vector2& lhs, float scalar)
{
  results.x = lhs.x * scalar;
	results.y = lhs.y * scalar;
}

void Vector2::Multiply(Vector2& results, float scalar, const Vector2& rhs)
{
  results.x = rhs.x * scalar;
	results.y = rhs.y * scalar;
}

void Vector2::Multiply(Vector2& results, const Vector2& lhs, const Vector2& rhs)
{
  results.x = lhs.x * rhs.x;
	results.y = lhs.y * rhs.y;
}

void Vector2::Divide(Vector2& results, float scalar, const Vector2& rhs)
{
  results.x = scalar / rhs.x;
	results.y = scalar / rhs.y;
}

void Vector2::Divide(Vector2& results, const Vector2& lhs, float scalar)
{
  results.x = lhs.x / scalar;
	results.y = lhs.y / scalar;
}

void Vector2::Divide(Vector2& results, const Vector2& lhs, const Vector2& rhs)
{
  results.x = lhs.x / rhs.x;
	results.y = lhs.y / rhs.y;
}

