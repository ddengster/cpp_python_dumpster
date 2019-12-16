
#include "Vector2.h"
#include "Vector3.h"

const Vector2 Vector2::ZERO(0.0f, 0.0f);
const Vector2 Vector2::UNIT_X(1.0f, 0.0f);
const Vector2 Vector2::UNIT_Y(0.0f, 1.0f);
const Vector2 Vector2::NEGATIVE_UNIT_X(-1.0f, 0.0f);
const Vector2 Vector2::NEGATIVE_UNIT_Y(0.0f, -1.0f);
const Vector2 Vector2::UNIT_SCALE(1, 1);

Vector2::Vector2(const Vector3& rhs)
 : x(rhs.x), y(rhs.y)
{
}