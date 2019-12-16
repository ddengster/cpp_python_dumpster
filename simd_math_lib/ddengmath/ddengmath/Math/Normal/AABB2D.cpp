
#include "AABB2D.h"
#include "GenericMath.h"

AABB2D::AABB2D(const Vector2& position, float width, float height)
{
  mMin.x = position.x - width * 0.5f;
  mMax.x = position.x + width * 0.5f;
  
  mMin.y = position.y - height * 0.5f;
  mMax.y = position.y + height * 0.5f;
}

void AABB2D::SetHalfSize(const Vector2& halfsize)
{
  Vector2 pos(GetCenterPosition());
  mMin = pos - halfsize;
  mMax = pos + halfsize;
}

Vector2 AABB2D::GetCorner(AABB2D_CORNER_ENUM corner) const
{
  switch (corner)
  {
  case TOP_RIGHT_CORNER_2D:
    return mMax;
  case TOP_LEFT_CORNER_2D:
    return Vector2(mMin.x, mMax.y);
  case BOT_LEFT_CORNER_2D:
    return mMin;
  case BOT_RIGHT_CORNER_2D:
    return Vector2(mMax.y, mMin.y);
  default:
    assert(0 && "Invalid corner for aabb2d!");
    break;
  }
  return Vector2();
}

void AABB2D::SetCenterPosition(const Vector2& newcenterposition)
{
  Vector2 halfsize = GetHalfSize();
  mMin = newcenterposition - halfsize;
  mMax = newcenterposition + halfsize;
}

void AABB2D::UpdateBoundingBody(const Vector2& newcenterposition)
{
  SetCenterPosition(newcenterposition);
}

void AABB2D::UpdateBoundingBody(const Vector2& newcenterposition, const Vector2& newhalfsize)
{
  mMin = newcenterposition - newhalfsize;
  mMax = newcenterposition + newhalfsize;
}

void AABB2D::UpdateBoundingBody(const Vector2& newcenterposition, float newwidth, float newheight)
{
  *this = AABB2D(newcenterposition, newwidth, newheight);
}

void AABB2D::Translate(const Vector2& displacement)
{
  Vector2 position = GetCenterPosition() + displacement;
  UpdateBoundingBody(position);
}

void AABB2D::Scale(const Vector2& scale)
{
  Vector2 halfsize(GetSize() * scale * 0.5f);

  Vector2 position = GetCenterPosition();

  mMin = position - halfsize;
  mMax = position + halfsize;
}

bool AABB2D::Intersects(const Vector2& point) const
{
  return( point.x >= mMin.x  &&  point.x <= mMax.x  && 
				  point.y >= mMin.y  &&  point.y <= mMax.y );
}

bool AABB2D::Intersects(const AABB2D& aabb) const
{
  return !(((mMax.x < aabb.mMin.x) || (aabb.mMax.x < mMin.x)) ||
         ((mMax.y < aabb.mMin.y) || (aabb.mMax.y < mMin.y)));
}

void AABB2D::Merge(const AABB2D& aabb)
{
  mMax.MakeCeil(aabb.mMax);
  mMin.MakeFloor(aabb.mMin);
}

void AABB2D::Merge(const Vector2& point)
{
  mMax.MakeCeil(point);
  mMin.MakeFloor(point);
}

void AABB2D::FlipY(float window_maxy)
{
  float temp = mMax.y;
  mMax.y = window_maxy - mMin.y;
  mMin.y = window_maxy - temp;
}

void AABB2D::CheckValidity()
{
  assert( (mMin.x < mMax.x) && "Minimum X is more than Maximum X");
  assert( (mMin.y < mMax.y) && "Minimum Y is more than Maximum Y");
}