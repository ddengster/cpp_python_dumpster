
#include "AABB3D.h"


AABB3D::AABB3D(const Vector3& position, float width, float height, float depth)
{
  mMin.x = position.x - width * 0.5f;
  mMax.x = position.x + width * 0.5f;
  
  mMin.y = position.y - height * 0.5f;
  mMax.y = position.y + height * 0.5f;

  mMin.z = position.z - depth * 0.5f;
  mMax.z = position.z + depth * 0.5f;
}

void AABB3D::SetHalfSize(const Vector3& halfsize)
{
  Vector3 pos(GetCenterPosition());
  mMin = pos - halfsize;
  mMax = pos + halfsize;
}

Vector3 AABB3D::GetCorner(AABB3D_CORNER_ENUM corner) const
{
  switch (corner)
  {
  case TOP_RIGHT_FRONT_CORNER_3D:
    return Vector3(mMax.x, mMax.y, mMin.z);
  case TOP_LEFT_FRONT_CORNER_3D:
    return Vector3(mMin.x, mMax.y, mMin.z);
  case BOT_LEFT_FRONT_CORNER_3D:
    return mMin;
  case BOT_RIGHT_FRONT_CORNER_3D:
    return Vector3(mMax.x, mMin.y, mMin.z);
  case TOP_RIGHT_BACK_CORNER_3D:
    return mMax;
  case TOP_LEFT_BACK_CORNER_3D:
    return Vector3(mMin.x, mMax.y, mMax.z);
  case BOT_LEFT_BACK_CORNER_3D:
    return Vector3(mMin.x, mMin.y, mMax.z);
  case BOT_RIGHT_BACK_CORNER_3D:
    return Vector3(mMax.x, mMin.y, mMax.z);
  default:
    assert(0 && "Invalid corner for AABB3D!");
    break;
  }
  return Vector3();
}

void AABB3D::SetCenterPosition(const Vector3& newcenterposition)
{
  Vector3 halfsize = GetHalfSize();
  mMin = newcenterposition - halfsize;
  mMax = newcenterposition + halfsize;
}

void AABB3D::UpdateBoundingBody(const Vector3& newcenterposition)
{
  SetCenterPosition(newcenterposition);
}

void AABB3D::UpdateBoundingBody(const Vector3& newcenterposition, const Vector3& newhalfsize)
{
  mMin = newcenterposition - newhalfsize;
  mMax = newcenterposition + newhalfsize;
}

void AABB3D::UpdateBoundingBody(const Vector3& newcenterposition, float newwidth, float newheight, float newdepth)
{
  *this = AABB3D(newcenterposition, newwidth, newheight, newdepth);
}

void AABB3D::Translate(const Vector3& displacement)
{
  Vector3 position = GetCenterPosition() + displacement;
  UpdateBoundingBody(position);
}

void AABB3D::Scale(const Vector3& scale)
{
  Vector3 halfsize(GetSize() * scale * 0.5f);

  Vector3 position = GetCenterPosition();

  mMin = position - halfsize;
  mMax = position + halfsize;
}

bool AABB3D::Intersects(const Vector3& point) const
{
  return( point.x >= mMin.x  &&  point.x <= mMax.x  && 
				  point.y >= mMin.y  &&  point.y <= mMax.y  &&
          point.z >= mMin.z  &&  point.z <= mMax.z  );
}

bool AABB3D::Intersects(const AABB3D& aabb) const
{
  return !( ((mMax.x < aabb.mMin.x) || (aabb.mMax.x < mMin.x)) ||
            ((mMax.y < aabb.mMin.y) || (aabb.mMax.y < mMin.y)) ||
            ((mMax.z < aabb.mMin.z) || (aabb.mMax.z < mMin.z))
          );
}

void AABB3D::Merge(const AABB3D& aabb)
{
  mMax.MakeCeil(aabb.mMax);
  mMin.MakeFloor(aabb.mMin);
}

void AABB3D::Merge(const Vector3& point)
{
  mMax.MakeCeil(point);
  mMin.MakeFloor(point);
}

float AABB3D::GetSquaredDistanceToPoint(const Vector3& pt) const
{
  float sqdist = 0.0f;
  static const float axis = 3;
  for (int i=0; i<3; ++i)
  {
    float v = pt[i];
    if (v < mMin[i])
      sqdist += (mMin[i] - v) * (mMin[i] - v);
    if (v > mMax[i])
      sqdist += (v - mMax[i]) * (v - mMax[i]);
  }
  return sqdist;
}

void AABB3D::FlipZ()
{
  float temp = mMax.z;
  mMax.z = -mMin.z;
  mMin.z = -temp;
}

void AABB3D::CheckValidity()
{
  assert( (mMin.x < mMax.x) && "Minimum X is more than Maximum X");
  assert( (mMin.y < mMax.y) && "Minimum Y is more than Maximum Y");
  assert( (mMin.z < mMax.z) && "Minimum Z is more than Maximum Z");
}