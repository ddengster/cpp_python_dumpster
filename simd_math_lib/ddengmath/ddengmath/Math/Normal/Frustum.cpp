
#include "Frustum.h"
#include "Matrix4.h"

Frustum::Frustum(const Matrix4& proj)
{
  mLeft = Plane(-proj.GetRow(0) - proj.GetRow(3));
  mLeft.Normalise();
  
  mRight = Plane(proj.GetRow(0) - proj.GetRow(3));
  mRight.Normalise();

  mBottom = Plane(-proj.GetRow(1) - proj.GetRow(3));
  mBottom.Normalise();

  mTop = Plane(proj.GetRow(1) - proj.GetRow(3));
  mTop.Normalise();

  mNear = Plane(-proj.GetRow(2) - proj.GetRow(3));
  mNear.Normalise();

  mFar = Plane(proj.GetRow(2) - proj.GetRow(3));
  mFar.Normalise();
}

Frustum::Frustum(const Plane planes[6])
{
  mLeft = planes[0];
  mRight = planes[1];
  mTop = planes[2];
  mBottom = planes[3];
  mNear = planes[4];
  mFar = planes[5];
}

#if 0
Frustum::Frustum(Camera* cam)
{
  //todo
}
#endif