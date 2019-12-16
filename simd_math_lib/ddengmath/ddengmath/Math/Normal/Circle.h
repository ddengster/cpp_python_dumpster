
#ifndef _Circle_H
#define _Circle_H

#include "iBoundingVolume2D.h"

#if BOUNDINGVOL2D_USE_LESS_MEMORY
class Circle
#else
class Circle : public iBoundingVolume2D
#endif
{
public:
  Vector2 mCenter;
  float mRadius;

public:
  //Constructor
  inline Circle() { }
  inline Circle(const Vector2& position, float radius) : mCenter(position), mRadius(radius) { }

  //no need for copy constructor
  //no need for assignment op

  //advanced
  inline void Translate(const Vector2& disp) { mCenter += disp; }
  inline void Scale(float scale) { mRadius *= scale; }

  void UpdateBoundingBody(const Vector2& newcenterposition) { mCenter = newcenterposition; }

  bool Intersects(const Vector2& pt) const { return (mCenter.SquaredDistance(pt) <= (mRadius * mRadius)); }
  bool Intersects(const Circle& circle) const { 
    float totalradius = mRadius + circle.mRadius;
    return (mCenter.SquaredDistance(circle.mCenter) <= (totalradius * totalradius)); 
  }

  inline friend std::ostream& operator<<(std::ostream& outStream, const Circle& circle)
  {
    outStream << "Circle: Center - " << circle.mCenter << " Radius: " << circle.mRadius << "\n";
    return outStream;
  }
};

#endif
