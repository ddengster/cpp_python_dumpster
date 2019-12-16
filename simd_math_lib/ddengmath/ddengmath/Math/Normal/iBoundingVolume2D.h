
#ifndef _IBOUNDINGVOLUME2D_H
#define _IBOUNDINGVOLUME2D_H

#include "Vector2.h"

//virtual class inheritance uses more memory(usually 4 bytes)
#define BOUNDINGVOL2D_USE_LESS_MEMORY 0

class iBoundingVolume2D
{
public:
  virtual void UpdateBoundingBody(const Vector2& newcenterposition) = 0;
};

#endif
