
#ifndef _IBOUNDINGVOLUME3D_H
#define _IBOUNDINGVOLUME3D_H

#include "Vector3.h"

//virtual class inheritance uses more memory(usually 4 bytes for vtable)
#define BOUNDINGVOL3D_USE_LESS_MEMORY 1

class iBoundingVolume3D
{
public:
  virtual void UpdateBoundingBody(const Vector3& newposition) = 0;
};

#endif
