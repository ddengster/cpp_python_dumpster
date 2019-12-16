
#ifndef _QUATERNION_H
#define _QUATERNION_H

#include "GenericMath.h"
#include "Matrix3.h"
#include "Vector4.h"

class Quaternion
{
public:
  float w, x, y, z;

  static const Quaternion ZERO;
	static const Quaternion IDENTITY;
public:
  //constructors
  inline Quaternion(float fw = 1.0f, float fx = 0.0f, float fy = 0.0f, float fz = 0.0f) 
    :w(fw), x(fx), y(fy), z(fz) { }
  inline Quaternion(const Quaternion& rhs)
    :w(rhs.w), x(rhs.x), y(rhs.y), z(rhs.z) { }
  //inline Quaternion(const Matrix3& rot);
  inline Quaternion(const Radian& anglerad, const Vector3& axis) { SetRotationToAxis(anglerad, axis); }
  inline Quaternion(const Degree& angledeg, const Vector3& axis) { SetRotationToAxis(angledeg, axis); }
  inline Quaternion(float* valptr)	{	memcpy(&w, valptr, sizeof(float)*4);	}
  inline Quaternion(const Matrix3& mat) { GenerateFromRotationMatrix3(mat); }

  //access op
  inline float operator[](unsigned int i) const { assert(i < 4); return *(&w+i); }
  inline float& operator[](unsigned int i) { assert(i < 4); return *(&w+i); }
  inline float* ptr() { return &w; }
  inline const float* ptr() const { return &w; }

  //mutators/accessors
  Matrix3 GetRotationMatrix3() const;
  void GenerateFromRotationMatrix3(const Matrix3& rotationmat);
  inline void SetRotationToAxis(const Radian& anglerad, const Vector3& axisvector);
  inline void SetRotationToAxis(const Degree& angledeg, const Vector3& axisvector);
  void GetRotationToAxis(Radian& anglerad, Vector3& axisvector);
  void GetRotationToAxis(Degree& angledeg, Vector3& axisvector);

  //assignment op
  inline Quaternion& operator=(const Quaternion& rhs) { w = rhs.w; x = rhs.x; y = rhs.y; z = rhs.z; return *this; }

  //comparision op
  inline bool operator==(const Quaternion& rhs) const	{	return (rhs.x == x) && (rhs.y == y) && (rhs.z == z) && (rhs.w == w); }
  inline bool operator!=(const Quaternion& rhs) const { return !(*this == rhs); }

  //basic arithmetic op
  inline Quaternion operator+(const Quaternion& rhs) const;
	inline Quaternion operator-(const Quaternion& rhs) const;
  //what you want to use: combines rotations of 2 quaternions together. Order of mult matters
	inline Quaternion operator*(const Quaternion& rhs) const; 
	inline Quaternion operator*(float scalar) const;
  inline Vector3 operator*(const Vector3& rhs) const; //rotate a vector by this quaternion
  inline Quaternion operator-() const;
  
  inline Quaternion& operator+=(const Quaternion& rhs);
  inline Quaternion& operator-=(const Quaternion& rhs);
  inline Quaternion& operator*=(const Quaternion& rhs);
  inline Quaternion& operator*=(float scalar);

  //static op
  static inline void Add(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs);
	static inline void Subtract(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs);
	static inline void Multiply(Quaternion& result, const Quaternion& lhs, const Quaternion& rhs);
	static inline void Multiply(Quaternion& result, const Quaternion& lhs, float scalar);

  //global friend func
  friend inline Quaternion operator* (float scalar, const Quaternion& rkQ);
  
  //advanced op(INCOMPLETE)
  //Slerp, inverse normalize?
  
  /*WARNING: different coordinate systems for roll, pitch and yaw: 
    - x axis points forward
    - y axis points to the right
    - z axis points downwards
  */
  inline void GetRoll(float& degval); //rotation about x axis
  inline void GetPitch(float& degval); //rotation about y axis
  inline void GetYaw(float& degval); //rotation about z axis

  inline friend std::ostream& operator <<(std::ostream& o, const Quaternion& q)
	{
		o << "Quaternion(" << q.w << ", " << q.x << ", " << q.y << ", " << q.z << ")";
		return o;
	}
};

#include "Quaternion.inl"

#endif
