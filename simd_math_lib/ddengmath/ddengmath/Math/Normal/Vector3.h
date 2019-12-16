
#ifndef _VECTOR3_H
#define _VECTOR3_H

#include "GenericMath.h"
#include "Vector2.h"

class Vector3
{
public:
  float x, y, z;

public:
  //Constructors
  inline Vector3() { }
  inline Vector3(float fX, float fY, float fZ) : x(fX), y(fY), z(fZ) { }
  inline Vector3(const Vector3& rhs) : x(rhs.x), y(rhs.y), z(rhs.z) { }
  inline explicit Vector3(float scalar) : x(scalar), y(scalar), z(scalar) { }
  inline explicit Vector3(float coordinate[3]) : x(coordinate[0]), y(coordinate[1]), z(coordinate[2]) { }
  inline explicit Vector3(const Vector2& rhs) : x(rhs.x), y(rhs.y), z(0.0f) { }

  //access op
  inline float  operator[](unsigned int i) const { assert(i < 3); return *(&x+i); }
  inline float& operator[](unsigned int i)       { assert(i < 3); return *(&x+i); }
  inline const float* ptr() const { return &x; }
  inline float* ptr()             { return &x; }
  inline void Set(float fx, float fy, float fz) { x = fx; y = fy; z = fz; }

  //assignment op
  inline Vector3& operator=(const Vector3& rhs) { x = rhs.x; y = rhs.y; z = rhs.z; return *this; }
  inline Vector3& operator=(const Vector2& rhs) { x = rhs.x; y = rhs.y; z = 0; return *this; }
  inline Vector3& operator=(float scalar) { x = y = z = scalar; return *this; }

  //comparison op
  inline bool operator==(const Vector3& rhs) const { return ((x == rhs.x) && (y == rhs.y) && (z == rhs.z)); } 
  inline bool operator!=(const Vector3& rhs) const { return ((x != rhs.x) || (y != rhs.y) && (z != rhs.z)); }
  inline bool operator<(const Vector3& rhs) const  { return ((x < rhs.x)  && (y < rhs.y)  && (z < rhs.z));  }
  inline bool operator>(const Vector3& rhs) const  { return ((x > rhs.x)  && (y > rhs.y)  && (z > rhs.z));  }
  inline bool IsEqual(const Vector3& rhs, float error = 0) const { return ((fabs(x - rhs.x) <= error) && (fabs(y - rhs.y) <= error) && (fabs(z - rhs.z) <= error)); }

  //basic member arithmetic op
  inline Vector3 operator+(const Vector3& rhs) const { return Vector3(x + rhs.x, y + rhs.y, z + rhs.z); }
  inline Vector3 operator-(const Vector3& rhs) const { return Vector3(x - rhs.x, y - rhs.y, z - rhs.z); }
  inline Vector3 operator*(float scalar) const { return Vector3(x * scalar, y * scalar, z * scalar); }
  inline Vector3 operator/(float scalar) const { 
    assert(scalar != 0.0); 
    float fInv = 1.0f / scalar; 
    return Vector3(x * fInv, y * fInv, z * fInv); 
  }
  inline Vector3 operator*(const Vector3& rhs) const { return Vector3(x * rhs.x, y * rhs.y, z * rhs.z); }
  inline Vector3 operator/(const Vector3& rhs) const { return Vector3(x / rhs.x, y / rhs.y, z * rhs.z); }
  inline Vector3 operator-() const { return Vector3(-x, -y, -z); }

  inline Vector3& operator+=(const Vector3& rhs);
  inline Vector3& operator-=(const Vector3& rhs);
  inline Vector3& operator*=(const Vector3& rhs);
  inline Vector3& operator/=(const Vector3& rhs);
  inline Vector3& operator+=(float scalar);
  inline Vector3& operator-=(float scalar);
  inline Vector3& operator*=(float scalar);
  inline Vector3& operator/=(float scalar);

  //advanced member arithmetic op
  inline float Length() const;
  inline float SquaredLength() const;
  inline float Distance(const Vector3& vec) const;
  inline float SquaredDistance(const Vector3& vec) const;
  inline float DotProduct(const Vector3& rhs) const;
  inline Vector3 MidPoint(const Vector3& vec) const;

  /** Normalises the vector.
		*   \remarks
		*       This method normalises the vector such that it's
		*       length / magnitude is 1. The result is called a unit vector.
		*   \note
		*       This function will not crash for zero-sized vectors, but there
		*       will be no changes made to their components.
		*/
  inline void Normalise();
  inline Vector3 NormalisedCopy(void) const;

  inline void MakeFloor(const Vector3& cmp); //makes this vector to be floor of this and the input vector
  inline void MakeCeil(const Vector3& cmp);  //makes this vector to be ceil of this and the input vector
  inline Vector3 CrossProduct(const Vector3& rhs) const; 
	inline bool IsZeroLength(void) const;
  inline Vector3 Reflect(const Vector3& normal) const;

  //inline Vector3 Perpendicular(void) const;
  //Quaternion GetRotationTo(const Vector3& dest, const Vector3& fallbackAxis = Vector3::ZERO) const;
  inline bool DirectionEquals(const Vector3& rhs,	const Radian& tolerance = Radian(1e-03f)) const; //involves 2 normalise, inefficient
  inline void Clamp(const Vector3 &min, const Vector3 &max);
  inline void Clamp(float min, float max);

  static const Vector3 ZERO;
  static const Vector3 UNIT_X;
  static const Vector3 UNIT_Y;
  static const Vector3 UNIT_Z;
  static const Vector3 NEGATIVE_UNIT_X;
  static const Vector3 NEGATIVE_UNIT_Y;
  static const Vector3 NEGATIVE_UNIT_Z;
  static const Vector3 UNIT_SCALE;

  //static arithmetic func
  static inline void Add(Vector3& results, const Vector3& lhs, const Vector3& rhs);
  static inline void Subtract(Vector3& results, const Vector3& lhs, const Vector3& rhs);
  static inline void Multiply(Vector3& results, const Vector3& lhs, float scalar);
  static inline void Multiply(Vector3& results, float scalar, const Vector3& rhs);
  static inline void Multiply(Vector3& results, const Vector3& lhs, const Vector3& rhs);
  static inline void Divide(Vector3& results, float scalar, const Vector3& rhs);
  static inline void Divide(Vector3& results, const Vector3& lhs, float scalar);
  static inline void Divide(Vector3& results, const Vector3& lhs, const Vector3& rhs);

  //global friend func
  inline friend Vector3 operator+(const Vector3& lhs, float rhs) { return Vector3(lhs.x + rhs, lhs.y + rhs, lhs.z + rhs); }
  inline friend Vector3 operator+(float lhs, const Vector3& rhs) { return Vector3(lhs + rhs.x, lhs + rhs.y, lhs + rhs.z); }
  inline friend Vector3 operator-(const Vector3& lhs, float rhs) { return Vector3(lhs.x - rhs, lhs.y - rhs, lhs.z - rhs); }
  inline friend Vector3 operator-(float lhs, const Vector3& rhs) { return Vector3(lhs - rhs.x, lhs - rhs.y, lhs - rhs.z); }
  inline friend Vector3 operator*(float lhs, const Vector3& rhs) { return Vector3(lhs * rhs.x, lhs * rhs.y, lhs * rhs.z); }
  inline friend Vector3 operator/(float lhs, const Vector3& rhs) { return Vector3(lhs / rhs.x, lhs / rhs.y, lhs / rhs.z); }

  /** Function for writing to a stream.
    */
  inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Vector3& v)
  {
      o << "Vector3(" << v.x << ", " << v.y << ", " << v.z << ")";
      return o;
  }

  inline friend std::istream& operator>>(std::istream& inStream, Vector3& vector)
  {
    inStream >> vector.x;
    inStream.ignore();

    inStream >> vector.y;
    inStream.ignore();
    inStream >> vector.z;

    return inStream;
  }
};

#include "Vector3.inl"

#endif
