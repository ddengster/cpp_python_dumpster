
#ifndef _VECTOR4_H
#define _VECTOR4_H

#include "GenericMath.h"
#include "Vector3.h"

class Vector4
{
public:
  float x, y, z, w;

public:
  //Constructors
  inline Vector4() { }
  inline Vector4(float fX, float fY, float fZ, float fW) : x(fX), y(fY), z(fZ), w(fW) { }
  inline Vector4(const Vector4& rhs) : x(rhs.x), y(rhs.y), z(rhs.z), w(rhs.w) { }
  inline explicit Vector4(float scalar) : x(scalar), y(scalar), z(scalar), w(scalar) { }
  inline explicit Vector4(float coordinate[4]) : x(coordinate[0]), y(coordinate[1]), z(coordinate[2]), w(coordinate[3]) { }
  inline explicit Vector4(const Vector3& rhs) : x(rhs.x), y(rhs.y), z(rhs.z), w(0.0f) { }

  //access op
  inline float  operator[](unsigned int i) const { assert(i < 4); return *(&x+i); }
  inline float& operator[](unsigned int i)       { assert(i < 4); return *(&x+i); }
  inline const float* ptr() const { return &x; }
  inline float* ptr()             { return &x; }
  inline void Set(float fx, float fy, float fz, float fw) { x = fx; y = fy; z = fz; w = fw; }

  //assignment op
  inline Vector4& operator=(const Vector4& rhs) { x = rhs.x; y = rhs.y; z = rhs.z; w = rhs.w; return *this; }
  inline Vector4& operator=(const Vector3& rhs) { x = rhs.x; y = rhs.y; z = rhs.z; w = 0;     return *this; }
  inline Vector4& operator=(float scalar) { x = y = z = w = scalar; return *this; }

  //comparison op
  inline bool operator==(const Vector4& rhs) const { return ((x == rhs.x) && (y == rhs.y) && (z == rhs.z) && (w == rhs.w)); } 
  inline bool operator!=(const Vector4& rhs) const { return ((x != rhs.x) || (y != rhs.y) && (z != rhs.z) && (w != rhs.w)); }
  inline bool operator<(const Vector4& rhs) const  { return ((x < rhs.x)  && (y < rhs.y)  && (z < rhs.z)  && (w < rhs.w));  }
  inline bool operator>(const Vector4& rhs) const  { return ((x > rhs.x)  && (y > rhs.y)  && (z > rhs.z)  && (w > rhs.w));  }
  inline bool IsEqual(const Vector4& rhs, float error = 0) const { 
    return ((fabs(x - rhs.x) <= error) && (fabs(y - rhs.y) <= error) && (fabs(z - rhs.z) <= error) && (fabs(w - rhs.w) <= error)); 
  }

  //basic member arithmetic op
  inline Vector4 operator+(const Vector4& rhs) const { return Vector4(x + rhs.x, y + rhs.y, z + rhs.z, w + rhs.w); }
  inline Vector4 operator-(const Vector4& rhs) const { return Vector4(x - rhs.x, y - rhs.y, z - rhs.z, w - rhs.w); }
  inline Vector4 operator*(float scalar) const { return Vector4(x * scalar, y * scalar, z * scalar, w * scalar); }
  inline Vector4 operator/(float scalar) const { 
    assert(scalar != 0.0); 
    float fInv = 1.0f / scalar; 
    return Vector4(x * fInv, y * fInv, z * fInv, w * fInv); 
  }
  inline Vector4 operator*(const Vector4& rhs) const { return Vector4(x * rhs.x, y * rhs.y, z * rhs.z, w * rhs.w); }
  inline Vector4 operator/(const Vector4& rhs) const { return Vector4(x / rhs.x, y / rhs.y, z * rhs.z, w * rhs.w); }
  inline Vector4 operator-() const { return Vector4(-x, -y, -z, -w); }

  inline Vector4& operator+=(const Vector4& rhs);
  inline Vector4& operator-=(const Vector4& rhs);
  inline Vector4& operator*=(const Vector4& rhs);
  inline Vector4& operator/=(const Vector4& rhs);
  inline Vector4& operator+=(float scalar);
  inline Vector4& operator-=(float scalar);
  inline Vector4& operator*=(float scalar);
  inline Vector4& operator/=(float scalar);

  //advanced member arithmetic op
  inline float Length() const;
  inline float SquaredLength() const;
  inline float Distance(const Vector4& vec) const;
  inline float SquaredDistance(const Vector4& vec) const;
  inline float DotProduct(const Vector4& rhs) const;
  inline Vector4 MidPoint(const Vector4& vec) const;

  /** Normalises the vector.
		*   \remarks
		*       This method normalises the vector such that it's
		*       length / magnitude is 1. The result is called a unit vector.
		*   \note
		*       This function will not crash for zero-sized vectors, but there
		*       will be no changes made to their components.
		*/
  inline void Normalise();
  inline Vector4 NormalisedCopy(void) const;

  inline void MakeFloor(const Vector4& cmp); //makes this vector to be floor of this and the input vector
  inline void MakeCeil(const Vector4& cmp);  //makes this vector to be ceil of this and the input vector
  inline Vector4 CrossProduct(const Vector4& rhs) const; //vector3 cross product, with w set to 0
	inline bool IsZeroLength(void) const;
  inline Vector4 Reflect(const Vector4& normal) const;

  //inline Vector4 Perpendicular(void) const;
  //Quaternion GetRotationTo(const Vector4& dest, const Vector4& fallbackAxis = Vector4::ZERO) const;
  inline bool DirectionEquals(const Vector4& rhs,	const Radian& tolerance = Radian(1e-03f)) const;
  inline void Clamp(const Vector4 &min, const Vector4 &max);
  inline void Clamp(float min, float max);

  static const Vector4 ZERO;
  static const Vector4 UNIT_X;
  static const Vector4 UNIT_Y;
  static const Vector4 UNIT_Z;
  static const Vector4 UNIT_W;
  static const Vector4 NEGATIVE_UNIT_X;
  static const Vector4 NEGATIVE_UNIT_Y;
  static const Vector4 NEGATIVE_UNIT_Z;
  static const Vector4 NEGATIVE_UNIT_W;
  static const Vector4 UNIT_SCALE;

  //static arithmetic func
  static inline void Add(Vector4& results, const Vector4& lhs, const Vector4& rhs);
  static inline void Subtract(Vector4& results, const Vector4& lhs, const Vector4& rhs);
  static inline void Multiply(Vector4& results, const Vector4& lhs, float scalar);
  static inline void Multiply(Vector4& results, float scalar, const Vector4& rhs);
  static inline void Multiply(Vector4& results, const Vector4& lhs, const Vector4& rhs);
  static inline void Divide(Vector4& results, float scalar, const Vector4& rhs);
  static inline void Divide(Vector4& results, const Vector4& lhs, float scalar);
  static inline void Divide(Vector4& results, const Vector4& lhs, const Vector4& rhs);

  //global friend func
  inline friend Vector4 operator+(const Vector4& lhs, float rhs) { return Vector4(lhs.x + rhs, lhs.y + rhs, lhs.z + rhs, lhs.w + rhs); }
  inline friend Vector4 operator+(float lhs, const Vector4& rhs) { return Vector4(lhs + rhs.x, lhs + rhs.y, lhs + rhs.z, lhs + rhs.w); }
  inline friend Vector4 operator-(const Vector4& lhs, float rhs) { return Vector4(lhs.x - rhs, lhs.y - rhs, lhs.z - rhs, lhs.w - rhs); }
  inline friend Vector4 operator-(float lhs, const Vector4& rhs) { return Vector4(lhs - rhs.x, lhs - rhs.y, lhs - rhs.z, lhs - rhs.w); }
  inline friend Vector4 operator*(float lhs, const Vector4& rhs) { return Vector4(lhs * rhs.x, lhs * rhs.y, lhs * rhs.z, lhs * rhs.w); }
  inline friend Vector4 operator/(float lhs, const Vector4& rhs) { return Vector4(lhs / rhs.x, lhs / rhs.y, lhs / rhs.z, lhs / rhs.w); }

  /** Function for writing to a stream.
    */
  inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Vector4& v)
  {
      o << "Vector4(" << v.x << ", " << v.y << ", " << v.z << ", " << v.w << ")";
      return o;
  }

  inline friend std::istream& operator>>(std::istream& inStream, Vector4& vector)
  {
    inStream >> vector.x;
    inStream.ignore();
    inStream >> vector.y;
    inStream.ignore();
    inStream >> vector.z;
    inStream.ignore();
    inStream >> vector.w;

    return inStream;
  }
};

#include "Vector4.inl"

#endif
