
#ifndef _VECTOR2_H
#define _VECTOR2_H

#include "GenericMath.h"

class Vector3;

class Vector2
{
public:
  float x, y;

public:
  //Constructors
  inline Vector2() { }
  inline Vector2(float fX, float fY)    : x(fX), y(fY) { }
  inline Vector2(const Vector2& rhs) : x(rhs.x), y(rhs.y) { }
  inline explicit Vector2(float scalar) : x(scalar), y(scalar) { }
  inline explicit Vector2(float coordinate[2]) : x(coordinate[0]), y(coordinate[1]) { }
  explicit Vector2(const Vector3& rhs);

  //access op
  inline float  operator[](unsigned int i) const { assert(i < 2); return *(&x+i); }
  inline float& operator[](unsigned int i)       { assert(i < 2); return *(&x+i); }
  inline const float* ptr() const { return &x; }
  inline float* ptr()       { return &x; }
  inline void Set(float fx, float fy) { x = fx; y = fy; }

  //assignment op
  inline Vector2& operator=(const Vector2& rhs) { x = rhs.x; y = rhs.y; return *this; }
  inline Vector2& operator=(float scalar) { x = y = scalar; return *this; }

  //comparison op
  inline bool operator==(const Vector2& rhs) const { return ((x == rhs.x) && (y == rhs.y)); } 
  inline bool operator!=(const Vector2& rhs) const { return ((x != rhs.x) || (y != rhs.y)); }
  inline bool operator<(const Vector2& rhs) const  { return ((x < rhs.x)  && (y < rhs.y));  }
  inline bool operator>(const Vector2& rhs) const  { return ((x > rhs.x)  && (y > rhs.y));  }
  inline bool IsEqual(const Vector2& rhs, float error = 0) const { return ((fabs(x - rhs.x) <= error) && (fabs(y - rhs.y) <= error)); }

  //basic member arithmetic op
  inline Vector2 operator+(const Vector2& rhs) const { return Vector2(x + rhs.x, y + rhs.y); }
  inline Vector2 operator-(const Vector2& rhs) const { return Vector2(x - rhs.x, y - rhs.y); }
  inline Vector2 operator*(float scalar) const { return Vector2(x * scalar, y * scalar); }
  inline Vector2 operator/(float scalar) const { 
    assert(scalar != 0.0); 
    float fInv = 1.0f / scalar; 
    return Vector2(	x * fInv, y * fInv); 
  }
  inline Vector2 operator*(const Vector2& rhs) const { return Vector2(x * rhs.x, y * rhs.y); }
  inline Vector2 operator/(const Vector2& rhs) const { return Vector2(x / rhs.x, y / rhs.y); }
  inline Vector2 operator-() const { return Vector2(-x, -y); }

  inline Vector2& operator+=(const Vector2& rhs);
  inline Vector2& operator-=(const Vector2& rhs);
  inline Vector2& operator*=(const Vector2& rhs);
  inline Vector2& operator/=(const Vector2& rhs);
  inline Vector2& operator+=(float scalar);
  inline Vector2& operator-=(float scalar);
  inline Vector2& operator*=(float scalar);
  inline Vector2& operator/=(float scalar);

  //advanced member op
  inline float Length() const;
  inline float SquaredLength() const;
  inline float Distance(const Vector2& vec) const;
  inline float SquaredDistance(const Vector2& vec) const;
  inline float DotProduct(const Vector2& rhs) const;
  inline Vector2 MidPoint(const Vector2& vec) const;

  /** Normalises the vector.
		*   \remarks
		*       This method normalises the vector such that it's
		*       length / magnitude is 1. The result is called a unit vector.
		*   \note
		*       This function will not crash for zero-sized vectors, but there
		*       will be no changes made to their components.
		*/
  inline void Normalise();
  inline Vector2 NormalisedCopy(void) const;

  inline void MakeFloor(const Vector2& cmp); //makes this vector to be floor of this and the input vector
  inline void MakeCeil(const Vector2& cmp);  //makes this vector to be ceil of this and the input vector
  inline Vector2 Perpendicular(void) const;  // (-y, x) 90 degrees anticlockwise
  inline float CrossProduct(const Vector2& rhs) const; //similar to computing determinant ad - bc
	inline bool IsZeroLength(void) const;
  inline Vector2 Reflect(const Vector2& normal) const;

  static const Vector2 ZERO;
  static const Vector2 UNIT_X;
  static const Vector2 UNIT_Y;
  static const Vector2 NEGATIVE_UNIT_X;
  static const Vector2 NEGATIVE_UNIT_Y;
  static const Vector2 UNIT_SCALE;

  //static arithmetic func
  static inline void Add(Vector2& results, const Vector2& lhs, const Vector2& rhs);
  static inline void Subtract(Vector2& results, const Vector2& lhs, const Vector2& rhs);
  static inline void Multiply(Vector2& results, const Vector2& lhs, float scalar);
  static inline void Multiply(Vector2& results, float scalar, const Vector2& rhs);
  static inline void Multiply(Vector2& results, const Vector2& lhs, const Vector2& rhs);
  static inline void Divide(Vector2& results, float scalar, const Vector2& rhs);
  static inline void Divide(Vector2& results, const Vector2& lhs, float scalar);
  static inline void Divide(Vector2& results, const Vector2& lhs, const Vector2& rhs);

  //global friend func
  inline friend Vector2 operator+(const Vector2& lhs, float rhs) { return Vector2(lhs.x + rhs, lhs.y + rhs); }
  inline friend Vector2 operator+(float lhs, const Vector2& rhs) { return Vector2(lhs + rhs.x, lhs + rhs.y); }
  inline friend Vector2 operator-(const Vector2& lhs, float rhs) { return Vector2(lhs.x - rhs, lhs.y - rhs); }
  inline friend Vector2 operator-(float lhs, const Vector2& rhs) { return Vector2(lhs - rhs.x, lhs - rhs.y); }
  inline friend Vector2 operator*(float lhs, const Vector2& rhs) { return Vector2(lhs * rhs.x, lhs * rhs.y); }
  inline friend Vector2 operator/(float lhs, const Vector2& rhs) { return Vector2(lhs / rhs.x, lhs / rhs.y); }

  /** Function for writing to a stream.
    */
  inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Vector2& v)
  {
      o << "Vector2(" << v.x << ", " << v.y <<  ")";
      return o;
  }

  inline friend std::istream& operator>>(std::istream& inStream, Vector2& vector)
  {
    inStream >> vector.x;
    inStream.ignore();

    inStream >> vector.y;

    return inStream;
  }
};

#include "Vector2.inl"

#endif
