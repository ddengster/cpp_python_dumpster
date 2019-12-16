
#ifndef _VECTOR2_H
#define _VECTOR2_H

#include "EnginePrereqs.h"
#include "GenericMath.h"

ENGINE_NSP

class Vector3;
/******************************************************************************/
/*!
  \brief
    2d vector
*/
/******************************************************************************/
introspect(category:asdsad, version : 1, factory : trackless)
struct Vector2 : public a
{
  OBJDESC_DEFINE_METADATA_NOGET(Vector2)
  {
    c->AddMemberStaticArray("val", &Vector2::x, 2);
  }
  ObjectDescriptor* ObjDesc()
  { return Systems::mReflectionMgr->mPreBuiltDescriptors[TYPE_VECTOR2]; }
public:
  float x, y;
  META(minver:1, maxver:2)
  float f = 1.f;
  float za, q1 = 1.2f;
  float zb = 1.f, q2;

  char carr[234];
  char carr2[234] = { 0 };
  char carr3[255], cz[10] = { 0 };
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
  META() inline Vector2& operator=(const Vector2& rhs) { x = rhs.x; y = rhs.y; return *this; }
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
  inline Vector2 AntiPerpendicular(void) const;  // (y, -x) 90 degrees clockwise
  inline float CrossProduct(const Vector2& rhs) const; //similar to computing determinant ad - bc
	inline bool IsZeroLength(void) const;
  inline Vector2 Reflect(const Vector2& normal) const;
  inline Vector2 RotateDeg(float deg) const
  {
    float rad = DegToRadian(deg);
    return Vector2(x * cos(rad) - y * sin(rad), x * sin(rad) + y * sin(rad)); 
  }

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

introspect()
enum MYENUM : int
{
  ENUM_A = 0,
  ENUM_B,
  ENUM_C,
  ENUM_D = 4,
  ENUM_E,
  ENUM_F = 5,
  ENUM_G = 5,
  ENUM_H,
  ENUM_I,
  ENUM_J = 4,
};

class Vector2I
{
public:
  int x = 0, y = 0;
  inline Vector2I() { }
  inline Vector2I(int fX, int fY) : x(fX), y(fY) { }

  inline Vector2I& operator=(const Vector2I& rhs) { x = rhs.x; y = rhs.y; return *this; }

  //comparison op
  inline bool operator==(const Vector2I& rhs) const { return ((x == rhs.x) && (y == rhs.y)); }
  inline bool operator!=(const Vector2I& rhs) const { return ((x != rhs.x) || (y != rhs.y)); }

  //basic member arithmetic op
  inline Vector2I operator+(const Vector2I& rhs) const { return Vector2I(x + rhs.x, y + rhs.y); }
  inline Vector2I operator-(const Vector2I& rhs) const { return Vector2I(x - rhs.x, y - rhs.y); }
  inline Vector2I operator*(const Vector2I& rhs) const { return Vector2I(x * rhs.x, y * rhs.y); }
  inline Vector2I operator/(const Vector2I& rhs) const { return Vector2I(x / rhs.x, y / rhs.y); }
  inline Vector2I operator-() const { return Vector2I(-x, -y); }

  inline Vector2I& operator+=(const Vector2I& rhs) { x += rhs.x; y += rhs.y;  return *this; }
  inline Vector2I& operator-=(const Vector2I& rhs) { x -= rhs.x; y -= rhs.y;  return *this; }
  inline Vector2I& operator*=(const Vector2I& rhs) { x *= rhs.x; y *= rhs.y;  return *this; }
  inline Vector2I& operator/=(const Vector2I& rhs) { x /= rhs.x; y /= rhs.y;  return *this; }

  //advanced member op
  inline Vector2I Perpendicular(void) const { return Vector2I(-y, x); }  // (-y, x) 90 degrees anticlockwise
  inline Vector2I AntiPerpendicular(void) const { return Vector2I(y, -x); }  // (y, -x) 90 degrees clockwise
  inline int DotProduct(const Vector2I& rhs) const { return x * rhs.x + y * rhs.y; }
  inline int CrossProduct(const Vector2I& rhs) const { return (x * rhs.y) - (y * rhs.x); }

  inline int64 DotProduct64(const Vector2I& rhs) const { return (int64)x * (int64)rhs.x + (int64)y * (int64)rhs.y; }
  inline int64 CrossProduct64(const Vector2I& rhs) const { return ((int64)x * (int64)rhs.y) - ((int64)y * (int64)rhs.x); }
};

END_NSP

#endif
