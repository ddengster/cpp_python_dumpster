
#ifndef _GENERICMATH_H
#define _GENERICMATH_H

#include <math.h>
#include <stdint.h>
#include <limits>
#include <assert.h>
#include <iostream>

class Degree;
class Radian;

#define PI 3.14159265358979323846f
#define ZERO_ERROR_RANGE 1e-07f

class GenericMath
{
public:
    // Wrappers to hide implementations of functions.  The ArcCos and ArcSin
    // functions clamp the input argument to [-1,1] to avoid NaN issues
    // when the input is slightly larger than 1 or slightly smaller than -1.
    // Other functions have the potential for using a fast and approximate
    // algorithm rather than calling the standard math library functions.

	/** Trigo functions **/
  static inline float ArcCos (float fValue);
  static inline float ArcSin (float fValue);
  static inline float ArcTan (float fValue);
  static inline float ArcTan2 (float fY, float fX);

	static inline float Sin (float fValue);
	static inline float Cos (float fValue);
	static inline float Tan (float fValue);
  /*********************/

  /** Logarithm functions **/
  static inline float Log (float fValue);
  static inline float Log2 (float fValue);
  static inline float Log10 (float fValue);
  static inline float Power (float fBase, float fExponent);
  /*********************/

  /** Sqr/t functions **/
  static inline float Sqr (float fValue);
  static inline float Sqrt (float fValue);
  static inline float InvSqrt (float fValue);
  /*********************/

  /** Util Functions **/
  static inline float Ceil (float fValue);
  static inline float Exp (float fValue);
  static inline float FAbs (float fValue);
  static inline float Floor (float fValue);
  static inline float FMod (float fX, float fY);
  /*********************/

  // Return -1 if the input is negative, 0 if the input is zero, and +1
  // if the input is positive.
  static inline int	IsSigned (int iValue);
  static inline float IsSigned(float fValue);

  // Fast evaluation of trigonometric and inverse trigonometric functions
  // using polynomial approximations.  The speed ups were measured on an
  // AMD 2800 (2.08 GHz) processor using Visual Studion .NET 2003 with a
  // release build.

  // The input must be in [0,pi/2].
  // max error sin0 = 1.7e-04, speed up = 4.0
  // max error sin1 = 1.9e-08, speed up = 2.8
  static inline float FastSin0 (float fAngle);
  static inline float FastSin1 (float fAngle);

  // The input must be in [0,pi/2]
  // max error cos0 = 1.2e-03, speed up = 4.5
  // max error cos1 = 6.5e-09, speed up = 2.8
  static inline float FastCos0 (float fAngle);
  static inline float FastCos1 (float fAngle);

  // The input must be in [0,pi/4].
  // max error tan0 = 8.1e-04, speed up = 5.6
  // max error tan1 = 1.9e-08, speed up = 3.4
  static inline float FastTan0 (float fAngle);
  static inline float FastTan1 (float fAngle);

  // The input must be in [0,1].
  // max error invsin0 = 6.8e-05, speed up = 7.5
  // max error invsin1 = 1.4e-07, speed up = 5.5
  static inline float FastInvSin0 (float fValue);
  static inline float FastInvSin1 (float fValue);

  // The input must be in [0,1].
  // max error invcos0 = 6.8e-05, speed up = 7.5
  // max error invcos1 = 1.4e-07, speed up = 5.7
  static inline float FastInvCos0 (float fValue);
  static inline float FastInvCos1 (float fValue);

  // The input must be in [-1,1]. 
  // max error invtan0 = 1.2e-05, speed up = 2.8
  // max error invtan1 = 2.3e-08, speed up = 1.8
  static inline float FastInvTan0 (float fValue);
  static inline float FastInvTan1 (float fValue);

  // A fast approximation to 1/sqrt.
  static inline float FastInvSqrt (float fValue);

  // Fast approximations to exp(-x).  The input x must be in [0,infinity).
  // max error negexp0 = 0.00024, speed up = 25.4
  // max error negexp1 = 0.000024, speed up = 25.4
  // max error negexp2 = 0.0000024, speed up = 20.5
  // max error negexp3 = 0.00000025, speed up = 17.3
  static inline float FastNegExp0 (float fValue);
  static inline float FastNegExp1 (float fValue);
  static inline float FastNegExp2 (float fValue);
  static inline float FastNegExp3 (float fValue);

	static const float DEG_TO_RAD;
	static const float RAD_TO_DEG;
	static Radian ConvertDegreeToRadian(Degree deg);
	static Degree ConvertRadianToDegree(Radian rad);

  static const float TWO_PI;
  static const float HALF_PI;
  static const float INV_PI;
  static const float INV_TWO_PI;
  static const float LN_2;
  static const float LN_10;
  static const float INV_LN_2;
  static const float INV_LN_10;
	static const float POSITIVE_INFINITY;
	static const float NEGATIVE_INFINITY;

	/** Compare 2 floats, using tolerance for inaccuracies.
	 */
	static inline bool floatEqual(float a, float b, float tolerance = std::numeric_limits<float>::epsilon());

};

class Radian
{
private:
	float mRad;
public:
	explicit Radian(float r=0) : mRad(r) {}
	Radian& operator=(const Radian& r) { mRad = r.mRad; return *this; }

	Degree	ConvertToDegree() const;
	float	GetValue() const { return mRad; }
	void	SetValue(float val) { mRad = val; }

	const Radian& operator+() const { return *this; }
	Radian operator+(const Radian& r) const { return Radian ( mRad + r.mRad ); }
	Radian& operator+=(const Radian& r) { mRad += r.mRad; return *this; }
	Radian operator-() const { return Radian(-mRad); }
	Radian operator-(const Radian& r) const { return Radian ( mRad - r.mRad ); }
	Radian& operator-=(const Radian& r) { mRad -= r.mRad; return *this; }
	Radian operator*(float f) const { return Radian ( mRad * f ); }
	Radian operator*(const Radian& f) const { return Radian ( mRad * f.mRad ); }
	Radian& operator*=(float f) { mRad *= f; return *this; }
	Radian operator/( float f ) const { return Radian ( mRad / f ); }
	Radian& operator/=( float f ) { mRad /= f; return *this; }

	bool operator<(const Radian& r) const { return mRad <  r.mRad; }
	bool operator<=(const Radian& r) const { return mRad <= r.mRad; }
	bool operator==(const Radian& r) const { return mRad == r.mRad; }
	bool operator!=(const Radian& r) const { return mRad != r.mRad; }
	bool operator>=(const Radian& r) const { return mRad >= r.mRad; }
	bool operator>(const Radian& r) const { return mRad >  r.mRad; }
};

class Degree
{
private:
	float mDeg;

public:
	explicit Degree(float d=0) : mDeg(d) {}
	Degree& operator=(const Degree& d) { mDeg = d.mDeg; return *this; }

	//TODO:inline?
	Radian	ConvertToRadian() const;
	float	GetValue() const { return mDeg; }
	void	SetValue(float val) { mDeg = val; }

	const Degree& operator+() const { return *this; }
	Degree operator+(const Degree& d) const { return Degree ( mDeg + d.mDeg ); }
	Degree& operator+=(const Degree& d) { mDeg += d.mDeg; return *this; }
	Degree operator-() const { return Degree(-mDeg); }
	Degree operator-(const Degree& d) const { return Degree ( mDeg - d.mDeg ); }
	Degree& operator-=(const Degree& d) { mDeg -= d.mDeg; return *this; }
	Degree operator*(float f) const { return Degree ( mDeg * f ); }
	Degree operator*(const Degree& f) const { return Degree ( mDeg * f.mDeg ); }
	Degree& operator*=(float f) { mDeg *= f; return *this; }
	Degree operator/(float f) const { return Degree ( mDeg / f ); }
	Degree& operator/=(float f) { mDeg /= f; return *this; }

	bool operator<(const Degree& d) const { return mDeg <  d.mDeg; }
	bool operator<=(const Degree& d) const { return mDeg <= d.mDeg; }
	bool operator==(const Degree& d) const { return mDeg == d.mDeg; }
	bool operator!=(const Degree& d) const { return mDeg != d.mDeg; }
	bool operator>=(const Degree& d) const { return mDeg >= d.mDeg; }
	bool operator>(const Degree& d) const { return mDeg >  d.mDeg; }
};

#include "GenericMath.inl"

#endif
