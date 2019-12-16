
#ifndef _GENERICMATH_H
#define _GENERICMATH_H

#include "Prerequisites.h"
#include <math.h>
#include <limits>

/** \ingroup Maths
 * Generic Class for Trigo functions, random seed generation functions, etc.
 * Taken from http://www.geometrictools.com/LibFoundation/Mathematics/Wm4Math.h
 */
DECLARE_ENGINE_NAMESPACE

class Sphere;
class Ray;
class Plane;
class AxisAlignedBox;

class Degree;
class Radian
{
private:
	Real mRad;
public:
	explicit Radian(Real r=0) : mRad(r) {}
	Radian& operator=(const Radian& r) { mRad = r.mRad; return *this; }

	//TODO:inline?
	Degree	ConvertToDegree() const;
	Real	GetValue() const { return mRad; }
	void	SetValue(Real val) { mRad = val; }

	const Radian& operator+() const { return *this; }
	Radian operator+(const Radian& r) const { return Radian ( mRad + r.mRad ); }
	Radian& operator+=(const Radian& r) { mRad += r.mRad; return *this; }
	Radian operator-() const { return Radian(-mRad); }
	Radian operator-(const Radian& r) const { return Radian ( mRad - r.mRad ); }
	Radian& operator-=(const Radian& r) { mRad -= r.mRad; return *this; }
	Radian operator*(Real f) const { return Radian ( mRad * f ); }
	Radian operator*(const Radian& f) const { return Radian ( mRad * f.mRad ); }
	Radian& operator*=(Real f) { mRad *= f; return *this; }
	Radian operator/( Real f ) const { return Radian ( mRad / f ); }
	Radian& operator/=( Real f ) { mRad /= f; return *this; }

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
	Real mDeg;

public:
	explicit Degree(Real d=0) : mDeg(d) {}
	Degree& operator=(const Degree& d) { mDeg = d.mDeg; return *this; }

	//TODO:inline?
	Radian	ConvertToRadian() const;
	Real	GetValue() const { return mDeg; }
	void	SetValue(Real val) { mDeg = val; }

	const Degree& operator+() const { return *this; }
	Degree operator+(const Degree& d) const { return Degree ( mDeg + d.mDeg ); }
	Degree& operator+=(const Degree& d) { mDeg += d.mDeg; return *this; }
	Degree operator-() const { return Degree(-mDeg); }
	Degree operator-(const Degree& d) const { return Degree ( mDeg - d.mDeg ); }
	Degree& operator-=(const Degree& d) { mDeg -= d.mDeg; return *this; }
	Degree operator*(Real f) const { return Degree ( mDeg * f ); }
	Degree operator*(const Degree& f) const { return Degree ( mDeg * f.mDeg ); }
	Degree& operator*=(Real f) { mDeg *= f; return *this; }
	Degree operator/(Real f) const { return Degree ( mDeg / f ); }
	Degree& operator/=(Real f) { mDeg /= f; return *this; }

	bool operator<(const Degree& d) const { return mDeg <  d.mDeg; }
	bool operator<=(const Degree& d) const { return mDeg <= d.mDeg; }
	bool operator==(const Degree& d) const { return mDeg == d.mDeg; }
	bool operator!=(const Degree& d) const { return mDeg != d.mDeg; }
	bool operator>=(const Degree& d) const { return mDeg >= d.mDeg; }
	bool operator>(const Degree& d) const { return mDeg >  d.mDeg; }
};

class GenericMath
{
public:
    // Wrappers to hide implementations of functions.  The ACos and ASin
    // functions clamp the input argument to [-1,1] to avoid NaN issues
    // when the input is slightly larger than 1 or slightly smaller than -1.
    // Other functions have the potential for using a fast and approximate
    // algorithm rather than calling the standard math library functions.

	/** Trigo functions **/
    static inline Real ACos (Real fValue);
    static inline Real ASin (Real fValue);
    static inline Real ATan (Real fValue);
    static inline Real ATan2 (Real fY, Real fX);

	static inline Real Sin (Real fValue);
	static inline Real Cos (Real fValue);
	static inline Real Tan (Real fValue);

	/** ??? Functions **/
    static inline Real Ceil (Real fValue);
    static inline Real Exp (Real fValue);
    static inline Real FAbs (Real fValue);
    static inline Real Floor (Real fValue);
    static inline Real FMod (Real fX, Real fY);
    
	/** Logarithm functions **/
    static inline Real Log (Real fValue);
    static inline Real Log2 (Real fValue);
    static inline Real Log10 (Real fValue);
    static inline Real Power (Real fBase, Real fExponent);
    
	/** Sqr/t functions **/
    static inline Real Sqr (Real fValue);
    static inline Real Sqrt (Real fValue);
    static inline Real InvSqrt (Real fValue);

	// Return -1 if the input is negative, 0 if the input is zero, and +1
    // if the input is positive.
    static inline int	IsSigned (int iValue);
    static inline Real IsSigned(Real fValue);

	///////////////////////////
	//Randomization functions//
	///////////////////////////
    // Generate a random number in [0,1).  The random number generator may
    // be seeded by a first call to UnitRandom with a positive seed.
    static inline Real UnitRandom(unsigned int uiSeed = 0);

	// Generate a random number in [-1,1).  The random number generator may
    // be seeded by a first call to SymmetricRandom with a positive seed.
    static inline Real SymmetricRandom(unsigned int uiSeed = 0);

	// Generate a random number in [min,max).  The random number generator may
    // be seeded by a first call to IntervalRandom with a positive seed.
    static inline Real IntervalRandom(Real fMin, Real fMax, unsigned int uiSeed = 0);

	
	// Fast evaluation of trigonometric and inverse trigonometric functions
    // using polynomial approximations.  The speed ups were measured on an
    // AMD 2800 (2.08 GHz) processor using Visual Studion .NET 2003 with a
    // release build.

    // The input must be in [0,pi/2].
    // max error sin0 = 1.7e-04, speed up = 4.0
    // max error sin1 = 1.9e-08, speed up = 2.8
    static inline Real FastSin0 (Real fAngle);
    static inline Real FastSin1 (Real fAngle);

    // The input must be in [0,pi/2]
    // max error cos0 = 1.2e-03, speed up = 4.5
    // max error cos1 = 6.5e-09, speed up = 2.8
    static inline Real FastCos0 (Real fAngle);
    static inline Real FastCos1 (Real fAngle);

    // The input must be in [0,pi/4].
    // max error tan0 = 8.1e-04, speed up = 5.6
    // max error tan1 = 1.9e-08, speed up = 3.4
    static inline Real FastTan0 (Real fAngle);
    static inline Real FastTan1 (Real fAngle);

    // The input must be in [0,1].
    // max error invsin0 = 6.8e-05, speed up = 7.5
    // max error invsin1 = 1.4e-07, speed up = 5.5
    static inline Real FastInvSin0 (Real fValue);
    static inline Real FastInvSin1 (Real fValue);

    // The input must be in [0,1].
    // max error invcos0 = 6.8e-05, speed up = 7.5
    // max error invcos1 = 1.4e-07, speed up = 5.7
    static inline Real FastInvCos0 (Real fValue);
    static inline Real FastInvCos1 (Real fValue);

    // The input must be in [-1,1]. 
    // max error invtan0 = 1.2e-05, speed up = 2.8
    // max error invtan1 = 2.3e-08, speed up = 1.8
    static inline Real FastInvTan0 (Real fValue);
    static inline Real FastInvTan1 (Real fValue);

    // A fast approximation to 1/sqrt.
    static inline Real FastInvSqrt (Real fValue);

    // Fast approximations to exp(-x).  The input x must be in [0,infinity).
    // max error negexp0 = 0.00024, speed up = 25.4
    // max error negexp1 = 0.000024, speed up = 25.4
    // max error negexp2 = 0.0000024, speed up = 20.5
    // max error negexp3 = 0.00000025, speed up = 17.3
    static inline Real FastNegExp0 (Real fValue);
    static inline Real FastNegExp1 (Real fValue);
    static inline Real FastNegExp2 (Real fValue);
    static inline Real FastNegExp3 (Real fValue);

	static const Real DEG_TO_RAD;
	static const Real RAD_TO_DEG;
	static Radian ConvertDegreeToRadian(Degree deg);
	static Degree ConvertRadianToDegree(Radian rad);

    static const Real TWO_PI;
    static const Real HALF_PI;
    static const Real INV_PI;
    static const Real INV_TWO_PI;
    static const Real LN_2;
    static const Real LN_10;
    static const Real INV_LN_2;
    static const Real INV_LN_10;
	static const Real POSITIVE_INFINITY;
	static const Real NEGATIVE_INFINITY;

	/** Compare 2 reals, using tolerance for inaccuracies.
	 */
	static inline bool RealEqual(Real a, Real b, Real tolerance = std::numeric_limits<Real>::epsilon());

	/** Ray / Plane intersection, returns boolean result and distance. */
	static std::pair<bool, Real> Intersects(const Ray& ray, const Plane& plane);

	/** Ray / Sphere intersection, returns boolean result and distance. */
	static std::pair<bool, Real> Intersects(const Ray& ray, const Sphere& sphere, 
									 bool discardInside = true);

	/** Ray / box intersection, returns boolean result and distance. */
	static std::pair<bool, Real> Intersects(const Ray& ray, const AxisAlignedBox& box);

	/** Sphere / box intersection test. */
	static bool Intersects(const Sphere& sphere, const AxisAlignedBox& box);

	/** Plane / box intersection test. */
	static bool Intersects(const Plane& plane, const AxisAlignedBox& box);

	/** Sphere / Plane intersection test. 
	  @remarks NB just do a Plane.getDistance(Sphere.getCenter()) for more detail!
	 */
	static bool Intersects(const Sphere& sphere, const Plane& plane);
};

#include "GenericMath.inl"


END_ENGINE_NAMESPACE

#endif