
#ifndef _VECTOR3_H
#define _VECTOR3_H

#include "Prerequisites.h"
#include "Math/Quaternion.h"
#include "Math/GenericMath.h"

DECLARE_ENGINE_NAMESPACE

/** \ingroup Maths
 * Standard 3-dimensional vector.
 *   @remarks
 *       A direction in 3D space represented as distances along the 3
 *       orthogonal axes (x, y, z). Note that positions, directions and
 *       scaling factors can be represented by a vector, depending on how
 *       you interpret the values.
 */
class /*shared*/ Vector3
{
	public:
		Real x, y, z;

	public:
		/** Default constructor.
		 *   \note
		 *       It does <b>NOT</b> initialize the vector for efficiency.
		 */
		inline Vector3()
		{
		}
		/**
		 * Constructor using Real numbers as the default value of its elements.
		 *
		 * \param fX		Value of element x
		 * \param fY		Value of element y
		 * \param fZ		Value of element z
		 */
		inline Vector3(const Real fX, const Real fY, const Real fZ)
			: x(fX), y(fY), z(fZ)
		{
		}
		/**
		 * Constructor using pointer to an array of three Real numbers as the default value of its elements.
		 *
		 * \param r					Array of element x, y and z
		 */
		inline explicit Vector3(Real* const r)
			: x(r[0]), y(r[1]), z(r[2])
		{
		}
		/**
		 * Constructor using another vector as its defualt value.
		 *
		 * \param rkVector			The other vector
		 */
		inline Vector3(const Vector3& rkVector)
			: x(rkVector.x), y(rkVector.y), z(rkVector.z)
		{
		}

		// For use with this[i] = x;
		/// Operator to access x, y or z element of this vector
		inline Real& operator[](const uint32_t i)
		{
			//assert(i < 3);

			return *((&x)+i);
		}
		// For use with x = this[i]
		inline const Real& operator[](const uint32_t i) const
		{
			//assert(i < 3);

			return *((&x)+i);
		}
		/// Pointer accessor for direct copying
		inline Real* ptr()
		{
			return &x;
		}
		/// Pointer accessor for direct copying
		inline const Real* ptr() const
		{
			return &x;
		}

		/** 
		 * Assigns the value of the other vector.
		 *   \param
		 *       rkVector The other vector
		 */
		inline Vector3& operator=(const Vector3& rkVector)
		{
			x = rkVector.x;
			y = rkVector.y;
			z = rkVector.z;

			return *this;
		}

		inline Vector3& operator=(const Real fScaler)
		{
			x = fScaler;
			y = fScaler;
			z = fScaler;

			return *this;
		}

		inline bool operator==(const Vector3& rkVector) const
		{
			return (x == rkVector.x && y == rkVector.y && z == rkVector.z);
		}
		bool IsEqual(const Vector3& rkVector)
		{
			return (x == rkVector.x && y == rkVector.y && z == rkVector.z);
		}
		inline bool operator!=(const Vector3& rkVector) const
		{
			return (x != rkVector.x || y != rkVector.y || z != rkVector.z);
		}
		bool NotEqual(const Vector3& rkVector)
		{
			return (x != rkVector.x || y != rkVector.y || z != rkVector.z);
		}
		// arithmetic operations
		inline Vector3 operator+(const Vector3& rkVector) const
		{
			return Vector3(
					x + rkVector.x,
					y + rkVector.y,
					z + rkVector.z);
		}
		static void Add(Vector3& results, const Vector3& leftOperandVector, const Vector3& rightOperandVector)
		{
			results.x = leftOperandVector.x + rightOperandVector.x;
			results.y = leftOperandVector.y + rightOperandVector.y;
			results.z = leftOperandVector.z + rightOperandVector.z;
		}
		inline Vector3 operator-(const Vector3& rkVector) const
		{
			return Vector3(
					x - rkVector.x,
					y - rkVector.y,
					z - rkVector.z);
		}
		static void Subtract(Vector3& results, const Vector3& leftOperandVector, const Vector3& rightOperandVector)
		{
			results.x = leftOperandVector.x - rightOperandVector.x;
			results.y = leftOperandVector.y - rightOperandVector.y;
			results.z = leftOperandVector.z - rightOperandVector.z;
		}
		inline Vector3 operator*(const Real fScalar) const
		{
			return Vector3(
					x * fScalar,
					y * fScalar,
					z * fScalar);
		}
		static void Multiply(Vector3& results, const Vector3& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x * fScalar;
			results.y = leftOperandVector.y * fScalar;
			results.z = leftOperandVector.z * fScalar;
		}
		inline Vector3 operator*(const Vector3& rhs) const
		{
			return Vector3(
					x * rhs.x,
					y * rhs.y,
					z * rhs.z);
		}
		static void Multiply(Vector3& results, const Vector3& leftOperandVector, const Vector3& rightOperandVector)
		{
			results.x = leftOperandVector.x * rightOperandVector.x;
			results.y = leftOperandVector.y * rightOperandVector.y;
			results.z = leftOperandVector.z * rightOperandVector.z;
		}
		inline Vector3 operator/(const Real fScalar) const
		{
			assert(fScalar != 0.0);

			Real fInv = 1.0 / fScalar;

			return Vector3(
					x * fInv,
					y * fInv,
					z * fInv);
		}
		static void Divide(Vector3& results, const Vector3& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x / fScalar;
			results.y = leftOperandVector.y / fScalar;
			results.z = leftOperandVector.z / fScalar;
		}
		inline Vector3 operator/(const Vector3& rhs) const
		{
			return Vector3(
					x / rhs.x,
					y / rhs.y,
					z / rhs.z);
		}
		static void Divide(Vector3& results, const Vector3& leftOperandVector, const Vector3& rightOperandVector)
		{
			results.x = leftOperandVector.x / rightOperandVector.x;
			results.y = leftOperandVector.y / rightOperandVector.y;
			results.z = leftOperandVector.z / rightOperandVector.z;
		}
		inline const Vector3& operator+() const
		{
			return *this;
		}

		inline Vector3 operator-() const
		{
			return Vector3(-x, -y, -z);
		}

		// overloaded operators to help Vector3
		inline friend Vector3 operator*(const Real fScalar, const Vector3& rkVector)
		{
			return Vector3(
					fScalar * rkVector.x,
					fScalar * rkVector.y,
					fScalar * rkVector.z);
		}
		static void Multiply(Vector3& results, const Real fScalar, const Vector3& rightOperandVector)
		{
			results.x = rightOperandVector.x * fScalar;
			results.y = rightOperandVector.y * fScalar;
			results.z = rightOperandVector.z * fScalar;
		}
		inline friend Vector3 operator/(const Real fScalar, const Vector3& rkVector)
		{
			return Vector3(
					fScalar / rkVector.x,
					fScalar / rkVector.y,
					fScalar / rkVector.z);
		}
		static void Divide(Vector3& results, const Real fScalar, const Vector3& rightOperandVector)
		{
			results.x = rightOperandVector.x / fScalar;
			results.y = rightOperandVector.y / fScalar;
			results.z = rightOperandVector.z / fScalar;
		}
		inline friend Vector3 operator+(const Vector3& lhs, const Real rhs)
		{
			return Vector3(
					lhs.x + rhs,
					lhs.y + rhs,
					lhs.z + rhs);
		}

		inline friend Vector3 operator+(const Real lhs, const Vector3& rhs)
		{
			return Vector3(
					lhs + rhs.x,
					lhs + rhs.y,
					lhs + rhs.z);
		}

		inline friend Vector3 operator-(const Vector3& lhs, const Real rhs)
		{
			return Vector3(
					lhs.x - rhs,
					lhs.y - rhs,
					lhs.z - rhs);
		}

		inline friend Vector3 operator-(const Real lhs, const Vector3& rhs)
		{
			return Vector3(
					lhs - rhs.x,
					lhs - rhs.y,
					lhs - rhs.z);
		}

		// arithmetic updates
		inline Vector3& operator+=(const Vector3& rkVector)
		{
			x += rkVector.x;
			y += rkVector.y;
			z += rkVector.z;

			return *this;
		}

		inline Vector3& operator+=(const Real fScalar)
		{
			x += fScalar;
			y += fScalar;
			z += fScalar;
			return *this;
		}

		inline Vector3& operator-=(const Vector3& rkVector)
		{
			x -= rkVector.x;
			y -= rkVector.y;
			z -= rkVector.z;

			return *this;
		}

		inline Vector3& operator-=(const Real fScalar)
		{
			x -= fScalar;
			y -= fScalar;
			z -= fScalar;
			return *this;
		}

		inline Vector3& operator*=(const Real fScalar)
		{
			x *= fScalar;
			y *= fScalar;
			z *= fScalar;
			return *this;
		}

		inline Vector3& operator*=(const Vector3& rkVector)
		{
			x *= rkVector.x;
			y *= rkVector.y;
			z *= rkVector.z;

			return *this;
		}

		inline Vector3& operator/=(const Real fScalar)
		{
			assert( fScalar != 0.0 );

			Real fInv = 1.0 / fScalar;

			x *= fInv;
			y *= fInv;
			z *= fInv;

			return *this;
		}

		inline Vector3& operator/=(const Vector3& rkVector)
		{
			x /= rkVector.x;
			y /= rkVector.y;
			z /= rkVector.z;

			return *this;
		}

		/** Returns the length (magnitude) of the vector.
		 *   \warning
		 *       This operation requires a square root and is expensive in
		 *       terms of CPU operations. If you don't need to know the exact
		 *       length (e.g. for just comparing lengths) use squaredLength()
		 *       instead.
		 */
		inline Real Length() const
		{
			return GenericMath::Sqrt(x * x + y * y + z * z);
		}

		/** Returns the square of the length(magnitude) of the vector.
		 *   \remarks
		 *       This  method is for efficiency - calculating the actual
		 *       length of a vector requires a square root, which is expensive
		 *       in terms of the operations required. This method returns the
		 *       square of the length of the vector, i.e. the same as the
		 *       length but before the square root is taken. Use this if you
		 *       want to find the longest / shortest vector without incurring
		 *       the square root.
		 */
		inline Real SquaredLength() const
		{
			return x * x + y * y + z * z;
		}

		/** Returns the distance to another vector.
		 *   \warning
		 *       This operation requires a square root and is expensive in
		 *       terms of CPU operations. If you don't need to know the exact
		 *       distance (e.g. for just comparing distances) use squaredDistance()
		 *       instead.
		 */
		inline Real Distance(const Vector3& rhs) const
		{
			return (*this - rhs).Length();
		}

		/** Returns the square of the distance to another vector.
		 *   @remarks
		 *       This method is for efficiency - calculating the actual
		 *       distance to another vector requires a square root, which is
		 *       expensive in terms of the operations required. This method
		 *       returns the square of the distance to another vector, i.e.
		 *       the same as the distance but before the square root is taken.
		 *       Use this if you want to find the longest / shortest distance
		 *       without incurring the square root.
		 */
		inline Real SquaredDistance(const Vector3& rhs) const
		{
			return (*this - rhs).SquaredLength();
		}

		/** Calculates the dot (scalar) product of this vector with another.
		 *   \remarks
		 *       The dot product can be used to calculate the angle between 2
		 *       vectors. If both are unit vectors, the dot product is the
		 *       cosine of the angle; otherwise the dot product must be
		 *       divided by the product of the lengths of both vectors to get
		 *       the cosine of the angle. This result can further be used to
		 *       calculate the distance of a point from a Plane.
		 *   \param
		 *       vec Vector with which to calculate the dot product (together
		 *       with this one).
		 *   \returns
		 *       A float representing the dot product value.
		 */
		inline Real DotProduct(const Vector3& vec) const
		{
			return x * vec.x + y * vec.y + z * vec.z;
		}

		/** Calculates the absolute dot (scalar) product of this vector with another.
		 *   \remarks
		 *       This function work similar dotProduct, except it use absolute value
		 *       of each component of the vector to computing.
		 *   \param
		 *       vec Vector with which to calculate the absolute dot product (together
		 *       with this one).
		 *   \returns
		 *       A Real representing the absolute dot product value.
		 */
		inline Real AbsDotProduct(const Vector3& vec) const
		{
			return GenericMath::FAbs(x * vec.x) + GenericMath::FAbs(y * vec.y) + GenericMath::FAbs(z * vec.z);
		}

		/** Normalises the vector.
		 *   \remarks
		 *       This method normalises the vector such that it's
		 *       length / magnitude is 1. The result is called a unit vector.
		 *   \note
		 *       This function will not crash for zero-sized vectors, but there
		 *       will be no changes made to their components.
		 *   \returns 
		 *		 The previous length of the vector.
		 */
		inline Real Normalise()
		{
			Real fLength = GenericMath::Sqrt(x * x + y * y + z * z);

			// Will also work for zero-sized vectors, but will change nothing
			if (fLength > 1e-08)
			{
				Real fInvLength = 1.0 / fLength;
				x *= fInvLength;
				y *= fInvLength;
				z *= fInvLength;
			}

			return fLength;
		}

		/** Calculates the cross-product of 2 vectors, i.e. the vector that
		 *   lies perpendicular to them both.
		 *   @remarks
		 *       The cross-product is normally used to calculate the normal
		 *       vector of a Plane, by calculating the cross-product of 2
		 *       non-equivalent vectors which lie on the Plane (e.g. 2 edges
		 *       of a triangle).
		 *   @param
		 *       vec Vector which, together with this one, will be used to
		 *       calculate the cross-product.
		 *   @returns
		 *       A vector which is the result of the cross-product. This
		 *       vector will <b>NOT</b> be normalised, to maximise efficiency
		 *       - call Vector3::normalise on the result if you wish this to
		 *       be done. As for which side the resultant vector will be on, the
		 *       returned vector will be on the side from which the arc from 'this'
		 *       to rkVector is anticlockwise, e.g. UNIT_Y.crossProduct(UNIT_Z)
		 *       = UNIT_X, whilst UNIT_Z.crossProduct(UNIT_Y) = -UNIT_X.
		 *		This is because OGRE uses a right-handed coordinate system.
		 *   @par
		 *       For a clearer explanation, look a the left and the bottom edges
		 *       of your monitor's screen. Assume that the first vector is the
		 *       left edge and the second vector is the bottom edge, both of
		 *       them starting from the lower-left corner of the screen. The
		 *       resulting vector is going to be perpendicular to both of them
		 *       and will go <i>inside</i> the screen.
		 */
		inline Vector3 CrossProduct(const Vector3& rkVector) const
		{
			return Vector3(
					y * rkVector.z - z * rkVector.y,
					z * rkVector.x - x * rkVector.z,
					x * rkVector.y - y * rkVector.x);
		}

		/** 
		 * Returns a vector at a point half way between this and the passed
		 *   in vector.
		 */
		inline Vector3 MidPoint(const Vector3& vec) const
		{
			return Vector3(
					( x + vec.x ) * 0.5,
					( y + vec.y ) * 0.5,
					( z + vec.z ) * 0.5 );
		}

		/** 
		 * Returns true if the vector's scalar components are all greater
		 *   that the ones of the vector it is compared against.
		 */
		inline bool operator<(const Vector3& rhs) const
		{
			if(x < rhs.x && y < rhs.y && z < rhs.z)
				return true;
			return false;
		}

		/** 
		 * Returns true if the vector's scalar components are all smaller
		 *    that the ones of the vector it is compared against.
		 */
		inline bool operator>(const Vector3& rhs) const
		{
			if(x > rhs.x && y > rhs.y && z > rhs.z)
				return true;
			return false;
		}

		/** 
		 * Sets this vector's components to the minimum of its own and the
		 *   ones of the passed in vector.
		 *   \remarks
		 *       'Minimum' in this case means the combination of the lowest
		 *       value of x, y and z from both vectors. Lowest is taken just
		 *       numerically, not magnitude, so -1 < 0.
		 */
		inline void MakeFloor(const Vector3& cmp)
		{
			if(cmp.x < x) x = cmp.x;
			if(cmp.y < y) y = cmp.y;
			if(cmp.z < z) z = cmp.z;
		}

		/** 
		 * Sets this vector's components to the maximum of its own and the
		 *   ones of the passed in vector.
		 *   \remarks
		 *       'Maximum' in this case means the combination of the highest
		 *       value of x, y and z from both vectors. Highest is taken just
		 *       numerically, not magnitude, so 1 > -3.
		 */
		inline void MakeCeil(const Vector3& cmp)
		{
			if(cmp.x > x) x = cmp.x;
			if(cmp.y > y) y = cmp.y;
			if(cmp.z > z) z = cmp.z;
		}

		/** 
		 * Generates a vector perpendicular to this vector (eg an 'up' vector).
		 *   \remarks
		 *       This method will return a vector which is perpendicular to this
		 *       vector. There are an infinite number of possibilities but this
		 *       method will guarantee to generate one of them. If you need more
		 *       control you should use the Quaternion class.
		 */
		inline Vector3 Perpendicular(void) const
		{
			static const Real fSquareZero = 1e-06 * 1e-06;

			Vector3 perp = this->CrossProduct(Vector3::UNIT_X);

			// Check length
			if(perp.SquaredLength() < fSquareZero)
			{
				/* This vector is the Y axis multiplied by a scalar, so we have
				   to use another axis.
				 */
				perp = this->CrossProduct(Vector3::UNIT_Y);
			}
			perp.Normalise();

			return perp;
		}

		/** 
		 * Generates a new random vector which deviates from this vector by a
		 *   given angle in a random direction.
		 *   \remarks
		 *       This method assumes that the random number generator has already
		 *       been seeded appropriately.
		 *   \param
		 *       angle The angle at which to deviate
		 *   \param
		 *       up Any vector perpendicular to this one (which could generated
		 *       by cross-product of this vector and any other non-colinear
		 *       vector). If you choose not to provide this the function will
		 *       derive one on it's own, however if you provide one yourself the
		 *       function will be faster (this allows you to reuse up vectors if
		 *       you call this method more than once)
		 *   @returns
		 *       A random vector which deviates from this vector by angle. This
		 *       vector will not be normalised, normalise it if you wish
		 *       afterwards.
		 */
		inline Vector3 RandomDeviant(const Radian& angle, const Vector3& up = Vector3::ZERO) const
		{
			Vector3 newUp;

			if (up == Vector3::ZERO)
			{
				// Generate an up vector
				newUp = this->Perpendicular();
			}
			else
			{
				newUp = up;
			}

			// Rotate up vector by random amount around this
			Quaternion q;
			q.FromAngleAxis(Radian(GenericMath::UnitRandom() * GenericMath::TWO_PI), *this);
			newUp = q * newUp;

			// Finally rotate this by given angle around randomised up
			q.FromAngleAxis(angle, newUp);
			return q * (*this);
		}
		inline Vector3 RandomDeviant(Real angle, const Vector3& up = Vector3::ZERO) const
		{
			return RandomDeviant (Radian(angle), up);
		}

		/** Gets the shortest arc Quaternion to rotate this vector to the destination
		 *   vector.
		 * \remarks
		 *   If you call this with a dest vector that is close to the inverse
		 *   of this vector, we will rotate 180 degrees around the 'fallbackAxis'
		 *	(if specified, or a generated axis if not) since in this case
		 *	ANY axis of rotation is valid.
		 */
		Quaternion GetRotationTo(const Vector3& dest, const Vector3& fallbackAxis = Vector3::ZERO) const
		{
			// Based on Stan Melax's article in Game Programming Gems
			Quaternion q;
			// Copy, since cannot modify local
			Vector3 v0 = *this;
			Vector3 v1 = dest;
			v0.Normalise();
			v1.Normalise();

			Real d = v0.DotProduct(v1);
			// If dot == 1, vectors are the same
			if (d >= 1.0f)
			{
				return Quaternion::IDENTITY;
			}
			if (d < (1e-6f - 1.0f))
			{
				if (fallbackAxis != Vector3::ZERO)
				{
					// rotate 180 degrees about the fallback axis
					q.FromAngleAxis(Radian(PI), fallbackAxis);
				}
				else
				{
					// Generate an axis
					Vector3 axis = Vector3::UNIT_X.CrossProduct(*this);
					if (axis.IsZeroLength()) // pick another if colinear
						axis = Vector3::UNIT_Y.CrossProduct(*this);
					axis.Normalise();
					q.FromAngleAxis(Radian(PI), axis);
				}
			}
			else
			{
				Real s = GenericMath::Sqrt((1+d)*2);
				Real invs = 1 / s;

				Vector3 c = v0.CrossProduct(v1);

				q.x = c.x * invs;
				q.y = c.y * invs;
				q.z = c.z * invs;
				q.w = s * 0.5;
				q.Normalise();
			}
			return q;
		}

		/** Returns true if this vector is zero length. */
		inline bool IsZeroLength(void) const
		{
			Real sqlen = (x * x) + (y * y) + (z * z);
			return (sqlen < (1e-06 * 1e-06));
		}

		/** As normalise, except that this vector is unaffected and the
		  normalised vector is returned as a copy. */
		inline Vector3 NormalisedCopy(void) const
		{
			Vector3 ret = *this;
			ret.Normalise();
			return ret;
		}

		/** 
		 * Calculates a reflection vector to the Plane with the given normal .
		 * \remarks 
		 *      NB assumes 'this' is pointing AWAY FROM the Plane, invert if it is not.
		 */
		inline Vector3 Reflect(const Vector3& normal) const
		{
			return Vector3(*this - (2 * this->DotProduct(normal) * normal));
		}

		/** 
		 * Returns whether this vector is within a positional tolerance
		 *	of another vector.
		 * \param rhs The vector to compare with
		 * \param tolerance The amount that each element of the vector may vary by
		 *	and still be considered equal
		 */
		inline bool PositionEquals(const Vector3& rhs, Real tolerance = 1e-03) const
		{
			return GenericMath::RealEqual(x, rhs.x, tolerance) &&
				GenericMath::RealEqual(y, rhs.y, tolerance) &&
				GenericMath::RealEqual(z, rhs.z, tolerance);
		}

		/** 
		 * Returns whether this vector is within a positional tolerance
		 *	of another vector, also take scale of the vectors into account.
		 * \param rhs The vector to compare with
		 * \param tolerance The amount (related to the scale of vectors) that distance
		 *   of the vector may vary by and still be considered close
		 */
		inline bool PositionCloses(const Vector3& rhs, Real tolerance = 1e-03f) const
		{
			return SquaredDistance(rhs) <= (SquaredLength() + rhs.SquaredLength()) * tolerance;
		}

		/** Returns whether this vector is within a directional tolerance
		 *	of another vector.
		 * \param 
		 *     rhs The vector to compare with
		 * \param 
		 *     tolerance The maximum angle by which the vectors may vary and
		 *	   still be considered equal
		 * \note 
		 *     Both vectors should be normalised.
		 */
		inline bool DirectionEquals(const Vector3& rhs,	const Radian& tolerance) const
		{
			Real dot = DotProduct(rhs);
			Radian angle(GenericMath::ACos(dot));

			return (GenericMath::FAbs(angle.GetValue()) <= tolerance.GetValue());
		}

		// special points
		static const Vector3 ZERO;
		static const Vector3 UNIT_X;
		static const Vector3 UNIT_Y;
		static const Vector3 UNIT_Z;
		static const Vector3 NEGATIVE_UNIT_X;
		static const Vector3 NEGATIVE_UNIT_Y;
		static const Vector3 NEGATIVE_UNIT_Z;
		static const Vector3 UNIT_SCALE;

		/** Function for writing to a stream.
		 */
		inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Vector3& v)
		{
			o << "Vector3(" << v.x << ", " << v.y << ", " << v.z << ")";
			return o;
		}
};

END_ENGINE_NAMESPACE

#endif
