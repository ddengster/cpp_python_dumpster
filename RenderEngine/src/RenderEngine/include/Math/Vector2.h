
#ifndef _VECTOR2_H
#define _VECTOR2_H

#include "Prerequisites.h"
#include "Math/GenericMath.h"

DECLARE_ENGINE_NAMESPACE

/** \ingroup Maths
 * Standard 2-dimensional vector.
 *   @remarks
 *       A direction in 2D space represented as distances along the 2
 *       orthogonal axes (x, y). Note that positions, directions and
 *       scaling factors can be represented by a vector, depending on how
 *       you interpret the values.
 */
class /*shared*/ Vector2
{
	public:
		Real x, y;

	public:
		/** Default constructor.
		 *   \note
		 *       It does <b>NOT</b> initialize the vector for efficiency.
		 */
		inline Vector2()
		{
		}
		/**
		 * Constructor using Real numbers as the default value of its elements.
		 *
		 * \param fX		Value of element x
		 * \param fY		Value of element y
		 */
		inline Vector2(const Real fX, const Real fY)
			: x(fX), y(fY)
		{
		}
		/**
		 * Constructor using a single Real number as the default value of its elements.
		 *
		 * \param scaler		Value of element x and y
		 */
		inline explicit Vector2(const Real scaler)
			: x(scaler), y(scaler)
		{
		}
		/**
		 * Constructor using an array of two Real numbers as the default value of its elements.
		 *
		 * \param afCoordinate		Value of element x and y respectively
		 */
		inline explicit Vector2(const Real afCoordinate[2])
			: x(afCoordinate[0]), y(afCoordinate[1])
		{
		}
		/**
		 * Constructor using pointer to an array of two Real numbers as the default value of its elements.
		 *
		 * \param r					Array of element x and y
		 */
		inline explicit Vector2(Real* const r)
			: x(r[0]), y(r[1])
			{
			}
		/**
		 * Constructor using another vector as its defualt value.
		 *
		 * \param rkVector			The other vector
		 */
		inline Vector2(const Vector2& rkVector)
			: x(rkVector.x), y(rkVector.y)
		{
		}
		/// Operator to access x or y element of this vector
		inline Real operator[](const uint32_t i) const
		{
			//assert(i < 2);

			return *(&x+i);
		}

		inline Real& operator[](const uint32_t i)
		{
			//assert(i < 2);

			return *(&x+i);
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
		 * Assigns the value to the other vector.
		 *   \param
		 *       rkVector The other vector
		 */
		inline Vector2& operator=(const Vector2& rkVector)
		{
			x = rkVector.x;
			y = rkVector.y;

			return *this;
		}
		/** 
		 * Assigns both x and y value to a scalar.
		 *   \param
		 *       fScalar The scalar
		 */
		inline Vector2& operator=(const Real fScalar)
		{
			x = fScalar;
			y = fScalar;

			return *this;
		}

		inline bool operator==(const Vector2& rkVector) const
		{
			return (x == rkVector.x && y == rkVector.y);
		}

		inline bool operator!=(const Vector2& rkVector) const
		{
			return (x != rkVector.x || y != rkVector.y );
		}

		inline bool IsEquals(const Vector2& rkVector) const
		{
			return (x == rkVector.x && y == rkVector.y);
		}
		inline bool NotsEquals(const Vector2& rkVector) const
		{
			return (x != rkVector.x || y != rkVector.y );
		}

		// arithmetic operations
		inline Vector2 operator+(const Vector2& rkVector) const
		{
			return Vector2(	x + rkVector.x,
							y + rkVector.y);
		}
		static void Add(Vector2& results, const Vector2& leftOperandVector, const Vector2& rightOperandVector)
		{
			results.x = leftOperandVector.x + rightOperandVector.x;
			results.y = leftOperandVector.y + rightOperandVector.y;
		}
		inline Vector2 operator-(const Vector2& rkVector) const
		{
			return Vector2(	x - rkVector.x,
							y - rkVector.y);
		}
		static void Subtract(Vector2& results, const Vector2& leftOperandVector, const Vector2& rightOperandVector)
		{
			results.x = leftOperandVector.x - rightOperandVector.x;
			results.y = leftOperandVector.y - rightOperandVector.y;
		}
		inline Vector2 operator*(const Real fScalar) const
		{
			return Vector2(	x * fScalar,
							y * fScalar);
		}
		static void Multiply(Vector2& results, const Vector2& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x * fScalar;
			results.y = leftOperandVector.y * fScalar;
		}
		inline Vector2 operator*(const Vector2& rhs) const
		{
			return Vector2(	x * rhs.x,
							y * rhs.y);
		}
		static void Multiply(Vector2& results, const Vector2& leftOperandVector, const Vector2& rightOperandVector)
		{
			results.x = leftOperandVector.x * rightOperandVector.x;
			results.y = leftOperandVector.y * rightOperandVector.y;
		}
		inline Vector2 operator/(const Real fScalar) const
		{
			assert(fScalar != 0.0);

			Real fInv = 1.0 / fScalar;

			return Vector2(	x * fInv,
							y * fInv);
		}
		static void Divide(Vector2& results, const Vector2& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x / fScalar;
			results.y = leftOperandVector.y / fScalar;
		}
		inline Vector2 operator/(const Vector2& rhs) const
		{
			return Vector2(	x / rhs.x,
							y / rhs.y);
		}
		static void Divide(Vector2& results, const Vector2& leftOperandVector, const Vector2& rightOperandVector)
		{
			results.x = leftOperandVector.x / rightOperandVector.x;
			results.y = leftOperandVector.y / rightOperandVector.y;
		}
		inline const Vector2& operator+() const
		{
			return *this;
		}

		inline Vector2 operator-() const
		{
			return Vector2(-x, -y);
		}

		// overloaded operators to help Vector2
		inline friend Vector2 operator*(const Real fScalar, const Vector2& rkVector)
		{
			return Vector2(	fScalar * rkVector.x,
							fScalar * rkVector.y);
		}
		static void Multiply(Vector2& results, const Real fScalar, const Vector2& rightOperandVector)
		{
			results.x = rightOperandVector.x * fScalar;
			results.y = rightOperandVector.y * fScalar;
		}
		inline friend Vector2 operator/(const Real fScalar, const Vector2& rkVector)
		{
			return Vector2(	fScalar / rkVector.x,
							fScalar / rkVector.y);
		}
		static void Divide(Vector2& results, const Real fScalar, const Vector2& rightOperandVector)
		{
			results.x = rightOperandVector.x / fScalar;
			results.y = rightOperandVector.y / fScalar;
		}
		inline friend Vector2 operator+(const Vector2& lhs, const Real rhs)
		{
			return Vector2(
					lhs.x + rhs,
					lhs.y + rhs);
		}

		inline friend Vector2 operator+(const Real lhs, const Vector2& rhs)
		{
			return Vector2(	lhs + rhs.x,
							lhs + rhs.y);
		}

		inline friend Vector2 operator-(const Vector2& lhs, const Real rhs)
		{
			return Vector2(	lhs.x - rhs,
							lhs.y - rhs);
		}

		inline friend Vector2 operator-(const Real lhs, const Vector2& rhs)
		{
			return Vector2(	lhs - rhs.x,
							lhs - rhs.y);
		}
		// arithmetic updates
		inline Vector2& operator+=(const Vector2& rkVector)
		{
			x += rkVector.x;
			y += rkVector.y;

			return *this;
		}

		inline Vector2& operator+=(const Real fScaler)
		{
			x += fScaler;
			y += fScaler;

			return *this;
		}

		inline Vector2& operator-=(const Vector2& rkVector)
		{
			x -= rkVector.x;
			y -= rkVector.y;

			return *this;
		}

		inline Vector2& operator-=(const Real fScaler)
		{
			x -= fScaler;
			y -= fScaler;

			return *this;
		}

		inline Vector2& operator*=(const Real fScalar)
		{
			x *= fScalar;
			y *= fScalar;

			return *this;
		}

		inline Vector2& operator*=(const Vector2& rkVector)
		{
			x *= rkVector.x;
			y *= rkVector.y;

			return *this;
		}

		inline Vector2& operator/=(const Real fScalar)
		{
			assert( fScalar != 0.0 );

			Real fInv = 1.0 / fScalar;

			x *= fInv;
			y *= fInv;

			return *this;
		}

		inline Vector2& operator/=(const Vector2& rkVector)
		{
			x /= rkVector.x;
			y /= rkVector.y;

			return *this;
		}

		/** 
		 * Returns the length (magnitude) of the vector.
		 *   \warning
		 *       This operation requires a square root and is expensive in
		 *       terms of CPU operations. If you don't need to know the exact
		 *       length (e.g. for just comparing lengths) use squaredLength()
		 *       instead.
		 */
		inline Real Length() const
		{
			return GenericMath::Sqrt(x * x + y * y);
		}

		/** 
		 * Returns the square of the length(magnitude) of the vector.
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
			return x * x + y * y;
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
		inline Real DotProduct(const Vector2& vec) const
		{
			return x * vec.x + y * vec.y;
		}

		/** Normalises the vector.
		 *   \remarks
		 *       This method normalises the vector such that it's
		 *       length / magnitude is 1. The result is called a unit vector.
		 *   \note
		 *       This function will not crash for zero-sized vectors, but there
		 *       will be no changes made to their components.
		 *   \returns The previous length of the vector.
		 */
		inline Real Normalise()
		{
			Real fLength = GenericMath::Sqrt(x * x + y * y);

			// Will also work for zero-sized vectors, but will change nothing
			if (fLength > 1e-08)
			{
				Real fInvLength = 1.0 / fLength;
				x *= fInvLength;
				y *= fInvLength;
			}
			return fLength;
		}

		/** 
		 *  Returns a vector at a point half way between this and the passed
		 *   in vector.
		 */
		inline Vector2 MidPoint(const Vector2& vec) const
		{
			return Vector2(
					(x + vec.x) * 0.5,
					(y + vec.y) * 0.5 );
		}

		/** 
		 * Returns true if the vector's scalar components are all greater
		 *   that the ones of the vector it is compared against.
		 */
		inline bool operator<(const Vector2& rhs) const
		{
			if(x < rhs.x && y < rhs.y)
				return true;
			return false;
		}

		/** 
		 *  Returns true if the vector's scalar components are all smaller
		 *  that the ones of the vector it is compared against.
		 */
		inline bool operator>(const Vector2& rhs) const
		{
			if(x > rhs.x && y > rhs.y)
				return true;
			return false;
		}

		/** 
		 * Sets this vector's components to the minimum of its own and the
		 * ones of the passed in vector.
		 *   \remarks
		 *       'Minimum' in this case means the combination of the lowest
		 *       value of x, y and z from both vectors. Lowest is taken just
		 *       numerically, not magnitude, so -1 < 0.
		 */
		inline void MakeFloor(const Vector2& cmp )
		{
			if(cmp.x < x) x = cmp.x;
			if(cmp.y < y) y = cmp.y;
		}

		/** 
		 * Sets this vector's components to the maximum of its own and the
		 * ones of the passed in vector.
		 *   \remarks
		 *       'Maximum' in this case means the combination of the highest
		 *       value of x, y and z from both vectors. Highest is taken just
		 *       numerically, not magnitude, so 1 > -3.
		 */
		inline void MakeCeil(const Vector2& cmp)
		{
			if(cmp.x > x) x = cmp.x;
			if(cmp.y > y) y = cmp.y;
		}

		/** 
		 * Generates a vector perpendicular to this vector (eg an 'up' vector).
		 *   \remarks
		 *       This method will return a vector which is perpendicular to this
		 *       vector. There are an infinite number of possibilities but this
		 *       method will guarantee to generate one of them. If you need more
		 *       control you should use the Quaternion class.
		 */
		inline Vector2 Perpendicular(void) const
		{
			return Vector2(-y, x);
		}
        /** 
		 * Calculates the 2 dimensional cross-product of 2 vectors, which results
		 *	in a single floating point value which is 2 times the area of the triangle.
         */
        inline Real CrossProduct(const Vector2& rkVector) const
        {
            return x * rkVector.y - y * rkVector.x;
        }
        /** Generates a new random vector which deviates from this vector by a
         *   given angle in a random direction.
         *   \remarks
         *       This method assumes that the random number generator has already
         *       been seeded appropriately.
         *   \param
         *       angle The angle at which to deviate in radians
         *   \returns
         *       A random vector which deviates from this vector by angle. This
         *       vector will not be normalised, normalise it if you wish
         *       afterwards.
         */
        inline Vector2 RandomDeviant(Real angle) const
        {
			throw std::exception("Math Not implemented");
            /*angle *=  GenericMath::UnitRandom() * GenericMath::TWO_PI;
            Real cosa = cos(angle);
            Real sina = sin(angle);
            return  Vector2(cosa * x - sina * y,
                            sina * x + cosa * y);*/
        }

        /** Returns true if this vector is zero length. */
        inline bool IsZeroLength(void) const
        {
            Real sqlen = (x * x) + (y * y);
            return (sqlen < (1e-06 * 1e-06));

        }

        /** As normalise, except that this vector is unaffected and the
            normalised vector is returned as a copy. */
        inline Vector2 NormalisedCopy(void) const
        {
            Vector2 ret = *this;
            ret.Normalise();
            return ret;
        }

        /** 
		 * Calculates a reflection vector to the Plane with the given normal .
         * \remarks 
		 *     NB assumes 'this' is pointing AWAY FROM the Plane, invert if it is not.
         */
        inline Vector2 Reflect(const Vector2& normal) const
        {
            return Vector2(*this-(2*this->DotProduct(normal)*normal));
        }

        // special points
        static const Vector2 ZERO;
        static const Vector2 UNIT_X;
        static const Vector2 UNIT_Y;
        static const Vector2 NEGATIVE_UNIT_X;
        static const Vector2 NEGATIVE_UNIT_Y;
        static const Vector2 UNIT_SCALE;

        /** Function for writing to a stream.
        */
        inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Vector2& v)
        {
            o << "Vector2(" << v.x << ", " << v.y <<  ")";
            return o;
        }
    };

END_ENGINE_NAMESPACE

#endif