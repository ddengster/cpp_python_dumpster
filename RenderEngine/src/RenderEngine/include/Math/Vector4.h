
#ifndef _VECTOR4_H
#define _VECTOR4_H

#include "Prerequisites.h"

DECLARE_ENGINE_NAMESPACE

/** \ingroup Maths
 * 4-dimensional homogenous vector.
 */
class /*shared*/ Vector4
{
	public:
		Real x, y, z, w;

	public:
		/** 
		 * Default constructor.
		 *   \note
		 *       It does <b>NOT</b> initialize the vector for efficiency.
		 */
		inline Vector4()
		{
		}
		/**
		 * Constructor using Real numbers as the default value of its elements.
		 *
		 * \param fX		Value of element x
		 * \param fY		Value of element y
		 * \param fZ		Value of element z
		 * \param fW		Value of element w
		 */
		inline Vector4(const Real fX, const Real fY, const Real fZ, const Real fW)
			: x(fX), y(fY), z(fZ), w(fW)
			{
			}
		/**
		 * Constructor using an array of four Real numbers as the default value of its elements.
		 *
		 * \param afCoordinate		Value of element x, y, z, w respectively
		 */
		inline explicit Vector4(const Real afCoordinate[4])
			: x(afCoordinate[0]),
		y(afCoordinate[1]),
		z(afCoordinate[2]),
		w(afCoordinate[3])
		{
		}
		/**
		 * Constructor using pointer to an array of four Real numbers as the default value of its elements.
		 *
		 * \param r					Array of elements x, y, z, w
		 */
		inline explicit Vector4(Real* const r)
			: x(r[0]), y(r[1]), z(r[2]), w(r[3])
			{
			}
		/**
		 * Constructor using a single Real number as the default value of its elements.
		 *
		 * \param scaler		Value of element x, y, z, w
		 */
		inline explicit Vector4(const Real scaler)
			: x(scaler)
			, y(scaler)
			, z(scaler)
			, w(scaler)
			{
			}
		/**
		 * Constructor using another vector3 as its defualt value, and set w = 1.0f.
		 *
		 * \param rhs			 The other vector
		 */
		inline explicit Vector4(const Vector3& rhs)
			: x(rhs.x), y(rhs.y), z(rhs.z), w(1.0f)
			{
			}
		/**
		 * Constructor using another vector as its defualt value.
		 *
		 * \param rkVector			The other vector
		 */
		inline Vector4(const Vector4& rkVector)
			: x(rkVector.x), y(rkVector.y), z(rkVector.z), w (rkVector.w)
			{
			}
		/// Operator to access x or y element of this vector
		inline Real operator[](const uint32_t i) const
		{
			assert(i < 4);

			return *(&x+i);
		}

		inline Real& operator[](const uint32_t i)
		{
			assert(i < 4);

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
		 * Assigns the value of the other vector.
		 *   \param
		 *       rkVector The other vector
		 */
		inline Vector4& operator=(const Vector4& rkVector)
		{
			x = rkVector.x;
			y = rkVector.y;
			z = rkVector.z;
			w = rkVector.w;

			return *this;
		}

		inline Vector4& operator=(const Real fScalar)
		{
			x = fScalar;
			y = fScalar;
			z = fScalar;
			w = fScalar;
			return *this;
		}

		inline bool operator==(const Vector4& rkVector) const
		{
			return (x == rkVector.x &&
					y == rkVector.y &&
					z == rkVector.z &&
					w == rkVector.w);
		}
		bool IsEqual(const Vector4& rkVector)
		{
			return (x == rkVector.x &&
					y == rkVector.y &&
					z == rkVector.z &&
					w == rkVector.w);
		}
		inline bool operator!=(const Vector4& rkVector) const
		{
			return (x != rkVector.x ||
					y != rkVector.y ||
					z != rkVector.z ||
					w != rkVector.w);
		}
		bool NotEqual(const Vector4& rkVector)
		{
			return (x != rkVector.x ||
					y != rkVector.y ||
					z != rkVector.z ||
					w != rkVector.w);
		}
		inline Vector4& operator=(const Vector3& rhs)
		{
			x = rhs.x;
			y = rhs.y;
			z = rhs.z;
			w = 1.0f;
			return *this;
		}

		// arithmetic operations
		inline Vector4 operator+(const Vector4& rkVector) const
		{
			return Vector4(
					x + rkVector.x,
					y + rkVector.y,
					z + rkVector.z,
					w + rkVector.w);
		}
		static void Add(Vector4& results, const Vector4& leftOperandVector, const Vector4& rightOperandVector)
		{
			results.x = leftOperandVector.x + rightOperandVector.x;
			results.y = leftOperandVector.y + rightOperandVector.y;
			results.z = leftOperandVector.z + rightOperandVector.z;
			results.w = leftOperandVector.w + rightOperandVector.w;
		}
		inline Vector4 operator-(const Vector4& rkVector) const
		{
			return Vector4(
					x - rkVector.x,
					y - rkVector.y,
					z - rkVector.z,
					w - rkVector.w);
		}
		static void Subtract(Vector4& results, const Vector4& leftOperandVector, const Vector4& rightOperandVector)
		{
			results.x = leftOperandVector.x - rightOperandVector.x;
			results.y = leftOperandVector.y - rightOperandVector.y;
			results.z = leftOperandVector.z - rightOperandVector.z;
			results.w = leftOperandVector.w - rightOperandVector.w;
		}
		inline Vector4 operator*(const Real fScalar) const
		{
			return Vector4(
					x * fScalar,
					y * fScalar,
					z * fScalar,
					w * fScalar);
		}
		static void Multiply(Vector4& results, const Vector4& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x * fScalar;
			results.y = leftOperandVector.y * fScalar;
			results.z = leftOperandVector.z * fScalar;
			results.w = leftOperandVector.w * fScalar;
		}
		inline Vector4 operator*(const Vector4& rhs) const
		{
			return Vector4(
					rhs.x * x,
					rhs.y * y,
					rhs.z * z,
					rhs.w * w);
		}
		static void Multiply(Vector4& results, const Vector4& leftOperandVector, const Vector4& rightOperandVector)
		{
			results.x = leftOperandVector.x * rightOperandVector.x;
			results.y = leftOperandVector.y * rightOperandVector.y;
			results.z = leftOperandVector.z * rightOperandVector.z;
			results.w = leftOperandVector.w * rightOperandVector.w;
		}
		inline Vector4 operator/(const Real fScalar) const
		{
			assert(fScalar != 0.0);

			Real fInv = 1.0 / fScalar;

			return Vector4(
					x * fInv,
					y * fInv,
					z * fInv,
					w * fInv);
		}
		static void Divide(Vector4& results, const Vector4& leftOperandVector, const Real fScalar)
		{
			results.x = leftOperandVector.x / fScalar;
			results.y = leftOperandVector.y / fScalar;
			results.z = leftOperandVector.z / fScalar;
			results.w = leftOperandVector.w / fScalar;
		}
		inline Vector4 operator/(const Vector4& rhs) const
		{
			return Vector4(
					x / rhs.x,
					y / rhs.y,
					z / rhs.z,
					w / rhs.w);
		}
		static void Divide(Vector4& results, const Vector4& leftOperandVector, const Vector4& rightOperandVector)
		{
			results.x = leftOperandVector.x / rightOperandVector.x;
			results.y = leftOperandVector.y / rightOperandVector.y;
			results.z = leftOperandVector.z / rightOperandVector.z;
			results.w = leftOperandVector.w / rightOperandVector.w;
		}
		inline const Vector4& operator+() const
		{
			return *this;
		}

		inline Vector4 operator-() const
		{
			return Vector4(-x, -y, -z, -w);
		}

		inline friend Vector4 operator*(const Real fScalar, const Vector4& rkVector)
		{
			return Vector4(
					fScalar * rkVector.x,
					fScalar * rkVector.y,
					fScalar * rkVector.z,
					fScalar * rkVector.w);
		}
		static void Multiply(Vector4& results, const Real fScalar, const Vector4& rightOperandVector)
		{
			results.x = rightOperandVector.x * fScalar;
			results.y = rightOperandVector.y * fScalar;
			results.z = rightOperandVector.z * fScalar;
			results.w = rightOperandVector.w * fScalar;
		}
		inline friend Vector4 operator/(const Real fScalar, const Vector4& rkVector)
		{
			return Vector4(
					fScalar / rkVector.x,
					fScalar / rkVector.y,
					fScalar / rkVector.z,
					fScalar / rkVector.w);
		}
		static void Divide(Vector4& results, const Real fScalar, const Vector4& rightOperandVector)
		{
			results.x = rightOperandVector.x / fScalar;
			results.y = rightOperandVector.y / fScalar;
			results.z = rightOperandVector.z / fScalar;
			results.w = rightOperandVector.w / fScalar;
		}
		inline friend Vector4 operator+(const Vector4& lhs, const Real rhs)
		{
			return Vector4(
					lhs.x + rhs,
					lhs.y + rhs,
					lhs.z + rhs,
					lhs.w + rhs);
		}

		inline friend Vector4 operator+(const Real lhs, const Vector4& rhs)
		{
			return Vector4(
					lhs + rhs.x,
					lhs + rhs.y,
					lhs + rhs.z,
					lhs + rhs.w);
		}

		inline friend Vector4 operator-(const Vector4& lhs, Real rhs)
		{
			return Vector4(
					lhs.x - rhs,
					lhs.y - rhs,
					lhs.z - rhs,
					lhs.w - rhs);
		}

		inline friend Vector4 operator-(const Real lhs, const Vector4& rhs)
		{
			return Vector4(
					lhs - rhs.x,
					lhs - rhs.y,
					lhs - rhs.z,
					lhs - rhs.w);
		}

		// arithmetic updates
		inline Vector4& operator+=(const Vector4& rkVector)
		{
			x += rkVector.x;
			y += rkVector.y;
			z += rkVector.z;
			w += rkVector.w;

			return *this;
		}

		inline Vector4& operator-=(const Vector4& rkVector)
		{
			x -= rkVector.x;
			y -= rkVector.y;
			z -= rkVector.z;
			w -= rkVector.w;

			return *this;
		}

		inline Vector4& operator*=(const Real fScalar)
		{
			x *= fScalar;
			y *= fScalar;
			z *= fScalar;
			w *= fScalar;
			return *this;
		}

		inline Vector4& operator+=(const Real fScalar)
		{
			x += fScalar;
			y += fScalar;
			z += fScalar;
			w += fScalar;
			return *this;
		}

		inline Vector4& operator-=(const Real fScalar)
		{
			x -= fScalar;
			y -= fScalar;
			z -= fScalar;
			w -= fScalar;
			return *this;
		}

		inline Vector4& operator*=(const Vector4& rkVector)
		{
			x *= rkVector.x;
			y *= rkVector.y;
			z *= rkVector.z;
			w *= rkVector.w;

			return *this;
		}

		inline Vector4& operator/=(const Real fScalar)
		{
			assert( fScalar != 0.0 );

			Real fInv = 1.0 / fScalar;

			x *= fInv;
			y *= fInv;
			z *= fInv;
			w *= fInv;

			return *this;
		}

		inline Vector4& operator/=(const Vector4& rkVector)
		{
			x /= rkVector.x;
			y /= rkVector.y;
			z /= rkVector.z;
			w /= rkVector.w;

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
			return GenericMath::Sqrt(x * x + y * y + z * z + w * w);
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
			return x * x + y * y + z * z + w * w;
		}

		/** 
		 * Calculates the dot (scalar) product of this vector with another.
		 *   \param
		 *       vec Vector with which to calculate the dot product (together
		 *       with this one).
		 *   \returns
		 *       A float representing the dot product value.
		 */
		inline Real DotProduct(const Vector4& vec) const
		{
			return x * vec.x + y * vec.y + z * vec.z + w * vec.w;
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
			Real fLength = GenericMath::Sqrt(x * x + y * y + z * z + w * w);

			// Will also work for zero-sized vectors, but will change nothing
			if (fLength > 1e-08)
			{
				Real fInvLength = 1.0 / fLength;
				x *= fInvLength;
				y *= fInvLength;
				z *= fInvLength;
				w *= fInvLength;
			}

			return fLength;
		}
		/** 
		 * Function for writing to a stream.
		 */
		inline /*shared*/ friend std::ostream& operator <<
			( std::ostream& o, const Vector4& v )
			{
				o << "Vector4(" << v.x << ", " << v.y << ", " << v.z << ", " << v.w << ")";
				return o;
			}
		// special
		static const Vector4 ZERO;
};

END_ENGINE_NAMESPACE

#endif
