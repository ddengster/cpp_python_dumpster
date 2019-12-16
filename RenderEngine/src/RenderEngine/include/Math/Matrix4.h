
#ifndef _MATRIX4_H
#define _MATRIX4_H

#include "Prerequisites.h"

#include "Math/Matrix3.h"
#include "Math/Vector3.h"
#include "Math/Vector4.h"
#include "Math/Plane.h"
#include "Math/Quaternion.h"

DECLARE_ENGINE_NAMESPACE

/** \ingroup Maths
Class encapsulating a standard 4x4 homogenous matrix.
  @remarks
  M-Engine deals with the differences between D3D and OpenGL 
  internally when operating through different render systems. Users
  only need to conform to standard maths conventions, i.e. right-to-left 
  matrix multiplication, matrics passed to D3D is transposed
  to compensate.
  @par
  The generic form M * V which shows the layout of the matrix 
  entries is shown below:
  <pre>
  [ m[0][0]  m[0][1]  m[0][2]  m[0][3] ]   {x}
  | m[1][0]  m[1][1]  m[1][2]  m[1][3] | * {y}
  | m[2][0]  m[2][1]  m[2][2]  m[2][3] |   {z}
  [ m[3][0]  m[3][1]  m[3][2]  m[3][3] ]   {1}
  </pre>
 */
class /*shared*/ Matrix4
{
	public: //protected:
		/// The matrix entries, indexed by [row][col].
		union {
			Real m[4][4];
			Real _m[16];
		};
	public:
		/** Default constructor.
		 *   \note
		 *       It does <b>NOT</b> initialize the matrix for efficiency.
		 */
		inline Matrix4()
		{
		}
		/**
		 * Constructor using Real numbers as the default value of its elements.
		 *
		 * m00	-	Value of element 0,0
		 * m33	-	Value of element 3,3
		 */
		inline Matrix4(
				Real m00, Real m01, Real m02, Real m03,
				Real m10, Real m11, Real m12, Real m13,
				Real m20, Real m21, Real m22, Real m23,
				Real m30, Real m31, Real m32, Real m33)
		{
			m[0][0] = m00;
			m[0][1] = m01;
			m[0][2] = m02;
			m[0][3] = m03;
			m[1][0] = m10;
			m[1][1] = m11;
			m[1][2] = m12;
			m[1][3] = m13;
			m[2][0] = m20;
			m[2][1] = m21;
			m[2][2] = m22;
			m[2][3] = m23;
			m[3][0] = m30;
			m[3][1] = m31;
			m[3][2] = m32;
			m[3][3] = m33;
		}

		/** 
		 * Creates a standard 4x4 transformation matrix with a zero translation part from a 
		 * rotation/scaling 3x3 matrix.
		 *
		 * \param m3x3				The Matrix3 rotation/scaling matrix
		 */
		inline Matrix4(const Matrix3& m3x3)
		{
			operator=(IDENTITY);
			operator=(m3x3);
		}
		/** 
		 * Creates a standard 4x4 transformation matrix with a zero translation part from a 
		 * Quaternion.
		 *
		 * \param rot				Quaternion to convert to Matrix4
		 */
		inline Matrix4(const Quaternion& rot)
		{
			Matrix3 m3x3;
			rot.ToRotationMatrix(m3x3);
			operator=(IDENTITY);
			operator=(m3x3);
		}

		inline Real* operator[](uint32_t iRow)
		{
			/*assert( iRow < 4 );*/
			return m[iRow];
		}

		inline const Real *const operator[](uint32_t iRow) const
		{
			/*assert( iRow < 4 );*/
			return m[iRow];
		}
		/** 
		 * Get the cross product of this matrix and another matrix.
		 *
		 * \param m2				Matrix4 to cross multiply with
		 */
		inline Matrix4 Concatenate(const Matrix4 &m2) const
		{
			Matrix4 r;
			r.m[0][0] = m[0][0] * m2.m[0][0] + m[0][1] * m2.m[1][0] + m[0][2] * m2.m[2][0] + m[0][3] * m2.m[3][0];
			r.m[0][1] = m[0][0] * m2.m[0][1] + m[0][1] * m2.m[1][1] + m[0][2] * m2.m[2][1] + m[0][3] * m2.m[3][1];
			r.m[0][2] = m[0][0] * m2.m[0][2] + m[0][1] * m2.m[1][2] + m[0][2] * m2.m[2][2] + m[0][3] * m2.m[3][2];
			r.m[0][3] = m[0][0] * m2.m[0][3] + m[0][1] * m2.m[1][3] + m[0][2] * m2.m[2][3] + m[0][3] * m2.m[3][3];

			r.m[1][0] = m[1][0] * m2.m[0][0] + m[1][1] * m2.m[1][0] + m[1][2] * m2.m[2][0] + m[1][3] * m2.m[3][0];
			r.m[1][1] = m[1][0] * m2.m[0][1] + m[1][1] * m2.m[1][1] + m[1][2] * m2.m[2][1] + m[1][3] * m2.m[3][1];
			r.m[1][2] = m[1][0] * m2.m[0][2] + m[1][1] * m2.m[1][2] + m[1][2] * m2.m[2][2] + m[1][3] * m2.m[3][2];
			r.m[1][3] = m[1][0] * m2.m[0][3] + m[1][1] * m2.m[1][3] + m[1][2] * m2.m[2][3] + m[1][3] * m2.m[3][3];

			r.m[2][0] = m[2][0] * m2.m[0][0] + m[2][1] * m2.m[1][0] + m[2][2] * m2.m[2][0] + m[2][3] * m2.m[3][0];
			r.m[2][1] = m[2][0] * m2.m[0][1] + m[2][1] * m2.m[1][1] + m[2][2] * m2.m[2][1] + m[2][3] * m2.m[3][1];
			r.m[2][2] = m[2][0] * m2.m[0][2] + m[2][1] * m2.m[1][2] + m[2][2] * m2.m[2][2] + m[2][3] * m2.m[3][2];
			r.m[2][3] = m[2][0] * m2.m[0][3] + m[2][1] * m2.m[1][3] + m[2][2] * m2.m[2][3] + m[2][3] * m2.m[3][3];

			r.m[3][0] = m[3][0] * m2.m[0][0] + m[3][1] * m2.m[1][0] + m[3][2] * m2.m[2][0] + m[3][3] * m2.m[3][0];
			r.m[3][1] = m[3][0] * m2.m[0][1] + m[3][1] * m2.m[1][1] + m[3][2] * m2.m[2][1] + m[3][3] * m2.m[3][1];
			r.m[3][2] = m[3][0] * m2.m[0][2] + m[3][1] * m2.m[1][2] + m[3][2] * m2.m[2][2] + m[3][3] * m2.m[3][2];
			r.m[3][3] = m[3][0] * m2.m[0][3] + m[3][1] * m2.m[1][3] + m[3][2] * m2.m[2][3] + m[3][3] * m2.m[3][3];

			return r;
		}

		/** 
		 * Matrix concatenation using '*'.
		 */
		inline Matrix4 operator*(const Matrix4 &m2) const
		{
			return Concatenate(m2);
		}

		/** 
		 * Vector transformation using '*'.
		 * \remarks
		 *       Transforms the given 3-D vector by the matrix, projecting the 
		 *       result back into <i>w</i> = 1.
		 * 
		 * \note
		 *       This means that the initial <i>w</i> is considered to be 1.0,
		 *       and then all the tree elements of the resulting 3-D vector are
		 *       divided by the resulting <i>w</i>.
		 */
		inline Vector3 operator*(const Vector3& v) const
		{
			Vector3 r;

			Real fInvW = 1.0 / ( m[3][0] * v.x + m[3][1] * v.y + m[3][2] * v.z + m[3][3] );

			r.x = ( m[0][0] * v.x + m[0][1] * v.y + m[0][2] * v.z + m[0][3] ) * fInvW;
			r.y = ( m[1][0] * v.x + m[1][1] * v.y + m[1][2] * v.z + m[1][3] ) * fInvW;
			r.z = ( m[2][0] * v.x + m[2][1] * v.y + m[2][2] * v.z + m[2][3] ) * fInvW;

			return r;
		}
		/**
		 * Muliplication function for a Matrix4 and a Vector3.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param results				Variable to hold the results of this operation
		 * \param rkMatrix				Matrix operand
		 * \param v						Vector operand
		 */
		static void Concatenate(Vector3& results, const Matrix4& rkMatrix, const Vector3& v)
		{
			Real fInvW = 1.0 / ( rkMatrix[3][0] * v.x + rkMatrix[3][1] * v.y + rkMatrix[3][2] * v.z + rkMatrix[3][3] );

			results.x = ( rkMatrix[0][0] * v.x + rkMatrix[0][1] * v.y + rkMatrix[0][2] * v.z + rkMatrix[0][3] ) * fInvW;
			results.y = ( rkMatrix[1][0] * v.x + rkMatrix[1][1] * v.y + rkMatrix[1][2] * v.z + rkMatrix[1][3] ) * fInvW;
			results.z = ( rkMatrix[2][0] * v.x + rkMatrix[2][1] * v.y + rkMatrix[2][2] * v.z + rkMatrix[2][3] ) * fInvW;
		}
		/** 
		 * Vector transformation using '*'.
		 */
		inline Vector4 operator*(const Vector4& v) const
		{
			return Vector4(
					m[0][0] * v.x + m[0][1] * v.y + m[0][2] * v.z + m[0][3] * v.w, 
					m[1][0] * v.x + m[1][1] * v.y + m[1][2] * v.z + m[1][3] * v.w,
					m[2][0] * v.x + m[2][1] * v.y + m[2][2] * v.z + m[2][3] * v.w,
					m[3][0] * v.x + m[3][1] * v.y + m[3][2] * v.z + m[3][3] * v.w
					   );
		}
		/**
		 * Muliplication function for a Matrix4 and a Vector4.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param results				Variable to hold the results of this operation
		 * \param rkMatrix				Matrix operand
		 * \param v						Vector operand
		 */
		static void Concatenate(Vector4& results, const Matrix4& rkMatrix, const Vector4& v)
		{
			results.x = rkMatrix.m[0][0] * v.x + rkMatrix.m[0][1] * v.y + rkMatrix.m[0][2] * v.z + rkMatrix.m[0][3] * v.w; 
			results.y = rkMatrix.m[1][0] * v.x + rkMatrix.m[1][1] * v.y + rkMatrix.m[1][2] * v.z + rkMatrix.m[1][3] * v.w;
			results.z = rkMatrix.m[2][0] * v.x + rkMatrix.m[2][1] * v.y + rkMatrix.m[2][2] * v.z + rkMatrix.m[2][3] * v.w;
			results.w = rkMatrix.m[3][0] * v.x + rkMatrix.m[3][1] * v.y + rkMatrix.m[3][2] * v.z + rkMatrix.m[3][3] * v.w;
		}
		/** 
		 * Plane transformation using '*'.
		 */
		inline Plane operator*(const Plane& p) const
		{
			Plane ret;
			Matrix4 invTrans = Inverse().Transpose();
			Vector4 v4( p.normal.x, p.normal.y, p.normal.z, p.d );
			v4 = invTrans * v4;
			ret.normal.x = v4.x; 
			ret.normal.y = v4.y; 
			ret.normal.z = v4.z;
			ret.d = v4.w / ret.normal.Normalise();

			return ret;
		}
		/**
		 * Muliplication function for a Matrix4 and a Plane.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param results				Variable to hold the results of this operation
		 * \param rkMatrix				Matrix operand
		 * \param p						Plane operand
		 */
		static void Concatenate(Plane& results, const Matrix4& rkMatrix, const Plane& p)
		{
			Matrix4 invTrans = (rkMatrix.Inverse()).Transpose();
			Vector4 v4( p.normal.x, p.normal.y, p.normal.z, p.d );
			v4 = invTrans * v4;
			results.normal.x = v4.x; 
			results.normal.y = v4.y; 
			results.normal.z = v4.z;
			results.d = v4.w / results.normal.Normalise();
		}

		/** 
		 * Matrix addition using '+'.
		 */
		inline Matrix4 operator + (const Matrix4 &m2) const
		{
			Matrix4 r;

			r.m[0][0] = m[0][0] + m2.m[0][0];
			r.m[0][1] = m[0][1] + m2.m[0][1];
			r.m[0][2] = m[0][2] + m2.m[0][2];
			r.m[0][3] = m[0][3] + m2.m[0][3];

			r.m[1][0] = m[1][0] + m2.m[1][0];
			r.m[1][1] = m[1][1] + m2.m[1][1];
			r.m[1][2] = m[1][2] + m2.m[1][2];
			r.m[1][3] = m[1][3] + m2.m[1][3];

			r.m[2][0] = m[2][0] + m2.m[2][0];
			r.m[2][1] = m[2][1] + m2.m[2][1];
			r.m[2][2] = m[2][2] + m2.m[2][2];
			r.m[2][3] = m[2][3] + m2.m[2][3];

			r.m[3][0] = m[3][0] + m2.m[3][0];
			r.m[3][1] = m[3][1] + m2.m[3][1];
			r.m[3][2] = m[3][2] + m2.m[3][2];
			r.m[3][3] = m[3][3] + m2.m[3][3];

			return r;
		}
		/** 
		 * Matrix addition using function.
		 */
		inline Matrix4 Add(Matrix4& results, const Matrix4& m2) const
		{
			results.m[0][0] = m[0][0] + m2.m[0][0];
			results.m[0][1] = m[0][1] + m2.m[0][1];
			results.m[0][2] = m[0][2] + m2.m[0][2];
			results.m[0][3] = m[0][3] + m2.m[0][3];

			results.m[1][0] = m[1][0] + m2.m[1][0];
			results.m[1][1] = m[1][1] + m2.m[1][1];
			results.m[1][2] = m[1][2] + m2.m[1][2];
			results.m[1][3] = m[1][3] + m2.m[1][3];

			results.m[2][0] = m[2][0] + m2.m[2][0];
			results.m[2][1] = m[2][1] + m2.m[2][1];
			results.m[2][2] = m[2][2] + m2.m[2][2];
			results.m[2][3] = m[2][3] + m2.m[2][3];

			results.m[3][0] = m[3][0] + m2.m[3][0];
			results.m[3][1] = m[3][1] + m2.m[3][1];
			results.m[3][2] = m[3][2] + m2.m[3][2];
			results.m[3][3] = m[3][3] + m2.m[3][3];
		}

		/** 
		 * Matrix subtraction using '-'.
		 */
		inline Matrix4 operator-(const Matrix4 &m2) const
		{
			Matrix4 r;
			r.m[0][0] = m[0][0] - m2.m[0][0];
			r.m[0][1] = m[0][1] - m2.m[0][1];
			r.m[0][2] = m[0][2] - m2.m[0][2];
			r.m[0][3] = m[0][3] - m2.m[0][3];

			r.m[1][0] = m[1][0] - m2.m[1][0];
			r.m[1][1] = m[1][1] - m2.m[1][1];
			r.m[1][2] = m[1][2] - m2.m[1][2];
			r.m[1][3] = m[1][3] - m2.m[1][3];

			r.m[2][0] = m[2][0] - m2.m[2][0];
			r.m[2][1] = m[2][1] - m2.m[2][1];
			r.m[2][2] = m[2][2] - m2.m[2][2];
			r.m[2][3] = m[2][3] - m2.m[2][3];

			r.m[3][0] = m[3][0] - m2.m[3][0];
			r.m[3][1] = m[3][1] - m2.m[3][1];
			r.m[3][2] = m[3][2] - m2.m[3][2];
			r.m[3][3] = m[3][3] - m2.m[3][3];

			return r;
		}
		/** 
		 * Matrix subtraction using function.
		 */
		inline Matrix4 Subtract(Matrix4& results, const Matrix4& m2) const
		{
			results.m[0][0] = m[0][0] - m2.m[0][0];
			results.m[0][1] = m[0][1] - m2.m[0][1];
			results.m[0][2] = m[0][2] - m2.m[0][2];
			results.m[0][3] = m[0][3] - m2.m[0][3];

			results.m[1][0] = m[1][0] - m2.m[1][0];
			results.m[1][1] = m[1][1] - m2.m[1][1];
			results.m[1][2] = m[1][2] - m2.m[1][2];
			results.m[1][3] = m[1][3] - m2.m[1][3];

			results.m[2][0] = m[2][0] - m2.m[2][0];
			results.m[2][1] = m[2][1] - m2.m[2][1];
			results.m[2][2] = m[2][2] - m2.m[2][2];
			results.m[2][3] = m[2][3] - m2.m[2][3];

			results.m[3][0] = m[3][0] - m2.m[3][0];
			results.m[3][1] = m[3][1] - m2.m[3][1];
			results.m[3][2] = m[3][2] - m2.m[3][2];
			results.m[3][3] = m[3][3] - m2.m[3][3];
		}

		/** 
		 * Tests 2 matrices for equality.
		 */
		inline bool operator==(const Matrix4& m2) const
		{
			if( m[0][0] != m2.m[0][0] || m[0][1] != m2.m[0][1] || m[0][2] != m2.m[0][2] || m[0][3] != m2.m[0][3] ||
				m[1][0] != m2.m[1][0] || m[1][1] != m2.m[1][1] || m[1][2] != m2.m[1][2] || m[1][3] != m2.m[1][3] ||
				m[2][0] != m2.m[2][0] || m[2][1] != m2.m[2][1] || m[2][2] != m2.m[2][2] || m[2][3] != m2.m[2][3] ||
				m[3][0] != m2.m[3][0] || m[3][1] != m2.m[3][1] || m[3][2] != m2.m[3][2] || m[3][3] != m2.m[3][3] )
				return false;
			return true;
		}
		/** 
		 * Tests 2 matrices for equality.
		 */
		inline bool IsEquals(const Matrix4& m2) const
		{
			if( m[0][0] != m2.m[0][0] || m[0][1] != m2.m[0][1] || m[0][2] != m2.m[0][2] || m[0][3] != m2.m[0][3] ||
				m[1][0] != m2.m[1][0] || m[1][1] != m2.m[1][1] || m[1][2] != m2.m[1][2] || m[1][3] != m2.m[1][3] ||
				m[2][0] != m2.m[2][0] || m[2][1] != m2.m[2][1] || m[2][2] != m2.m[2][2] || m[2][3] != m2.m[2][3] ||
				m[3][0] != m2.m[3][0] || m[3][1] != m2.m[3][1] || m[3][2] != m2.m[3][2] || m[3][3] != m2.m[3][3] )
				return false;
			return true;
		}

		/** 
		 * Tests 2 matrices for inequality.
		 */
		inline bool operator!=(const Matrix4& m2) const
		{
			if( m[0][0] != m2.m[0][0] || m[0][1] != m2.m[0][1] || m[0][2] != m2.m[0][2] || m[0][3] != m2.m[0][3] ||
				m[1][0] != m2.m[1][0] || m[1][1] != m2.m[1][1] || m[1][2] != m2.m[1][2] || m[1][3] != m2.m[1][3] ||
				m[2][0] != m2.m[2][0] || m[2][1] != m2.m[2][1] || m[2][2] != m2.m[2][2] || m[2][3] != m2.m[2][3] ||
				m[3][0] != m2.m[3][0] || m[3][1] != m2.m[3][1] || m[3][2] != m2.m[3][2] || m[3][3] != m2.m[3][3] )
				return true;
			return false;
		}
		/** 
		 * Tests 2 matrices for inequality.
		 */
		inline bool NotEquals(const Matrix4& m2) const
		{
			if( m[0][0] != m2.m[0][0] || m[0][1] != m2.m[0][1] || m[0][2] != m2.m[0][2] || m[0][3] != m2.m[0][3] ||
				m[1][0] != m2.m[1][0] || m[1][1] != m2.m[1][1] || m[1][2] != m2.m[1][2] || m[1][3] != m2.m[1][3] ||
				m[2][0] != m2.m[2][0] || m[2][1] != m2.m[2][1] || m[2][2] != m2.m[2][2] || m[2][3] != m2.m[2][3] ||
				m[3][0] != m2.m[3][0] || m[3][1] != m2.m[3][1] || m[3][2] != m2.m[3][2] || m[3][3] != m2.m[3][3] )
				return true;
			return false;
		}

		/** 
		 * Assignment from 3x3 matrix to 4x4 matrix.
		 */
		inline void operator=(const Matrix3& mat3)
		{
			m[0][0] = mat3.m[0][0]; m[0][1] = mat3.m[0][1]; m[0][2] = mat3.m[0][2];
			m[1][0] = mat3.m[1][0]; m[1][1] = mat3.m[1][1]; m[1][2] = mat3.m[1][2];
			m[2][0] = mat3.m[2][0]; m[2][1] = mat3.m[2][1]; m[2][2] = mat3.m[2][2];
		}
		/** 
		 * Gets the transpose of this matrix.
		 */
		inline Matrix4 Transpose(void) const
		{
			return Matrix4(	m[0][0], m[1][0], m[2][0], m[3][0],
							m[0][1], m[1][1], m[2][1], m[3][1],
							m[0][2], m[1][2], m[2][2], m[3][2],
							m[0][3], m[1][3], m[2][3], m[3][3]);
		}

		/*
		   -----------------------------------------------------------------------
		   Translation Transformation
		   -----------------------------------------------------------------------
		 */
		/** Sets the translation transformation part of the matrix.
		 */
		inline void SetTrans(const Vector3& v)
		{
			m[0][3] = v.x;
			m[1][3] = v.y;
			m[2][3] = v.z;
		}

		/** Extracts the translation transformation part of the matrix.
		 */
		inline Vector3 GetTrans() const
		{
			return Vector3(m[0][3], m[1][3], m[2][3]);
		}


		/** Builds a translation matrix
		 */
		inline void MakeTrans(const Vector3& v)
		{
			m[0][0] = 1.0; m[0][1] = 0.0; m[0][2] = 0.0; m[0][3] = v.x;
			m[1][0] = 0.0; m[1][1] = 1.0; m[1][2] = 0.0; m[1][3] = v.y;
			m[2][0] = 0.0; m[2][1] = 0.0; m[2][2] = 1.0; m[2][3] = v.z;
			m[3][0] = 0.0; m[3][1] = 0.0; m[3][2] = 0.0; m[3][3] = 1.0;
		}

		inline void MakeTrans(Real tx, Real ty, Real tz)
		{
			m[0][0] = 1.0; m[0][1] = 0.0; m[0][2] = 0.0; m[0][3] = tx;
			m[1][0] = 0.0; m[1][1] = 1.0; m[1][2] = 0.0; m[1][3] = ty;
			m[2][0] = 0.0; m[2][1] = 0.0; m[2][2] = 1.0; m[2][3] = tz;
			m[3][0] = 0.0; m[3][1] = 0.0; m[3][2] = 0.0; m[3][3] = 1.0;
		}

		/** Gets a translation matrix.
		 */
		inline static Matrix4 GetTrans(const Vector3& v)
		{
			Matrix4 r;

			r.m[0][0] = 1.0; r.m[0][1] = 0.0; r.m[0][2] = 0.0; r.m[0][3] = v.x;
			r.m[1][0] = 0.0; r.m[1][1] = 1.0; r.m[1][2] = 0.0; r.m[1][3] = v.y;
			r.m[2][0] = 0.0; r.m[2][1] = 0.0; r.m[2][2] = 1.0; r.m[2][3] = v.z;
			r.m[3][0] = 0.0; r.m[3][1] = 0.0; r.m[3][2] = 0.0; r.m[3][3] = 1.0;

			return r;
        }

        /** Gets a translation matrix - variation for not using a vector.
        */
        inline static Matrix4 GetTrans(Real t_x, Real t_y, Real t_z)
        {
            Matrix4 r;

            r.m[0][0] = 1.0; r.m[0][1] = 0.0; r.m[0][2] = 0.0; r.m[0][3] = t_x;
            r.m[1][0] = 0.0; r.m[1][1] = 1.0; r.m[1][2] = 0.0; r.m[1][3] = t_y;
            r.m[2][0] = 0.0; r.m[2][1] = 0.0; r.m[2][2] = 1.0; r.m[2][3] = t_z;
            r.m[3][0] = 0.0; r.m[3][1] = 0.0; r.m[3][2] = 0.0; r.m[3][3] = 1.0;

            return r;
        }

        /*
        -----------------------------------------------------------------------
        Scale Transformation
        -----------------------------------------------------------------------
        */
        /** Sets the scale part of the matrix.
        */
        inline void SetScale(const Vector3& v)
        {
            m[0][0] = v.x;
            m[1][1] = v.y;
            m[2][2] = v.z;
        }

        /** Gets a scale matrix.
        */
        inline static Matrix4 GetScale(const Vector3& v)
        {
            Matrix4 r;
            r.m[0][0] = v.x; r.m[0][1] = 0.0; r.m[0][2] = 0.0; r.m[0][3] = 0.0;
            r.m[1][0] = 0.0; r.m[1][1] = v.y; r.m[1][2] = 0.0; r.m[1][3] = 0.0;
            r.m[2][0] = 0.0; r.m[2][1] = 0.0; r.m[2][2] = v.z; r.m[2][3] = 0.0;
            r.m[3][0] = 0.0; r.m[3][1] = 0.0; r.m[3][2] = 0.0; r.m[3][3] = 1.0;

            return r;
        }

        /** Gets a scale matrix - variation for not using a vector.
        */
        inline static Matrix4 GetScale(Real s_x, Real s_y, Real s_z)
        {
            Matrix4 r;
            r.m[0][0] = s_x; r.m[0][1] = 0.0; r.m[0][2] = 0.0; r.m[0][3] = 0.0;
            r.m[1][0] = 0.0; r.m[1][1] = s_y; r.m[1][2] = 0.0; r.m[1][3] = 0.0;
            r.m[2][0] = 0.0; r.m[2][1] = 0.0; r.m[2][2] = s_z; r.m[2][3] = 0.0;
            r.m[3][0] = 0.0; r.m[3][1] = 0.0; r.m[3][2] = 0.0; r.m[3][3] = 1.0;

            return r;
        }

        /** Extracts the rotation / scaling part of the Matrix as a 3x3 matrix. 
        @param m3x3 Destination Matrix3
        */
        inline void Extract3x3Matrix(Matrix3& m3x3) const
        {
			m3x3.m[0][0] = m[0][0];
			m3x3.m[0][1] = m[0][1];
			m3x3.m[0][2] = m[0][2];
			m3x3.m[1][0] = m[1][0];
			m3x3.m[1][1] = m[1][1];
			m3x3.m[1][2] = m[1][2];
			m3x3.m[2][0] = m[2][0];
			m3x3.m[2][1] = m[2][1];
			m3x3.m[2][2] = m[2][2];
        }

        /** Extracts the rotation / scaling part as a Quaternion from the Matrix.
         */
        inline Quaternion ExtractQuaternion() const
        {
			Matrix3 m3x3;
			Extract3x3Matrix(m3x3);
			return Quaternion(m3x3);
        }

        static const Matrix4 ZERO;
        static const Matrix4 IDENTITY;
        /** Useful little matrix which takes 2D clipspace {-1, 1} to {0,1}
            and inverts the Y. */
        static const Matrix4 CLIPSPACE2DTOIMAGESPACE;

        inline Matrix4 operator*(Real scalar) const
        {
            return Matrix4(
                scalar*m[0][0], scalar*m[0][1], scalar*m[0][2], scalar*m[0][3],
                scalar*m[1][0], scalar*m[1][1], scalar*m[1][2], scalar*m[1][3],
                scalar*m[2][0], scalar*m[2][1], scalar*m[2][2], scalar*m[2][3],
                scalar*m[3][0], scalar*m[3][1], scalar*m[3][2], scalar*m[3][3]);
        }

        /** Function for writing to a stream.
        */
        inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Matrix4& m)
        {
            o << "Matrix4(";
			for (uint32_t i = 0; i < 4; ++i)
            {
                o << " row" << (unsigned)i << "{";
                for(uint32_t j = 0; j < 4; ++j)
                {
                    o << m[i][j] << " ";
                }
                o << "}";
            }
            o << ")";
            return o;
        }
		
		Matrix4 Adjoint() const;
		Real Determinant() const;
		Matrix4 Inverse() const;

        /** Building a Matrix4 from orientation / scale / position.
        @remarks
            Transform is performed in the order scale, rotate, translation, i.e. translation is independent
            of orientation axes, scale does not affect size of translation, rotation and scaling are always
            centered on the origin.
        */
        void MakeTransform(const Vector3& position, const Vector3& scale, const Quaternion& orientation);

        /** Building an inverse Matrix4 from orientation / scale / position.
        @remarks
            As makeTransform except it build the inverse given the same data as makeTransform, so
            performing -translation, -rotate, 1/scale in that order.
        */
        void MakeInverseTransform(const Vector3& position, const Vector3& scale, const Quaternion& orientation);

        /** Check whether or not the matrix is affine matrix.
            @remarks
                An affine matrix is a 4x4 matrix with row 4 equal to (0, 0, 0, 1),
                e.g. no projective coefficients.
        */
        inline bool IsAffine(void) const
        {
            return m[3][0] == 0 && m[3][1] == 0 && m[3][2] == 0 && m[3][3] == 1;
        }

        /** Returns the inverse of the affine matrix.
            @note
                The matrix must be an affine matrix. @see Matrix4::isAffine.
        */
        Matrix4 InverseAffine(void) const;

        /** Concatenate two affine matrix.
            @note
                The matrices must be affine matrix. @see Matrix4::isAffine.
        */
        inline Matrix4 ConcatenateAffine(const Matrix4 &m2) const
        {
            assert(IsAffine() && m2.IsAffine());

            return Matrix4(
                m[0][0] * m2.m[0][0] + m[0][1] * m2.m[1][0] + m[0][2] * m2.m[2][0],
                m[0][0] * m2.m[0][1] + m[0][1] * m2.m[1][1] + m[0][2] * m2.m[2][1],
                m[0][0] * m2.m[0][2] + m[0][1] * m2.m[1][2] + m[0][2] * m2.m[2][2],
                m[0][0] * m2.m[0][3] + m[0][1] * m2.m[1][3] + m[0][2] * m2.m[2][3] + m[0][3],

                m[1][0] * m2.m[0][0] + m[1][1] * m2.m[1][0] + m[1][2] * m2.m[2][0],
                m[1][0] * m2.m[0][1] + m[1][1] * m2.m[1][1] + m[1][2] * m2.m[2][1],
                m[1][0] * m2.m[0][2] + m[1][1] * m2.m[1][2] + m[1][2] * m2.m[2][2],
                m[1][0] * m2.m[0][3] + m[1][1] * m2.m[1][3] + m[1][2] * m2.m[2][3] + m[1][3],

                m[2][0] * m2.m[0][0] + m[2][1] * m2.m[1][0] + m[2][2] * m2.m[2][0],
                m[2][0] * m2.m[0][1] + m[2][1] * m2.m[1][1] + m[2][2] * m2.m[2][1],
                m[2][0] * m2.m[0][2] + m[2][1] * m2.m[1][2] + m[2][2] * m2.m[2][2],
                m[2][0] * m2.m[0][3] + m[2][1] * m2.m[1][3] + m[2][2] * m2.m[2][3] + m[2][3],

                0, 0, 0, 1);
        }

        /** 3-D Vector transformation specially for affine matrix.
            @remarks
                Transforms the given 3-D vector by the matrix, projecting the 
                result back into <i>w</i> = 1.
            @note
                The matrix must be an affine matrix. @see Matrix4::isAffine.
        */
        inline Vector3 TransformAffine(const Vector3& v) const
        {
            assert(IsAffine());

            return Vector3(	m[0][0] * v.x + m[0][1] * v.y + m[0][2] * v.z + m[0][3], 
							m[1][0] * v.x + m[1][1] * v.y + m[1][2] * v.z + m[1][3],
							m[2][0] * v.x + m[2][1] * v.y + m[2][2] * v.z + m[2][3]);
        }

        /** 
		 * 4-D Vector transformation specially for affine matrix.
         *   \note
         *       The matrix must be an affine matrix. @see Matrix4::isAffine.
         */
        inline Vector4 TransformAffine(const Vector4& v) const
        {
            assert(IsAffine());

            return Vector4( m[0][0] * v.x + m[0][1] * v.y + m[0][2] * v.z + m[0][3] * v.w, 
							m[1][0] * v.x + m[1][1] * v.y + m[1][2] * v.z + m[1][3] * v.w,
							m[2][0] * v.x + m[2][1] * v.y + m[2][2] * v.z + m[2][3] * v.w,
							v.w);
        }

		inline void ToArray16(Real _m[16]) const
		{
			uint32_t counter = 0;

			for (uint32_t i = 0; i < 4; ++i)
            {
                for(uint32_t j = 0; j < 4; ++j)
                {
                    _m[counter] = m[i][j];
					++counter;
                }
            }
		}
    };

    /* Removed from Vector4 and made a non-member here because otherwise
       Matrix4.h and Vector4.h have to try to include and inline each 
       other, which frankly doesn't work ;)
   */
    inline Vector4 operator* (const Vector4& v, const Matrix4& mat)
    {
        return Vector4(
            v.x*mat[0][0] + v.y*mat[1][0] + v.z*mat[2][0] + v.w*mat[3][0],
            v.x*mat[0][1] + v.y*mat[1][1] + v.z*mat[2][1] + v.w*mat[3][1],
            v.x*mat[0][2] + v.y*mat[1][2] + v.z*mat[2][2] + v.w*mat[3][2],
            v.x*mat[0][3] + v.y*mat[1][3] + v.z*mat[2][3] + v.w*mat[3][3]
            );
    }
	inline void Multiply(Vector4& results, const Vector4& v, const Matrix4& mat)
	{
		results.x = v.x*mat[0][0] + v.y*mat[1][0] + v.z*mat[2][0] + v.w*mat[3][0];
        results.y = v.x*mat[0][1] + v.y*mat[1][1] + v.z*mat[2][1] + v.w*mat[3][1];
        results.z = v.x*mat[0][2] + v.y*mat[1][2] + v.z*mat[2][2] + v.w*mat[3][2];
        results.w = v.x*mat[0][3] + v.y*mat[1][3] + v.z*mat[2][3] + v.w*mat[3][3];
	}

END_ENGINE_NAMESPACE

#endif
