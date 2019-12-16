
#ifndef _MATRIX3_H
#define _MATRIX3_H

#include "Prerequisites.h"

#include "Math/Vector3.h"
#include "Math/GenericMath.h"

// NB All code adapted from Wild Magic 0.2 Matrix math (free source code)
// http://www.geometrictools.com/

// NOTE.  The (x,y,z) coordinate system is assumed to be right-handed.
// Coordinate axis rotation matrices are of the form
//   RX =    1       0       0
//           0     cos(t) -sin(t)
//           0     sin(t)  cos(t)
// where t > 0 indicates a counterclockwise rotation in the yz-Plane
//   RY =  cos(t)    0     sin(t)
//           0       1       0
//        -sin(t)    0     cos(t)
// where t > 0 indicates a counterclockwise rotation in the zx-Plane
//   RZ =  cos(t) -sin(t)    0
//         sin(t)  cos(t)    0
//           0       0       1
// where t > 0 indicates a counterclockwise rotation in the xy-Plane.

DECLARE_ENGINE_NAMESPACE
/** \ingroup Maths
A 3x3 matrix which can represent rotations around axes.
  \note
  <b>All the code is adapted from the Wild Magic 0.2 Matrix
  library (http://www.geometrictools.com/).</b>
  \par
  The coordinate system is assumed to be <b>right-handed</b>.
  \note
    functions starting with name "From*" is doing the specified operations into the matrix
    functions starting with name "To*" is to query from matrix.
 */
class /*shared*/ Matrix3
{
	public:
		/** Default constructor.
		 *   \note
		 *       It does <b>NOT</b> initialize the matrix for efficiency.
		 */
		inline Matrix3()
		{
		}
		/**
		 * Constructor with a 3 x 3 array as the default value.
		 *
		 * \param arr			The array of values to initialise the Matrix3 to.
		 */
		inline explicit Matrix3(const Real arr[3][3])
		{
			memcpy(m,arr,9*sizeof(Real));
		}
		/**
		 * Constructor another Matrix3 as the default value.
		 *
		 * \param rkMatrix		The Matrix3 to initialise this to.
		 */
		inline Matrix3(const Matrix3& rkMatrix)
		{
			memcpy(m,rkMatrix.m,9*sizeof(Real));
		}
		/**
		 * Constructor using Real numbers as the default value of its elements.
		 *
		 * fEntry00	-	Value of element 0,0
		 * fEntry22	-	Value of element 2,2
		 */
		Matrix3(Real fEntry00, Real fEntry01, Real fEntry02,
				Real fEntry10, Real fEntry11, Real fEntry12,
				Real fEntry20, Real fEntry21, Real fEntry22)
		{
			m[0][0] = fEntry00;
			m[0][1] = fEntry01;
			m[0][2] = fEntry02;
			m[1][0] = fEntry10;
			m[1][1] = fEntry11;
			m[1][2] = fEntry12;
			m[2][0] = fEntry20;
			m[2][1] = fEntry21;
			m[2][2] = fEntry22;
		}

		// member access, allows use of construct mat[r][c]
		inline Real* operator[](uint32_t iRow) const
		{
			return (Real*)m[iRow];
		}
		/*inline operator Real* ()
		  {
		  return (Real*)m[0];
		  }*/
		/**
		 * Get the column, 'iCol', of this matrix, in a Vector3 form.
		 *
		 * \param iCol			The column to get from the matrix
		 * \return				A Vector3 representation of the column 'iCol'
		 */
		Vector3 GetColumn(uint32_t iCol) const;
		/**
		 * Replace column, 'iCol', of this matrix with the elements of a Vector3.
		 *
		 * \param iCol			The column to get from the matrix
		 * \param vec			Vector3 to replace 'iCol' column with
		 */
		void SetColumn(uint32_t iCol, const Vector3& vec);
		/**
		 * Sets this matrix's elements to the three Vector3's elements, column by column.
		 *
		 * \param xAxis			Vector for the first column
		 * \param yAxis			Vector for the second column
		 * \param zAxis			Vector for the third column
		 */
		void FromAxes(const Vector3& xAxis, const Vector3& yAxis, const Vector3& zAxis);

		// assignment and comparison
		/**
		 * Assignment operator.
		 */
		inline Matrix3& operator=(const Matrix3& rkMatrix)
		{
			memcpy(m,rkMatrix.m,9*sizeof(Real));
			return *this;
		}
		/**
		 * 'Is equals to' operator.
		 */
		bool operator==(const Matrix3& rkMatrix) const;	
		/**
		 * 'Not equals to' operator.
		 */
		inline bool operator!=(const Matrix3& rkMatrix) const
		{
			return !operator==(rkMatrix);
		}
		/**
		 * Assignment function.
		 *
		 * \param rkMatrix			Target matrix to assignment to
		 */
		inline void Equals(const Matrix3& rkMatrix)
		{
			memcpy(m,rkMatrix.m,9*sizeof(Real));
		}
		/**
		 * 'Is equals to' function.
		 *
		 * \param rkMatrix			Matrix to compare with
		 * \return					True if matrix is equal to this matrix false if not.
		 */
		bool IsEquals(const Matrix3& rkMatrix) const;
		/**
		 * 'Not equals to' function.
		 *
		 * \param rkMatrix			Matrix to compare with
		 * \return					True if matrix is not equal to this matrix false if equals.
		 */
		inline bool NotEquals(const Matrix3& rkMatrix) const
		{
			return !operator==(rkMatrix);
		}

		// arithmetic operations
		/**
		 * Addition operator.
		 */
		Matrix3 operator+(const Matrix3& rkMatrix) const;
		/**
		 * Subtraction operator.
		 */
		Matrix3 operator-(const Matrix3& rkMatrix) const;
		/**
		 * Multiplication operator.
		 */
		Matrix3 operator*(const Matrix3& rkMatrix) const;
		/**
		 * Negation operator.
		 */
		Matrix3 operator-() const;

		/**
		 * Addition function.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param result				Variable to hold the results of this operation
		 * \param leftOperandMatrix		Left side operand
		 * \param rightOperandMatrix	Right side operand
		 */
		static void Add(Matrix3& result, const Matrix3& leftOperandMatrix, const Matrix3& rightOperandMatrix);
		/**
		 * Subtraction function.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param result				Variable to hold the results of this operation
		 * \param leftOperandMatrix		Left side operand
		 * \param rightOperandMatrix	Right side operand
		 */
		static void Subtract(Matrix3& result, const Matrix3& leftOperandMatrix, const Matrix3& rightOperandMatrix);
		/**
		 * Muliplication function.
		 * \note
		 *		Do not pass in the same variable to both result and any of the operands or the result will be
		 *		unpredictable.
		 *
		 * \param result				Variable to hold the results of this operation
		 * \param leftOperandMatrix		Left side operand
		 * \param rightOperandMatrix	Right side operand
		 */
		static void Multiply(Matrix3& result, const Matrix3& leftOperandMatrix, const Matrix3& rightOperandMatrix);
		/**
		 * Addition function.
		 * \note
		 *		Do not pass in the same variable to both paramters or the result will be
		 *		unpredictable.
		 *
		 * \param result				Variable to hold the results of this operation
		 * \param rkMatrix				Matrix to negate
		 */
		static void Negation(Matrix3& result, const Matrix3& rkMatrix);

		/**
		 * Multiplication operator for matrix * vector.
		 * \note
		 *		matrix * vector [3x3 * 3x1 = 3x1]
		 */
		Vector3 operator*(const Vector3& rkVector) const;
		/**
		 * Multiplication operator for matrix * vector.
		 * \note
		 *		vector * matrix [1x3 * 3x3 = 1x3]
		 */
		/*shared*/ friend Vector3 operator*(const Vector3& rkVector, const Matrix3& rkMatrix);
		/**
		 * Multiplication operator for matrix * scalar.
		 */
		Matrix3 operator*(Real fScalar) const;
		/**
		 * Multiplication operator for matrix * scalar.
		 * \note
		 *		scalar * matrix
		 */
		/*shared*/ friend Matrix3 operator*(Real fScalar, const Matrix3& rkMatrix);

		/**
		 * Multiplication function for matrix * vector.
		 * \note
		 *		matrix * vector [3x3 * 3x1 = 3x1]
		 */
		static void Multiply(Vector3& result, const Matrix3& rkMatrix, const Vector3& rkVector);
		/**
		 * Multiplication function for matrix * vector.
		 * \note
		 *		vector * matrix [1x3 * 3x3 = 1x3]
		 */
		/*shared*/ friend void Multiply(Vector3& result, const Vector3& rkVector, const Matrix3& rkMatrix);
		/**
		 * Multiplication function for matrix * scalar.
		 */
		/*shared*/ friend void Multiply(Matrix3& result, const Matrix3& rkMatrix, Real fScalar);
		/**
		 * Multiplication operator for matrix * scalar.
		 * \note
		 *		scalar * matrix
		 */
		/*shared*/ friend void Multiply(Matrix3& result, Real fScalar, const Matrix3& rkMatrix);

		// utilities
		/**
		 * Transpose function.
		 *
		 * \return				The transposed version of this matrix.
		 */
		Matrix3 Transpose() const;
		/**
		 * Gets the inverse of this matrix. If this matrix's determinant is zero or near zero, this function returns
		 * false prematurely.
		 *
		 * \param rkInverse		Matrix store the results of this operation
		 * \param fTolerance	The 'near zero' tolerance to check against
		 * \return				If this matrix is not inversable
		 */
		bool Inverse(Matrix3& rkInverse, Real fTolerance = 1e-06) const;
		/**
		 * Gets the inverse of this matrix. If this matrix's determinant is zero or near zero, this function returns
		 * false prematurely.
		 *
		 * \param fTolerance	The 'near zero' tolerance to check against
		 * \return				Inverse of this matrix
		 */
		Matrix3 Inverse(Real fTolerance = 1e-06) const;
		/**
		 * Returns the determinant of this matrix.
		 *
		 * \return				Value of the determinant.
		 */
		Real Determinant() const;

		// singular value decomposition
		void SingularValueDecomposition(Matrix3& rkL, Vector3& rkS,	Matrix3& rkR) const;
		void SingularValueComposition(const Matrix3& rkL, const Vector3& rkS, const Matrix3& rkR);

		// Gram-Schmidt orthonormalization (applied to columns of rotation matrix)
		void OrthoNormalize();

		// orthogonal Q, diagonal D, upper triangular U stored as (u01,u02,u12)
		void QDUDecomposition(Matrix3& rkQ, Vector3& rkD, Vector3& rkU) const;

		Real SpectralNorm() const;

		// matrix must be orthonormal
		void ToAxisAngle(Vector3& rkAxis, Radian& rfAngle) const;
		inline void ToAxisAngle(Vector3& rkAxis, Degree& rfAngle) const
		{
			Radian r;
			ToAxisAngle (rkAxis, r);
			rfAngle = r.ConvertToDegree();
		}
		void FromAxisAngle(const Vector3& rkAxis, const Radian& fRadians);
		inline void ToAxisAngle(Vector3& rkAxis, Real& rfRadians) const
		{
			Radian r;
			ToAxisAngle (rkAxis, r);
			rfRadians = r.GetValue();
		}
		inline void FromAxisAngle(const Vector3& rkAxis, Real fRadians)
		{
			FromAxisAngle (rkAxis, Radian(fRadians));
		}

		// The matrix must be orthonormal.  The decomposition is yaw*pitch*roll
		// where yaw is rotation about the Up vector, pitch is rotation about the
		// Right axis, and roll is rotation about the Direction axis.
		bool ToEulerAnglesXYZ(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;
		bool ToEulerAnglesXZY(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;
		bool ToEulerAnglesYXZ(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;
		bool ToEulerAnglesYZX(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;
		bool ToEulerAnglesZXY(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;
		bool ToEulerAnglesZYX(Radian& rfYAngle, Radian& rfPAngle, Radian& rfRAngle) const;

		void FromEulerAnglesXYZ(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);
		void FromEulerAnglesXZY(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);
		void FromEulerAnglesYXZ(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);
		void FromEulerAnglesYZX(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);
		void FromEulerAnglesZXY(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);
		void FromEulerAnglesZYX(const Radian& fYAngle, const Radian& fPAngle, const Radian& fRAngle);

		inline bool ToEulerAnglesXYZ(float& rfYAngle, float& rfPAngle, float& rfRAngle) const
		{
			Radian y, p, r;
			bool b = ToEulerAnglesXYZ(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline bool ToEulerAnglesXZY(float& rfYAngle, float& rfPAngle, float& rfRAngle) const
		{
			Radian y, p, r;
			bool b = ToEulerAnglesXZY(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline bool ToEulerAnglesYXZ(float& rfYAngle, float& rfPAngle, float& rfRAngle) const
		{
			Radian y, p, r;
			bool b = ToEulerAnglesYXZ(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline bool ToEulerAnglesYZX(float& rfYAngle, float& rfPAngle, float& rfRAngle) const
		{
			Radian y, p, r;
			bool b = ToEulerAnglesYZX(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline bool ToEulerAnglesZXY(float& rfYAngle, float& rfPAngle, float& rfRAngle) const
		{
			Radian y, p, r;
			bool b = ToEulerAnglesZXY(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline bool ToEulerAnglesZYX(float& rfYAngle, float& rfPAngle, float& rfRAngle) const 
		{
			Radian y, p, r;
			bool b = ToEulerAnglesZYX(y,p,r);
			rfYAngle = y.GetValue();
			rfPAngle = p.GetValue();
			rfRAngle = r.GetValue();
			return b;
		}
		inline void FromEulerAnglesXYZ(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		inline void fromEulerAnglesXZY(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		inline void FromEulerAnglesYXZ(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		inline void FromEulerAnglesYZX(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		inline void FromEulerAnglesZXY(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		inline void FromEulerAnglesZYX(float fYAngle, float fPAngle, float fRAngle)
		{
			FromEulerAnglesXYZ (Radian(fYAngle), Radian(fPAngle), Radian(fRAngle));
		}
		// eigensolver, matrix must be symmetric
		void EigenSolveSymmetric(Real afEigenvalue[3], Vector3 akEigenvector[3]) const;

		static void TensorProduct(const Vector3& rkU, const Vector3& rkV, Matrix3& rkProduct);

		inline void ToArray9(Real _m[9]) const
		{
			uint32_t counter = 0;

			for (uint32_t i = 0; i < 3; ++i)
			{
				for(uint32_t j = 0; j < 3; ++j)
				{
					_m[counter] = m[i][j];
					++counter;
				}
			}
		}

		/** 
		 * Function for writing to a stream.
		 */
		inline /*shared*/ friend std::ostream& operator << (std::ostream& o, const Matrix3& m)
		{
			o << "Matrix3(";
			for (uint32_t i = 0; i < 3; ++i)
			{
				o << " row" << (unsigned)i << "{";
				for(uint32_t j = 0; j < 3; ++j)
				{
					o << m[i][j] << " ";
				}
				o << "}";
			}
			o << ")";
			return o;
		}

		static const Real EPSILON;
		static const Matrix3 ZERO;
		static const Matrix3 IDENTITY;

	protected:
		// support for eigensolver
		void Tridiagonal (Real afDiag[3], Real afSubDiag[3]);
		bool QLAlgorithm (Real afDiag[3], Real afSubDiag[3]);

		// support for singular value decomposition
		static const Real ms_fSvdEpsilon;
		static const unsigned int ms_iSvdMaxIterations;
		static void Bidiagonalize (Matrix3& kA, Matrix3& kL, Matrix3& kR);
		static void GolubKahanStep (Matrix3& kA, Matrix3& kL, Matrix3& kR);

		// support for spectral norm
		static Real MaxCubicRoot (Real afCoeff[3]);

		Real m[3][3];

		// for faster access
		friend class Matrix4;
};

END_ENGINE_NAMESPACE

#endif
