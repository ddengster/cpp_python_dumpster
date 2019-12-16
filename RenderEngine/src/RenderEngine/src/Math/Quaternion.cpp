
#include "Math/Quaternion.h"
#include "Math/Matrix3.h"
#include "Math/Vector3.h"

DECLARE_ENGINE_NAMESPACE

/** Constants **/
const Real Quaternion::ms_fEpsilon = 1e-03;
const Quaternion Quaternion::ZERO(0.0,0.0,0.0,0.0);
const Quaternion Quaternion::IDENTITY(1.0,0.0,0.0,0.0);

//-----------------------------------------------------------------------
void Quaternion::FromRotationMatrix(const Matrix3& kRot)
{
    // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
    // article "Quaternion Calculus and Fast Animation".

    Real fTrace = kRot[0][0]+kRot[1][1]+kRot[2][2];
    Real fRoot;

    if ( fTrace > 0.0 )
    {
        // |w| > 1/2, may as well choose w > 1/2
        fRoot = GenericMath::Sqrt(fTrace + 1.0);  // 2w
        w = 0.5*fRoot;
        fRoot = 0.5/fRoot;  // 1/(4w)
        x = (kRot[2][1]-kRot[1][2])*fRoot;
        y = (kRot[0][2]-kRot[2][0])*fRoot;
        z = (kRot[1][0]-kRot[0][1])*fRoot;
    }
    else
    {
        // |w| <= 1/2
        static uint32_t s_iNext[3] = { 1, 2, 0 };
        uint32_t i = 0;
        if ( kRot[1][1] > kRot[0][0] )
            i = 1;
        if ( kRot[2][2] > kRot[i][i] )
            i = 2;
        uint32_t j = s_iNext[i];
        uint32_t k = s_iNext[j];

        fRoot = GenericMath::Sqrt(kRot[i][i]-kRot[j][j]-kRot[k][k] + 1.0);
        Real* apkQuat[3] = { &x, &y, &z };
        *apkQuat[i] = 0.5*fRoot;
        fRoot = 0.5/fRoot;
        w = (kRot[k][j]-kRot[j][k])*fRoot;
        *apkQuat[j] = (kRot[j][i]+kRot[i][j])*fRoot;
        *apkQuat[k] = (kRot[k][i]+kRot[i][k])*fRoot;
    }
}
//-----------------------------------------------------------------------
void Quaternion::ToRotationMatrix(Matrix3& kRot) const
{
    Real fTx  = 2.0*x;
    Real fTy  = 2.0*y;
    Real fTz  = 2.0*z;
    Real fTwx = fTx*w;
    Real fTwy = fTy*w;
    Real fTwz = fTz*w;
    Real fTxx = fTx*x;
    Real fTxy = fTy*x;
    Real fTxz = fTz*x;
    Real fTyy = fTy*y;
    Real fTyz = fTz*y;
    Real fTzz = fTz*z;

    kRot[0][0] = 1.0-(fTyy+fTzz);
    kRot[0][1] = fTxy-fTwz;
    kRot[0][2] = fTxz+fTwy;
    kRot[1][0] = fTxy+fTwz;
    kRot[1][1] = 1.0-(fTxx+fTzz);
    kRot[1][2] = fTyz-fTwx;
    kRot[2][0] = fTxz-fTwy;
    kRot[2][1] = fTyz+fTwx;
    kRot[2][2] = 1.0-(fTxx+fTyy);
}
//-----------------------------------------------------------------------
void Quaternion::FromAngleAxis(const Radian& rfAngle, const Vector3& rkAxis)
{
    // assert:  axis[] is unit length
    //
    // The Quaternion representing the rotation is
    //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

	Radian fHalfAngle ( 0.5*rfAngle.GetValue() );
    Real fSin = GenericMath::Sin(fHalfAngle.GetValue());
    w = GenericMath::Cos(fHalfAngle.GetValue());
    x = fSin*rkAxis.x;
    y = fSin*rkAxis.y;
    z = fSin*rkAxis.z;
}
//-----------------------------------------------------------------------
void Quaternion::ToAngleAxis(Radian& rfAngle, Vector3& rkAxis) const
{
    // The Quaternion representing the rotation is
    //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

    Real fSqrLength = x*x+y*y+z*z;
    if ( fSqrLength > 0.0 )
    {
        rfAngle.SetValue(2.0*GenericMath::ACos(w));
        Real fInvLength = GenericMath::InvSqrt(fSqrLength);
        rkAxis.x = x*fInvLength;
        rkAxis.y = y*fInvLength;
        rkAxis.z = z*fInvLength;
    }
    else
    {
        // angle is 0 (mod 2*pi), so any axis will do
        rfAngle = Radian(0.0);
        rkAxis.x = 1.0;
        rkAxis.y = 0.0;
        rkAxis.z = 0.0;
    }
}
//-----------------------------------------------------------------------
void Quaternion::FromAxes(const Vector3* akAxis)
{
    Matrix3 kRot;

    for (uint32_t iCol = 0; iCol < 3; iCol++)
    {
        kRot[0][iCol] = akAxis[iCol].x;
        kRot[1][iCol] = akAxis[iCol].y;
        kRot[2][iCol] = akAxis[iCol].z;
    }

    FromRotationMatrix(kRot);
}
//-----------------------------------------------------------------------
void Quaternion::FromAxes(const Vector3& xaxis, const Vector3& yaxis, const Vector3& zaxis)
{
    Matrix3 kRot;

    kRot[0][0] = xaxis.x;
    kRot[1][0] = xaxis.y;
    kRot[2][0] = xaxis.z;

    kRot[0][1] = yaxis.x;
    kRot[1][1] = yaxis.y;
    kRot[2][1] = yaxis.z;

    kRot[0][2] = zaxis.x;
    kRot[1][2] = zaxis.y;
    kRot[2][2] = zaxis.z;

    FromRotationMatrix(kRot);

}
//-----------------------------------------------------------------------
void Quaternion::ToAxes(Vector3* akAxis) const
{
    Matrix3 kRot;

    ToRotationMatrix(kRot);

    for (uint32_t iCol = 0; iCol < 3; iCol++)
    {
        akAxis[iCol].x = kRot[0][iCol];
        akAxis[iCol].y = kRot[1][iCol];
        akAxis[iCol].z = kRot[2][iCol];
    }
}
//-----------------------------------------------------------------------
Vector3 Quaternion::xAxis(void) const
{
    //Real fTx  = 2.0*x;
    Real fTy  = 2.0*y;
    Real fTz  = 2.0*z;
    Real fTwy = fTy*w;
    Real fTwz = fTz*w;
    Real fTxy = fTy*x;
    Real fTxz = fTz*x;
    Real fTyy = fTy*y;
    Real fTzz = fTz*z;

    return Vector3(1.0-(fTyy+fTzz), fTxy+fTwz, fTxz-fTwy);
}
//-----------------------------------------------------------------------
Vector3 Quaternion::yAxis(void) const
{
    Real fTx  = 2.0*x;
    Real fTy  = 2.0*y;
    Real fTz  = 2.0*z;
    Real fTwx = fTx*w;
    Real fTwz = fTz*w;
    Real fTxx = fTx*x;
    Real fTxy = fTy*x;
    Real fTyz = fTz*y;
    Real fTzz = fTz*z;

    return Vector3(fTxy-fTwz, 1.0-(fTxx+fTzz), fTyz+fTwx);
}
//-----------------------------------------------------------------------
Vector3 Quaternion::zAxis(void) const
{
    Real fTx  = 2.0*x;
    Real fTy  = 2.0*y;
    Real fTz  = 2.0*z;
    Real fTwx = fTx*w;
    Real fTwy = fTy*w;
    Real fTxx = fTx*x;
    Real fTxz = fTz*x;
    Real fTyy = fTy*y;
    Real fTyz = fTz*y;

    return Vector3(fTxz+fTwy, fTyz-fTwx, 1.0-(fTxx+fTyy));
}
//-----------------------------------------------------------------------
void Quaternion::ToAxes(Vector3& xaxis, Vector3& yaxis, Vector3& zaxis) const
{
    Matrix3 kRot;

    ToRotationMatrix(kRot);

    xaxis.x = kRot[0][0];
    xaxis.y = kRot[1][0];
    xaxis.z = kRot[2][0];

    yaxis.x = kRot[0][1];
    yaxis.y = kRot[1][1];
    yaxis.z = kRot[2][1];

    zaxis.x = kRot[0][2];
    zaxis.y = kRot[1][2];
    zaxis.z = kRot[2][2];
}

//-----------------------------------------------------------------------
Quaternion Quaternion::operator+(const Quaternion& rkQ) const
{
    return Quaternion(w+rkQ.w,x+rkQ.x,y+rkQ.y,z+rkQ.z);
}
//-----------------------------------------------------------------------
void Quaternion::Add(Quaternion& results, const Quaternion& leftOperandQuaternion, const Quaternion& rightOperandQuaternion)
{
	results.w+=leftOperandQuaternion.w+rightOperandQuaternion.w;
	results.x+=leftOperandQuaternion.x+rightOperandQuaternion.x;
	results.y+=leftOperandQuaternion.y+rightOperandQuaternion.y;
	results.z+=leftOperandQuaternion.z+rightOperandQuaternion.z;
}
//-----------------------------------------------------------------------
Quaternion Quaternion::operator-(const Quaternion& rkQ) const
{
    return Quaternion(w-rkQ.w,x-rkQ.x,y-rkQ.y,z-rkQ.z);
}
//-----------------------------------------------------------------------
void Quaternion::Subtract(Quaternion& results, const Quaternion& leftOperandQuaternion, const Quaternion& rightOperandQuaternion)
{
	results.w-=leftOperandQuaternion.w+rightOperandQuaternion.w;
	results.x-=leftOperandQuaternion.x+rightOperandQuaternion.x;
	results.y-=leftOperandQuaternion.y+rightOperandQuaternion.y;
	results.z-=leftOperandQuaternion.z+rightOperandQuaternion.z;
}
//-----------------------------------------------------------------------
Quaternion Quaternion::operator*(const Quaternion& rkQ) const
{
    // NOTE:  Multiplication is not generally commutative, so in most
    // cases p*q != q*p.

    return Quaternion
    (
        w * rkQ.w - x * rkQ.x - y * rkQ.y - z * rkQ.z,
        w * rkQ.x + x * rkQ.w + y * rkQ.z - z * rkQ.y,
        w * rkQ.y + y * rkQ.w + z * rkQ.x - x * rkQ.z,
        w * rkQ.z + z * rkQ.w + x * rkQ.y - y * rkQ.x
    );
}
//-----------------------------------------------------------------------
void Quaternion::Multiply(Quaternion& results, const Quaternion& leftOperandQuaternion,const Quaternion& rightOperandQuaternion)
{
    // NOTE:  Multiplication is not generally commutative, so in most
    // cases p*q != q*p.
        results.w = leftOperandQuaternion.w * rightOperandQuaternion.w - leftOperandQuaternion.x * rightOperandQuaternion.x - 
					leftOperandQuaternion.y * rightOperandQuaternion.y - leftOperandQuaternion.z * rightOperandQuaternion.z;
        results.x = leftOperandQuaternion.w * rightOperandQuaternion.x + leftOperandQuaternion.x * rightOperandQuaternion.w + 
					leftOperandQuaternion.y * rightOperandQuaternion.z - leftOperandQuaternion.z * rightOperandQuaternion.y;
        results.y = leftOperandQuaternion.w * rightOperandQuaternion.y + leftOperandQuaternion.y * rightOperandQuaternion.w + 
					leftOperandQuaternion.z * rightOperandQuaternion.x - leftOperandQuaternion.x * rightOperandQuaternion.z;
        results.z = leftOperandQuaternion.w * rightOperandQuaternion.z + leftOperandQuaternion.z * rightOperandQuaternion.w + 
					leftOperandQuaternion.x * rightOperandQuaternion.y - leftOperandQuaternion.y * rightOperandQuaternion.x;
}
//-----------------------------------------------------------------------
Quaternion Quaternion::operator*(Real fScalar) const
{
    return Quaternion(fScalar*w,fScalar*x,fScalar*y,fScalar*z);
}
//-----------------------------------------------------------------------
void Quaternion::Multiply(Quaternion& results, const Quaternion& leftOperandQuaternion, Real fScalar)
{
	results.w = leftOperandQuaternion.w * fScalar;
	results.x = leftOperandQuaternion.x * fScalar;
	results.y = leftOperandQuaternion.y * fScalar;
	results.z = leftOperandQuaternion.z * fScalar;
}
//-----------------------------------------------------------------------
Quaternion operator*(Real fScalar, const Quaternion& rkQ)
{
    return Quaternion(fScalar*rkQ.w,fScalar*rkQ.x,fScalar*rkQ.y,
        fScalar*rkQ.z);
}
//-----------------------------------------------------------------------
Quaternion Quaternion::operator-() const
{
    return Quaternion(-w,-x,-y,-z);
}
//-----------------------------------------------------------------------
Real Quaternion::Dot(const Quaternion& rkQ) const
{
    return w*rkQ.w+x*rkQ.x+y*rkQ.y+z*rkQ.z;
}
//-----------------------------------------------------------------------
Real Quaternion::Norm() const
{
    return w*w+x*x+y*y+z*z;
}
//-----------------------------------------------------------------------
Quaternion Quaternion::Inverse() const
{
    Real fNorm = w*w+x*x+y*y+z*z;
    if ( fNorm > 0.0 )
    {
        Real fInvNorm = 1.0/fNorm;
        return Quaternion(w*fInvNorm,-x*fInvNorm,-y*fInvNorm,-z*fInvNorm);
    }
    else
    {
        // return an invalid result to flag the error
        return ZERO;
    }
}
//-----------------------------------------------------------------------
Quaternion Quaternion::UnitInverse() const
{
    // assert:  'this' is unit length
    return Quaternion(w,-x,-y,-z);
}
//-----------------------------------------------------------------------
Quaternion Quaternion::Exp() const
{
    // If q = A*(x*i+y*j+z*k) where (x,y,z) is unit length, then
    // exp(q) = cos(A)+sin(A)*(x*i+y*j+z*k).  If sin(A) is near zero,
    // use exp(q) = cos(A)+A*(x*i+y*j+z*k) since A/sin(A) has limit 1.

    Radian fAngle ( GenericMath::Sqrt(x*x+y*y+z*z) );
    Real fSin = GenericMath::Sin(fAngle.GetValue());

    Quaternion kResult;
    kResult.w = GenericMath::Cos(fAngle.GetValue());

    if ( GenericMath::FAbs(fSin) >= ms_fEpsilon )
    {
		Real fCoeff = fSin/(fAngle.GetValue());
        kResult.x = fCoeff*x;
        kResult.y = fCoeff*y;
        kResult.z = fCoeff*z;
    }
    else
    {
        kResult.x = x;
        kResult.y = y;
        kResult.z = z;
    }

    return kResult;
}
//-----------------------------------------------------------------------
Quaternion Quaternion::Log() const
{
    // If q = cos(A)+sin(A)*(x*i+y*j+z*k) where (x,y,z) is unit length, then
    // log(q) = A*(x*i+y*j+z*k).  If sin(A) is near zero, use log(q) =
    // sin(A)*(x*i+y*j+z*k) since sin(A)/A has limit 1.

    Quaternion kResult;
    kResult.w = 0.0;

    if ( GenericMath::FAbs(w) < 1.0 )
    {
        Radian fAngle ( GenericMath::ACos(w) );
        Real fSin = GenericMath::Sin(fAngle.GetValue());
        if ( GenericMath::FAbs(fSin) >= ms_fEpsilon )
        {
			Real fCoeff = fAngle.GetValue()/fSin;
            kResult.x = fCoeff*x;
            kResult.y = fCoeff*y;
            kResult.z = fCoeff*z;
            return kResult;
        }
    }

    kResult.x = x;
    kResult.y = y;
    kResult.z = z;

    return kResult;
}
//-----------------------------------------------------------------------
Vector3 Quaternion::operator*(const Vector3& v) const
{
	// nVidia SDK implementation
	Vector3 uv, uuv;
	Vector3 qvec(x, y, z);
	uv = qvec.CrossProduct(v);
	uuv = qvec.CrossProduct(uv);
	uv *= (2.0f * w);
	uuv *= 2.0f;

	return v + uv + uuv;

}
//-----------------------------------------------------------------------
bool Quaternion::Equals(const Quaternion& rhs, const Radian& tolerance) const
{
    Real fCos = Dot(rhs);
    Radian angle(GenericMath::ACos(fCos));

	return (GenericMath::FAbs(angle.GetValue()) <= tolerance.GetValue())
        || GenericMath::RealEqual(angle.GetValue(), PI, tolerance.GetValue());
}

//-----------------------------------------------------------------------
Quaternion Quaternion::Slerp(Real fT, const Quaternion& rkP, const Quaternion& rkQ, bool shortestPath)
{
    Real fCos = rkP.Dot(rkQ);
    Quaternion rkT;

    // Do we need to invert rotation?
    if (fCos < 0.0f && shortestPath)
    {
        fCos = -fCos;
        rkT = -rkQ;
    }
    else
    {
        rkT = rkQ;
    }

    if (GenericMath::FAbs(fCos) < 1 - ms_fEpsilon)
    {
        // Standard case (slerp)
        Real fSin = GenericMath::Sqrt(1 - GenericMath::Sqr(fCos));
        Radian fAngle(GenericMath::ATan2(fSin, fCos));
        Real fInvSin = 1.0f / fSin;
        Real fCoeff0 = GenericMath::Sin((1.0f - fT) * fAngle.GetValue()) * fInvSin;
        Real fCoeff1 = GenericMath::Sin(fT * fAngle.GetValue()) * fInvSin;
        return fCoeff0 * rkP + fCoeff1 * rkT;
    }
    else
    {
        // There are two situations:
        // 1. "rkP" and "rkQ" are very close (fCos ~= +1), so we can do a linear
        //    interpolation safely.
        // 2. "rkP" and "rkQ" are almost inverse of each other (fCos ~= -1), there
        //    are an infinite number of possibilities interpolation. but we haven't
        //    have method to fix this case, so just use linear interpolation here.
        Quaternion t = (1.0f - fT) * rkP + fT * rkT;
        // taking the complement requires renormalisation
        t.Normalise();
        return t;
    }
}
//-----------------------------------------------------------------------
Quaternion Quaternion::SlerpExtraSpins(Real fT, const Quaternion& rkP, const Quaternion& rkQ, int iExtraSpins)
{
    Real fCos = rkP.Dot(rkQ);
    Radian fAngle ( GenericMath::ACos(fCos) );

	if (GenericMath::FAbs(fAngle.GetValue()) < ms_fEpsilon)
        return rkP;

    Real fSin = GenericMath::Sin(fAngle.GetValue());
    Radian fPhase (PI*iExtraSpins*fT);
    Real fInvSin = 1.0/fSin;
    Real fCoeff0 = GenericMath::Sin((1.0-fT)*fAngle.GetValue() - fPhase.GetValue())*fInvSin;
    Real fCoeff1 = GenericMath::Sin(fT*fAngle.GetValue() + fPhase.GetValue())*fInvSin;
    return fCoeff0*rkP + fCoeff1*rkQ;
}
//-----------------------------------------------------------------------
void Quaternion::Intermediate(const Quaternion& rkQ0, const Quaternion& rkQ1, const Quaternion& rkQ2,
							  Quaternion& rkA, Quaternion& rkB)
{
    // assert:  q0, q1, q2 are unit quaternions

    Quaternion kQ0inv = rkQ0.UnitInverse();
    Quaternion kQ1inv = rkQ1.UnitInverse();
    Quaternion rkP0 = kQ0inv*rkQ1;
    Quaternion rkP1 = kQ1inv*rkQ2;
    Quaternion kArg = 0.25*(rkP0.Log()-rkP1.Log());
    Quaternion kMinusArg = -kArg;

    rkA = rkQ1*kArg.Exp();
    rkB = rkQ1*kMinusArg.Exp();
}
//-----------------------------------------------------------------------
Quaternion Quaternion::Squad(Real fT, const Quaternion& rkP, const Quaternion& rkA,
							const Quaternion& rkB, const Quaternion& rkQ, bool shortestPath)
{
    Real fSlerpT = 2.0*fT*(1.0-fT);
    Quaternion kSlerpP = Slerp(fT, rkP, rkQ, shortestPath);
    Quaternion kSlerpQ = Slerp(fT, rkA, rkB);
    return Slerp(fSlerpT, kSlerpP ,kSlerpQ);
}
//-----------------------------------------------------------------------
Real Quaternion::Normalise(void)
{
    Real len = Norm();
    Real factor = 1.0f / GenericMath::Sqrt(len);
    *this = *this * factor;
    return len;
}
//-----------------------------------------------------------------------
Radian Quaternion::GetRoll(bool reprojectAxis) const
{
	if (reprojectAxis)
	{
		// roll = atan2(localx.y, localx.x)
		// pick parts of xAxis() implementation that we need
		Real fTx  = 2.0*x;
		Real fTy  = 2.0*y;
		Real fTz  = 2.0*z;
		Real fTwz = fTz*w;
		Real fTxy = fTy*x;
		Real fTyy = fTy*y;
		Real fTzz = fTz*z;

		// Vector3(1.0-(fTyy+fTzz), fTxy+fTwz, fTxz-fTwy);

		return Radian(GenericMath::ATan2(fTxy+fTwz, 1.0-(fTyy+fTzz)));

	}
	else
	{
		return Radian(GenericMath::ATan2(2*(x*y + w*z), w*w + x*x - y*y - z*z));
	}
}
//-----------------------------------------------------------------------
Radian Quaternion::GetPitch(bool reprojectAxis) const
{
	if (reprojectAxis)
	{
		// pitch = atan2(localy.z, localy.y)
		// pick parts of yAxis() implementation that we need
		Real fTx  = 2.0*x;
		Real fTy  = 2.0*y;
		Real fTz  = 2.0*z;
		Real fTwx = fTx*w;
		Real fTxx = fTx*x;
		Real fTyz = fTz*y;
		Real fTzz = fTz*z;

		// Vector3(fTxy-fTwz, 1.0-(fTxx+fTzz), fTyz+fTwx);
		return Radian(GenericMath::ATan2(fTyz+fTwx, 1.0-(fTxx+fTzz)));
	}
	else
	{
		// internal version
		return Radian(GenericMath::ATan2(2*(y*z + w*x), w*w - x*x - y*y + z*z));
	}
}
//-----------------------------------------------------------------------
Radian Quaternion::GetYaw(bool reprojectAxis) const
{
	if (reprojectAxis)
	{
		// yaw = atan2(localz.x, localz.z)
		// pick parts of zAxis() implementation that we need
		Real fTx  = 2.0*x;
		Real fTy  = 2.0*y;
		Real fTz  = 2.0*z;
		Real fTwy = fTy*w;
		Real fTxx = fTx*x;
		Real fTxz = fTz*x;
		Real fTyy = fTy*y;

		// Vector3(fTxz+fTwy, fTyz-fTwx, 1.0-(fTxx+fTyy));

		return Radian(GenericMath::ATan2(fTxz+fTwy, 1.0-(fTxx+fTyy)));
	}
	else
	{
		// internal version
		return Radian(GenericMath::ASin(-2*(x*z - w*y)));
	}
}
//-----------------------------------------------------------------------
Quaternion Quaternion::Nlerp(Real fT, const Quaternion& rkP,
    const Quaternion& rkQ, bool shortestPath)
{
	Quaternion result;
    Real fCos = rkP.Dot(rkQ);
	if (fCos < 0.0f && shortestPath)
	{
		result = rkP + fT * ((-rkQ) - rkP);
	}
	else
	{
		result = rkP + fT * (rkQ - rkP);
	}
    result.Normalise();
    return result;
}
END_ENGINE_NAMESPACE
