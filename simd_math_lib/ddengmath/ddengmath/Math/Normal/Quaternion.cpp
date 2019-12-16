
#include "Quaternion.h"

const Quaternion Quaternion::ZERO(0.0f, 0.0f, 0.0f, 0.0);
const Quaternion Quaternion::IDENTITY(1.0f, 0.0f, 0.0f, 0.0f);

Matrix3 Quaternion::GetRotationMatrix3() const 
{
  float fTx  = 2.0f * x;
  float fTy  = 2.0f * y;
  float fTz  = 2.0f * z;
  float fTwx = fTx  * w;
  float fTwy = fTy  * w;
  float fTwz = fTz  * w;
  float fTxx = fTx  * x;
  float fTxy = fTy  * x;
  float fTxz = fTz  * x;
  float fTyy = fTy  * y;
  float fTyz = fTz  * y;
  float fTzz = fTz  * z;

  Matrix3 rotationmat;
  rotationmat[0][0] = 1.0f - (fTyy + fTzz);
  rotationmat[0][1] = fTxy - fTwz;
  rotationmat[0][2] = fTxz + fTwy;
  rotationmat[1][0] = fTxy + fTwz;
  rotationmat[1][1] = 1.0f - (fTxx + fTzz);
  rotationmat[1][2] = fTyz + fTwx;
  rotationmat[2][0] = fTxz - fTwy;
  rotationmat[2][1] = fTyz - fTwx;
  rotationmat[2][2] = 1.0f - (fTxx + fTyy);

  //close to 0 checks
  /*rotationmat[0][0] = (GenericMath::FAbs(rotationmat[0][0]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[0][0];
  rotationmat[0][1] = (GenericMath::FAbs(rotationmat[0][1]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[0][1];
  rotationmat[0][2] = (GenericMath::FAbs(rotationmat[0][2]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[0][2];
  rotationmat[1][0] = (GenericMath::FAbs(rotationmat[1][0]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[1][0];
  rotationmat[1][1] = (GenericMath::FAbs(rotationmat[1][1]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[1][1];
  rotationmat[1][2] = (GenericMath::FAbs(rotationmat[1][2]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[1][2];
  rotationmat[2][0] = (GenericMath::FAbs(rotationmat[2][0]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[2][0];
  rotationmat[2][1] = (GenericMath::FAbs(rotationmat[2][1]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[2][1];
  rotationmat[2][2] = (GenericMath::FAbs(rotationmat[2][2]) < ZERO_ERROR_RANGE) ? 0.0f : rotationmat[2][2];
  */
  return rotationmat;
}

void Quaternion::GenerateFromRotationMatrix3(const Matrix3& rotationmat)
{
  // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
  // article "Quaternion Calculus and Fast Animation".

  float fTrace = rotationmat[0][0] + rotationmat[1][1] + rotationmat[2][2];
  float fRoot;

  if ( fTrace > 0.0 )
  {
      // |w| > 1/2, may as well choose w > 1/2
      fRoot = GenericMath::Sqrt(fTrace + 1.0f);  // 2w
      w = 0.5f * fRoot;
      fRoot = 0.5f / fRoot;  // 1/(4w)
      x = (rotationmat[2][1] - rotationmat[1][2]) * fRoot;
      y = (rotationmat[0][2] - rotationmat[2][0]) * fRoot;
      z = (rotationmat[1][0] - rotationmat[0][1]) * fRoot;
  }
  else
  {
      // |w| <= 1/2
      static const unsigned int s_iNext[3] = { 1, 2, 0 };
      unsigned int i = 0;
      if ( rotationmat[1][1] > rotationmat[0][0] )
          i = 1;
      if ( rotationmat[2][2] > rotationmat[i][i] )
          i = 2;
      unsigned int j = s_iNext[i];
      unsigned int k = s_iNext[j];

      fRoot = GenericMath::Sqrt(rotationmat[i][i] - rotationmat[j][j]  - rotationmat[k][k] + 1.0f);
      float* apkQuat[3] = { &x, &y, &z };
      *apkQuat[i] = 0.5f * fRoot;
      fRoot = 0.5f / fRoot;
      w = (rotationmat[k][j] - rotationmat[j][k])*fRoot;
      *apkQuat[j] = (rotationmat[j][i] + rotationmat[i][j]) * fRoot;
      *apkQuat[k] = (rotationmat[k][i] + rotationmat[i][k]) * fRoot;
  }
}

void Quaternion::GetRotationToAxis(Radian& anglerad, Vector3& axisvector)
{
  float fSqrLength = x*x+y*y+z*z;
  if ( fSqrLength > 0.0f )
  {
      anglerad.SetValue(2.0f * GenericMath::ArcCos(w));
      float fInvLength = GenericMath::InvSqrt(fSqrLength);
      axisvector.x = x * fInvLength;
      axisvector.y = y * fInvLength;
      axisvector.z = z * fInvLength;
  }
  else
  {
      // angle is 0 (mod 2*pi), so any axis will do
      anglerad = Radian(0.0f);
      axisvector.x = 1.0f;
      axisvector.y = 0.0f;
      axisvector.z = 0.0f;
  }
}

void Quaternion::GetRotationToAxis(Degree& angledeg, Vector3& axisvector)
{
  float fSqrLength = x*x+y*y+z*z;
  if ( fSqrLength > 0.0f )
  {
      float rad = 2.0f * GenericMath::ArcCos(w);
      angledeg.SetValue(Radian(rad).ConvertToDegree().GetValue());
      float fInvLength = GenericMath::InvSqrt(fSqrLength);
      axisvector.x = x * fInvLength;
      axisvector.y = y * fInvLength;
      axisvector.z = z * fInvLength;
  }
  else
  {
      // angle is 0 (mod 2*pi), so any axis will do
      angledeg = Degree(0.0f);
      axisvector.x = 1.0f;
      axisvector.y = 0.0f;
      axisvector.z = 0.0f;
  }
}
