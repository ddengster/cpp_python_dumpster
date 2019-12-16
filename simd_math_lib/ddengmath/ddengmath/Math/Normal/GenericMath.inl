


inline float GenericMath::ArcCos (float fValue)
{
    if (-(float)1.0 < fValue)
    {
        if (fValue < (float)1.0)
        {
            return (float)acos((double)fValue);
        }
        else
        {
            return (float)0.0;
        }
    }
    else
    {
        return PI;
    }
}
//----------------------------------------------------------------------------
inline float GenericMath::ArcSin (float fValue)
{
    if (-(float)1.0 < fValue)
    {
        if (fValue < (float)1.0)
        {
            return (float)asin((double)fValue);
        }
        else
        {
            return HALF_PI;
        }
    }
    else
    {
        return -HALF_PI;
    }
}
//----------------------------------------------------------------------------
inline float GenericMath::ArcTan (float fValue)
{
    return (float)atan((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::ArcTan2 (float fY, float fX)
{
    return (float)atan2((double)fY,(double)fX);
}
//----------------------------------------------------------------------------
inline float GenericMath::Ceil (float fValue)
{
    return (float)ceil((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Cos (float fValue)
{
    return (float)cos((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Exp (float fValue)
{
    return (float)exp((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::FAbs (float fValue)
{
    return (float)fabs((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Floor (float fValue)
{
    return (float)floor((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::FMod (float fX, float fY)
{
    return (float)fmod((double)fX,(double)fY);
}
//----------------------------------------------------------------------------
inline float GenericMath::InvSqrt (float fValue)
{
    return (float)(1.0/sqrt((double)fValue));
}
//----------------------------------------------------------------------------
inline float GenericMath::Log (float fValue)
{
    return (float)log((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Log2 (float fValue)
{
    return GenericMath::INV_LN_2 * (float)log((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Log10 (float fValue)
{
    return GenericMath::INV_LN_10 * (float)log((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Power (float fBase, float fExponent)
{
    return (float)pow((double)fBase,(double)fExponent);
}
//----------------------------------------------------------------------------
inline float GenericMath::Sin (float fValue)
{
    return (float)sin((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Sqr (float fValue)
{
    return fValue*fValue;
}
//----------------------------------------------------------------------------
inline float GenericMath::Sqrt (float fValue)
{
    return (float)sqrt((double)fValue);
}
//----------------------------------------------------------------------------
inline float GenericMath::Tan (float fValue)
{
    return (float)tan((double)fValue);
}
//----------------------------------------------------------------------------
inline int GenericMath::IsSigned(int iValue)
{
    if (iValue > 0)
    {
        return 1;
    }

    if (iValue < 0)
    {
        return -1;
    }

    return 0;
}
//----------------------------------------------------------------------------
inline float GenericMath::IsSigned(float fValue)
{
    if (fValue > (float)0.0)
    {
        return (float)1.0;
    }

    if (fValue < (float)0.0)
    {
        return -(float)1.0;
    }

    return (float)0.0;
}

//----------------------------------------------------------------------------
inline float GenericMath::FastSin0 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = (float)7.61e-03;
    fResult *= fASqr;
    fResult -= (float)1.6605e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    fResult *= fAngle;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastSin1 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = -(float)2.39e-08;
    fResult *= fASqr;
    fResult += (float)2.7526e-06;
    fResult *= fASqr;
    fResult -= (float)1.98409e-04;
    fResult *= fASqr;
    fResult += (float)8.3333315e-03;
    fResult *= fASqr;
    fResult -= (float)1.666666664e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    fResult *= fAngle;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastCos0 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = (float)3.705e-02;
    fResult *= fASqr;
    fResult -= (float)4.967e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastCos1 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = -(float)2.605e-07;
    fResult *= fASqr;
    fResult += (float)2.47609e-05;
    fResult *= fASqr;
    fResult -= (float)1.3888397e-03;
    fResult *= fASqr;
    fResult += (float)4.16666418e-02;
    fResult *= fASqr;
    fResult -= (float)4.999999963e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastTan0 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = (float)2.033e-01;
    fResult *= fASqr;
    fResult += (float)3.1755e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    fResult *= fAngle;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastTan1 (float fAngle)
{
    float fASqr = fAngle*fAngle;
    float fResult = (float)9.5168091e-03;
    fResult *= fASqr;
    fResult += (float)2.900525e-03;
    fResult *= fASqr;
    fResult += (float)2.45650893e-02;
    fResult *= fASqr;
    fResult += (float)5.33740603e-02;
    fResult *= fASqr;
    fResult += (float)1.333923995e-01;
    fResult *= fASqr;
    fResult += (float)3.333314036e-01;
    fResult *= fASqr;
    fResult += (float)1.0;
    fResult *= fAngle;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvSin0 (float fValue)
{
    float fRoot = GenericMath::Sqrt(((float)1.0)-fValue);
    float fResult = -(float)0.0187293;
    fResult *= fValue;
    fResult += (float)0.0742610;
    fResult *= fValue;
    fResult -= (float)0.2121144;
    fResult *= fValue;
    fResult += (float)1.5707288;
    fResult = HALF_PI - fRoot*fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvSin1 (float fValue)
{
    float fRoot = GenericMath::Sqrt(FAbs(((float)1.0)-fValue));
    float fResult = -(float)0.0012624911;
    fResult *= fValue;
    fResult += (float)0.0066700901;
    fResult *= fValue;
    fResult -= (float)0.0170881256;
    fResult *= fValue;
    fResult += (float)0.0308918810;
    fResult *= fValue;
    fResult -= (float)0.0501743046;
    fResult *= fValue;
    fResult += (float)0.0889789874;
    fResult *= fValue;
    fResult -= (float)0.2145988016;
    fResult *= fValue;
    fResult += (float)1.5707963050;
    fResult = HALF_PI - fRoot*fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvCos0 (float fValue)
{
    float fRoot = GenericMath::Sqrt(((float)1.0)-fValue);
    float fResult = -(float)0.0187293;
    fResult *= fValue;
    fResult += (float)0.0742610;
    fResult *= fValue;
    fResult -= (float)0.2121144;
    fResult *= fValue;
    fResult += (float)1.5707288;
    fResult *= fRoot;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvCos1 (float fValue)
{
    float fRoot = GenericMath::Sqrt(FAbs(((float)1.0)-fValue));
    float fResult = -(float)0.0012624911;
    fResult *= fValue;
    fResult += (float)0.0066700901;
    fResult *= fValue;
    fResult -= (float)0.0170881256;
    fResult *= fValue;
    fResult += (float)0.0308918810;
    fResult *= fValue;
    fResult -= (float)0.0501743046;
    fResult *= fValue;
    fResult += (float)0.0889789874;
    fResult *= fValue;
    fResult -= (float)0.2145988016;
    fResult *= fValue;
    fResult += (float)1.5707963050;
    fResult *= fRoot;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvTan0 (float fValue)
{
    float fVSqr = fValue*fValue;
    float fResult = (float)0.0208351;
    fResult *= fVSqr;
    fResult -= (float)0.085133;
    fResult *= fVSqr;
    fResult += (float)0.180141;
    fResult *= fVSqr;
    fResult -= (float)0.3302995;
    fResult *= fVSqr;
    fResult += (float)0.999866;
    fResult *= fValue;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastInvTan1 (float fValue)
{
    float fVSqr = fValue*fValue;
    float fResult = (float)0.0028662257;
    fResult *= fVSqr;
    fResult -= (float)0.0161657367;
    fResult *= fVSqr;
    fResult += (float)0.0429096138;
    fResult *= fVSqr;
    fResult -= (float)0.0752896400;
    fResult *= fVSqr;
    fResult += (float)0.1065626393;
    fResult *= fVSqr;
    fResult -= (float)0.1420889944;
    fResult *= fVSqr;
    fResult += (float)0.1999355085;
    fResult *= fVSqr;
    fResult -= (float)0.3333314528;
    fResult *= fVSqr;
    fResult += (float)1.0;
    fResult *= fValue;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastNegExp0 (float fValue)
{
    float fResult = (float)0.0038278;
    fResult *= fValue;
    fResult += (float)0.0292732;
    fResult *= fValue;
    fResult += (float)0.2507213;
    fResult *= fValue;
    fResult += (float)1.0;
    fResult *= fResult;
    fResult *= fResult;
    fResult = ((float)1.0)/fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastNegExp1 (float fValue)
{
    float fResult = (float)0.00026695;
    fResult *= fValue;
    fResult += (float)0.00227723;
    fResult *= fValue;
    fResult += (float)0.03158565;
    fResult *= fValue;
    fResult += (float)0.24991035;
    fResult *= fValue;
    fResult += (float)1.0;
    fResult *= fResult;
    fResult *= fResult;
    fResult = ((float)1.0)/fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastNegExp2 (float fValue)
{
    float fResult = (float)0.000014876;
    fResult *= fValue;
    fResult += (float)0.000127992;
    fResult *= fValue;
    fResult += (float)0.002673255;
    fResult *= fValue;
    fResult += (float)0.031198056;
    fResult *= fValue;
    fResult += (float)0.250010936;
    fResult *= fValue;
    fResult += (float)1.0;
    fResult *= fResult;
    fResult *= fResult;
    fResult = ((float)1.0)/fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline float GenericMath::FastNegExp3 (float fValue)
{
    float fResult = (float)0.0000006906;
    fResult *= fValue;
    fResult += (float)0.0000054302;
    fResult *= fValue;
    fResult += (float)0.0001715620;
    fResult *= fValue;
    fResult += (float)0.0025913712;
    fResult *= fValue;
    fResult += (float)0.0312575832;
    fResult *= fValue;
    fResult += (float)0.2499986842;
    fResult *= fValue;
    fResult += (float)1.0;
    fResult *= fResult;
    fResult *= fResult;
    fResult = ((float)1.0)/fResult;
    return fResult;
}
//----------------------------------------------------------------------------
inline bool GenericMath::floatEqual(float a, float b, float tolerance)
{
	if (fabs(b-a) <= tolerance)
        return true;
    else
        return false;
}
