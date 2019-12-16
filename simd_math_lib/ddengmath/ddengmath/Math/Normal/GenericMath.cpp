
#include "GenericMath.h"

const float GenericMath::DEG_TO_RAD = PI / (float)(180.0);
const float GenericMath::RAD_TO_DEG = (float)(180.0) / PI;

const float GenericMath::TWO_PI = 2.0f*PI;
const float GenericMath::HALF_PI = 0.5f*PI;
const float GenericMath::INV_PI = 1.0f/PI;
const float GenericMath::INV_TWO_PI = 1.0f/PI;
const float GenericMath::LN_2 = GenericMath::Log(2.0f);
const float GenericMath::LN_10 = GenericMath::Log(10.0f);
const float GenericMath::INV_LN_2 = 1.0f/GenericMath::LN_2;
const float GenericMath::INV_LN_10 = 1.0f/GenericMath::LN_10;

const float GenericMath::POSITIVE_INFINITY = std::numeric_limits<float>::infinity();
const float GenericMath::NEGATIVE_INFINITY = -std::numeric_limits<float>::infinity();


Radian GenericMath::ConvertDegreeToRadian(Degree deg)
{
	return Radian(deg.GetValue() * DEG_TO_RAD);
}

Degree GenericMath::ConvertRadianToDegree(Radian rad)
{
	return Degree(rad.GetValue() * RAD_TO_DEG);
}

Degree Radian::ConvertToDegree() const
{
	return GenericMath::ConvertRadianToDegree(*this);
}

Radian Degree::ConvertToRadian() const
{
	return GenericMath::ConvertDegreeToRadian(*this);
}
