
#include "Math/Plane.h"
#include "Math/Matrix3.h"
#include "Math/AxisAlignedBox.h"

DECLARE_ENGINE_NAMESPACE

//-----------------------------------------------------------------------
Plane::Plane()
{
	normal = Vector3::ZERO;
	d = 0.0;
}
//-----------------------------------------------------------------------
Plane::Plane(const Plane& rhs)
{
	normal = rhs.normal;
	d = rhs.d;
}
//-----------------------------------------------------------------------
Plane::Plane(const Vector3& rkNormal, Real fConstant)
{
	normal = rkNormal;
	d = -fConstant;
}
//-----------------------------------------------------------------------
Plane::Plane(const Vector3& rkNormal, const Vector3& rkPoint)
{
	Redefine(rkNormal, rkPoint);
}
//-----------------------------------------------------------------------
Plane::Plane(const Vector3& rkPoint0, const Vector3& rkPoint1, const Vector3& rkPoint2)
{
	Redefine(rkPoint0, rkPoint1, rkPoint2);
}
//-----------------------------------------------------------------------
Real Plane::GetDistance(const Vector3& rkPoint) const
{
	return normal.DotProduct(rkPoint) + d;
}
//-----------------------------------------------------------------------
Plane::Side Plane::GetSide(const Vector3& rkPoint) const
{
	Real fDistance = GetDistance(rkPoint);

	if ( fDistance < 0.0 )
		return Plane::NEGATIVE_SIDE;

	if ( fDistance > 0.0 )
		return Plane::POSITIVE_SIDE;

	return Plane::NO_SIDE;
}
//-----------------------------------------------------------------------
Plane::Side Plane::GetSide(const AxisAlignedBox& box) const
{
	if (box.IsNull()) 
		return NO_SIDE;
	if (box.IsInfinite())
		return BOTH_SIDE;

    return GetSide(box.GetCenter(), box.GetHalfSize());
}

//-----------------------------------------------------------------------
Plane::Side Plane::GetSide(const Vector3& centre, const Vector3& halfSize) const
{
    // Calculate the distance between box centre and the Plane
    Real dist = GetDistance(centre);

    // Calculate the maximise allows absolute distance for
    // the distance between box centre and Plane
    Real maxAbsDist = normal.AbsDotProduct(halfSize);

    if (dist < -maxAbsDist)
        return Plane::NEGATIVE_SIDE;

    if (dist > +maxAbsDist)
        return Plane::POSITIVE_SIDE;

    return Plane::BOTH_SIDE;
}
//-----------------------------------------------------------------------
void Plane::Redefine(const Vector3& rkPoint0, const Vector3& rkPoint1, const Vector3& rkPoint2)
{
	Vector3 kEdge1 = rkPoint1 - rkPoint0;
	Vector3 kEdge2 = rkPoint2 - rkPoint0;
	normal = kEdge1.CrossProduct(kEdge2);
	normal.Normalise();
	d = -normal.DotProduct(rkPoint0);
}
//-----------------------------------------------------------------------
void Plane::Redefine(const Vector3& rkNormal, const Vector3& rkPoint)
{
	normal = rkNormal;
	d = -rkNormal.DotProduct(rkPoint);
}
//-----------------------------------------------------------------------
Vector3 Plane::ProjectVector(const Vector3& p) const
{
	// We know Plane normal is unit length, so use simple method
	Matrix3 xform;
	xform[0][0] = 1.0f - normal.x * normal.x;
	xform[0][1] = -normal.x * normal.y;
	xform[0][2] = -normal.x * normal.z;
	xform[1][0] = -normal.y * normal.x;
	xform[1][1] = 1.0f - normal.y * normal.y;
	xform[1][2] = -normal.y * normal.z;
	xform[2][0] = -normal.z * normal.x;
	xform[2][1] = -normal.z * normal.y;
	xform[2][2] = 1.0f - normal.z * normal.z;
	return xform * p;

}
//-----------------------------------------------------------------------
Real Plane::Normalise(void)
{
    Real fLength = normal.Length();

    // Will also work for zero-sized vectors, but will change nothing
    if (fLength > 1e-08f)
    {
        Real fInvLength = 1.0f / fLength;
        normal *= fInvLength;
        d *= fInvLength;
    }

    return fLength;
}
//-----------------------------------------------------------------------
std::ostream& operator<<(std::ostream& o, const Plane& p)
{
	o << "Plane(normal=" << p.normal << ", d=" << p.d << ")";
	return o;
}

END_ENGINE_NAMESPACE
