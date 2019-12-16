
#include "Math/GenericMath.h"
#include "Math/Plane.h"
#include "Math/Ray.h"
#include "Math/Sphere.h"
#include "Math/AxisAlignedBox.h"

DECLARE_ENGINE_NAMESPACE

const Real GenericMath::DEG_TO_RAD = PI / (Real)(180.0);
const Real GenericMath::RAD_TO_DEG = (Real)(180.0) / PI;

const Real GenericMath::TWO_PI = 2.0f*PI;
const Real GenericMath::HALF_PI = 0.5f*PI;
const Real GenericMath::INV_PI = 1.0f/PI;
const Real GenericMath::INV_TWO_PI = 1.0f/PI;
const Real GenericMath::LN_2 = GenericMath::Log(2.0f);
const Real GenericMath::LN_10 = GenericMath::Log(10.0f);
const Real GenericMath::INV_LN_2 = 1.0f/GenericMath::LN_2;
const Real GenericMath::INV_LN_10 = 1.0f/GenericMath::LN_10;

const Real GenericMath::POSITIVE_INFINITY = std::numeric_limits<Real>::infinity();
const Real GenericMath::NEGATIVE_INFINITY = -std::numeric_limits<Real>::infinity();

Radian GenericMath::ConvertDegreeToRadian(Degree deg)
{
	return Radian(deg.GetValue() * DEG_TO_RAD);
}

Degree GenericMath::ConvertRadianToDegree(Radian rad)
{
	return Degree(rad.GetValue() * RAD_TO_DEG);
}

std::pair<bool, Real> GenericMath::Intersects(const Ray& ray, const Plane& plane)
{

    Real denom = plane.normal.DotProduct(ray.GetDirection());
    if (GenericMath::FAbs(denom) < std::numeric_limits<Real>::epsilon())
    {
        // Parallel
        return std::pair<bool, Real>(false, 0);
    }
    else
    {
        Real nom = plane.normal.DotProduct(ray.GetOrigin()) + plane.d;
        Real t = -(nom/denom);
        return std::pair<bool, Real>(t >= 0, t);
    }
}

std::pair<bool, Real> GenericMath::Intersects(const Ray& ray, const Sphere& sphere, bool discardInside)
{
    const Vector3& raydir = ray.GetDirection();
    // Adjust Ray origin relative to Sphere center
    const Vector3& rayorig = ray.GetOrigin() - sphere.GetCenter();
    Real radius = sphere.GetRadius();

    // Check origin inside first
    if (rayorig.SquaredLength() <= radius*radius && discardInside)
    {
        return std::pair<bool, Real>(true, 0);
    }

    // Mmm, quadratics
    // Build coeffs which can be used with std quadratic solver
    // ie t = (-b +/- sqrt(b*b + 4ac)) / 2a
    Real a = raydir.DotProduct(raydir);
    Real b = 2 * rayorig.DotProduct(raydir);
    Real c = rayorig.DotProduct(rayorig) - radius*radius;

    // Calc determinant
    Real d = (b*b) - (4 * a * c);
    if (d < 0)
    {
        // No intersection
        return std::pair<bool, Real>(false, 0);
    }
    else
    {
        // BTW, if d=0 there is one intersection, if d > 0 there are 2
        // But we only want the closest one, so that's ok, just use the 
        // '-' version of the solver
        Real t = ( -b - GenericMath::Sqrt(d) ) / (2 * a);
        if (t < 0)
            t = ( -b + GenericMath::Sqrt(d) ) / (2 * a);
        return std::pair<bool, Real>(true, t);
    }
}

std::pair<bool, Real> GenericMath::Intersects(const Ray& ray, const AxisAlignedBox& box)
{
	if (box.IsNull()) 
		return std::pair<bool, Real>(false, 0);
	if (box.IsInfinite()) 
		return std::pair<bool, Real>(true, 0);

	Real lowt = 0.0f;
	Real t;
	bool hit = false;
	Vector3 hitpoint;
	const Vector3& min = box.GetMinimum();
	const Vector3& max = box.GetMaximum();
	const Vector3& rayorig = ray.GetOrigin();
	const Vector3& raydir = ray.GetDirection();

	// Check origin inside first
	if (rayorig > min && rayorig < max)
	{
		return std::pair<bool, Real>(true, 0);
	}

	// Check each face in turn, only check closest 3
	// Min x
	if (rayorig.x <= min.x && raydir.x > 0)
	{
		t = (min.x - rayorig.x) / raydir.x;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.y >= min.y && hitpoint.y <= max.y &&
				hitpoint.z >= min.z && hitpoint.z <= max.z &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}
	// Max x
	if (rayorig.x >= max.x && raydir.x < 0)
	{
		t = (max.x - rayorig.x) / raydir.x;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.y >= min.y && hitpoint.y <= max.y &&
				hitpoint.z >= min.z && hitpoint.z <= max.z &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}
	// Min y
	if (rayorig.y <= min.y && raydir.y > 0)
	{
		t = (min.y - rayorig.y) / raydir.y;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.x >= min.x && hitpoint.x <= max.x &&
				hitpoint.z >= min.z && hitpoint.z <= max.z &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}
	// Max y
	if (rayorig.y >= max.y && raydir.y < 0)
	{
		t = (max.y - rayorig.y) / raydir.y;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.x >= min.x && hitpoint.x <= max.x &&
				hitpoint.z >= min.z && hitpoint.z <= max.z &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}
	// Min z
	if (rayorig.z <= min.z && raydir.z > 0)
	{
		t = (min.z - rayorig.z) / raydir.z;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.x >= min.x && hitpoint.x <= max.x &&
				hitpoint.y >= min.y && hitpoint.y <= max.y &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}
	// Max z
	if (rayorig.z >= max.z && raydir.z < 0)
	{
		t = (max.z - rayorig.z) / raydir.z;
		if (t >= 0)
		{
			// Substitute t back into ray and check bounds and dist
			hitpoint = rayorig + raydir * t;
			if (hitpoint.x >= min.x && hitpoint.x <= max.x &&
				hitpoint.y >= min.y && hitpoint.y <= max.y &&
				(!hit || t < lowt))
			{
				hit = true;
				lowt = t;
			}
		}
	}

	return std::pair<bool, Real>(hit, lowt);
} 

bool GenericMath::Intersects(const Sphere& sphere, const AxisAlignedBox& box)
{
    if (box.IsNull()) 
		return false;
    if (box.IsInfinite()) 
		return true;

    // Use splitting planes
    const Vector3& center = sphere.GetCenter();
    Real radius = sphere.GetRadius();
    const Vector3& min = box.GetMinimum();
    const Vector3& max = box.GetMaximum();

	// Arvo's algorithm
	Real s, d = 0;
	for (int i = 0; i < 3; ++i)
	{
		if (center.ptr()[i] < min.ptr()[i])
		{
			s = center.ptr()[i] - min.ptr()[i];
			d += s * s; 
		}
		else if(center.ptr()[i] > max.ptr()[i])
		{
			s = center.ptr()[i] - max.ptr()[i];
			d += s * s; 
		}
	}
	return d <= radius * radius;
}

bool GenericMath::Intersects(const Plane& plane, const AxisAlignedBox& box)
{
    return (plane.GetSide(box) == Plane::BOTH_SIDE);
}

bool GenericMath::Intersects(const Sphere& sphere, const Plane& plane)
{
    return (GenericMath::FAbs(plane.GetDistance(sphere.GetCenter()))
			<= sphere.GetRadius() );
}

Degree Radian::ConvertToDegree() const
{
	return GenericMath::ConvertRadianToDegree(*this);
}

Radian Degree::ConvertToRadian() const
{
	return GenericMath::ConvertDegreeToRadian(*this);
}

END_ENGINE_NAMESPACE
