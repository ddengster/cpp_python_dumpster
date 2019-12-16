
#ifndef _RAY_H
#define _RAY_H

#include "Prerequisites.h"
#include "Math/GenericMath.h"
#include "Math/Vector3.h"

DECLARE_ENGINE_NAMESPACE

class Plane;
class AxisAlignedBox;
/** \ingroup Maths
Representation of a Ray in space, ie a line with an origin and direction. 
*/
class /*shared*/ Ray
{
	protected:
		Vector3 mOrigin;
		Vector3 mDirection;
	public:
		Ray()
			:mOrigin(Vector3::ZERO), mDirection(Vector3::UNIT_Z)
		{}
		Ray(const Vector3& origin, const Vector3& direction)
			:mOrigin(origin), mDirection(direction)
		{}

		/** Sets the origin of the Ray. */
		void SetOrigin(const Vector3& origin) { mOrigin = origin; } 
		/** Gets the origin of the Ray. */
		const Vector3& GetOrigin(void) const { return mOrigin; } 

		/** Sets the direction of the Ray. */
		void SetDirection(const Vector3& dir) { mDirection = dir; } 
		/** Gets the direction of the Ray. */
		const Vector3& GetDirection(void) const { return mDirection; } 

		/** Gets the position of a point t units along the Ray. */
		Vector3 GetPoint(Real t) const 
		{ 
			return Vector3(mOrigin + (mDirection * t));
		}

		/** Gets the position of a point t units along the Ray. */
		Vector3 operator*(Real t) const
		{ 
			return GetPoint(t);
		};

		/** Tests whether this Ray intersects the given Plane. 
		  @returns A pair structure where the first element indicates whether
		  an intersection occurs, and if true, the second element will
		  indicate the distance along the Ray at which it intersects. 
		  This can be converted to a point in space by calling getPoint().
		 */
		std::pair<bool, Real> Intersects(const Plane& p) const
		{
			return GenericMath::Intersects(*this, p);
		}
		/** Tests whether this Ray intersects the given Sphere. 
		  @returns A pair structure where the first element indicates whether
		  an intersection occurs, and if true, the second element will
		  indicate the distance along the Ray at which it intersects. 
		  This can be converted to a point in space by calling getPoint().
		 */
		std::pair<bool, Real> Intersects(const Sphere& s) const
		{
			return GenericMath::Intersects(*this, s);
		}
		/** Tests whether this Ray intersects the given box. 
		  @returns A pair structure where the first element indicates whether
		  an intersection occurs, and if true, the second element will
		  indicate the distance along the Ray at which it intersects. 
		  This can be converted to a point in space by calling getPoint().
		 */
		std::pair<bool, Real> Intersects(const AxisAlignedBox& box) const
		{
			return GenericMath::Intersects(*this, box);
		}
};

END_ENGINE_NAMESPACE

#endif
