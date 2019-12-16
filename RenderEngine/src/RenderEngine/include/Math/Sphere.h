
#ifndef _SPHERE_H
#define _SPHERE_H

#include "Prerequisites.h"
#include "Math/Vector3.h"
#include "Math/AxisAlignedBox.h"

DECLARE_ENGINE_NAMESPACE

class Plane;
/** \ingroup Maths
A Sphere primitive, mostly used for bounds checking. 
  @remarks
  A Sphere in math texts is normally represented by the function
  x^2 + y^2 + z^2 = r^2 (for Sphere's centered on the origin). Ogre stores spheres
  simply as a center point and a radius.
 */
class /*shared*/ Sphere
{
	protected:
		Real mRadius;
		Vector3 mCenter;
	public:
		/** Standard constructor - creates a unit Sphere around the origin.*/
		Sphere() 
			: mRadius(1.0), mCenter(Vector3::ZERO)
		{}
		/** Constructor allowing arbitrary spheres. 
		  @param center The center point of the Sphere.
		  @param radius The radius of the Sphere.
		 */
		Sphere(const Vector3& center, Real radius)
			: mRadius(radius), mCenter(center)
		{}

		/** Returns the radius of the Sphere. */
		Real GetRadius(void) const { return mRadius; }

		/** Sets the radius of the Sphere. */
		void SetRadius(Real radius) { mRadius = radius; }

		/** Returns the center point of the Sphere. */
		const Vector3& GetCenter(void) const { return mCenter; }

		/** Sets the center point of the Sphere. */
		void SetCenter(const Vector3& center) { mCenter = center; }

		/** Returns whether or not this Sphere interects another Sphere. */
		bool Intersects(const Sphere& s) const
		{
			return (s.mCenter - mCenter).SquaredLength() <=
					GenericMath::Sqr(s.mRadius + mRadius);
		}
		/** Returns whether or not this Sphere interects a box. */
		bool Intersects(const AxisAlignedBox& box) const
		{
			return GenericMath::Intersects(*this, box);
		}
		/** Returns whether or not this Sphere interects a Plane. */
		bool Intersects(const Plane& plane) const
		{
			return GenericMath::Intersects(*this, plane);
		}
		/** Returns whether or not this Sphere interects a point. */
		bool Intersects(const Vector3& v) const
		{
			return ((v - mCenter).SquaredLength() <= GenericMath::Sqr(mRadius));
		}
};

END_ENGINE_NAMESPACE

#endif