
#ifndef _PLANE_H
#define _PLANE_H

#include "Prerequisites.h"
#include "Math/Vector3.h"

DECLARE_ENGINE_NAMESPACE

class AxisAlignedBox;
/** \ingroup Maths
Defines a Plane in 3D space.
  @remarks
  A Plane is defined in 3D space by the equation
  Ax + By + Cz + D = 0
  @par
  This equates to a vector (the normal of the Plane, whose x, y
  and z components equate to the coefficients A, B and C
  respectively), and a constant (D) which is the distance along
  the normal you have to go to move the Plane back to the origin.
 */
class /*shared*/ Plane
{
	public:
		/** Default constructor - sets everything to 0.
		 */
		Plane ();
		Plane (const Plane& rhs);
		/** Construct a Plane through a normal, and a distance to move the Plane along the normal.*/
		Plane (const Vector3& rkNormal, Real fConstant);
		Plane (const Vector3& rkNormal, const Vector3& rkPoint);
		Plane (const Vector3& rkPoint0, const Vector3& rkPoint1,
				const Vector3& rkPoint2);

		/** The "positive side" of the Plane is the half space to which the
		  Plane normal points. The "negative side" is the other half
		  space. The flag "no side" indicates the Plane itself.
		 */
		enum Side
		{
			NO_SIDE = 0,
			POSITIVE_SIDE = 1,
			NEGATIVE_SIDE = -1,
			BOTH_SIDE = 2
		};

		Side GetSide (const Vector3& rkPoint) const;

		Side GetSide (const AxisAlignedBox& box) const;

		/** Returns which side of the Plane that the given box lies on.
		  The box is defined as centre/half-size pairs for effectively.
		  @param centre The centre of the box.
		  @param halfSize The half-size of the box.
		  @returns
		  POSITIVE_SIDE if the box complete lies on the "positive side" of the Plane,
		  NEGATIVE_SIDE if the box complete lies on the "negative side" of the Plane,
		  and BOTH_SIDE if the box intersects the Plane.
		 */
		Side GetSide (const Vector3& centre, const Vector3& halfSize) const;

		/** This is a pseudodistance. The sign of the return value is
		  positive if the point is on the positive side of the Plane,
		  negative if the point is on the negative side, and zero if the
		  point is on the Plane.
		  @par
		  The absolute value of the return value is the true distance only
		  when the Plane normal is a unit length vector.
		 */
		Real GetDistance (const Vector3& rkPoint) const;

		/** Redefine this Plane based on 3 points. */
		void Redefine(const Vector3& rkPoint0, const Vector3& rkPoint1, const Vector3& rkPoint2);

		/** Redefine this Plane based on a normal and a point. */
		void Redefine(const Vector3& rkNormal, const Vector3& rkPoint);

		/** Project a vector onto the Plane. 
		  @remarks This gives you the element of the input vector that is perpendicular 
		  to the normal of the Plane. You can get the element which is parallel
		  to the normal of the Plane by subtracting the result of this method
		  from the original vector, since parallel + perpendicular = original.
		  @param v The input vector
		 */
		Vector3 ProjectVector(const Vector3& v) const;

		/** Normalises the Plane.
		  @remarks
		  This method normalises the Plane's normal and the length scale of d
		  is as well.
		  @note
		  This function will not crash for zero-sized vectors, but there
		  will be no changes made to their components.
		  @returns The previous length of the Plane's normal.
		 */
		Real Normalise(void);

		Vector3 normal;
		Real d;

		/// Comparison operator
		bool operator==(const Plane& rhs) const
		{
			return (rhs.d == d && rhs.normal == normal);
		}
		bool operator!=(const Plane& rhs) const
		{
			return (rhs.d != d && rhs.normal != normal);
		}
		bool IsEquals(const Plane& rhs) const
		{
			return (rhs.d == d && rhs.normal == normal);
		}
		bool NotEquals(const Plane& rhs) const
		{
			return (rhs.d != d && rhs.normal != normal);
		}

		/*shared*/ friend std::ostream& operator<< (std::ostream& o, const Plane& p);
};


END_ENGINE_NAMESPACE

#endif