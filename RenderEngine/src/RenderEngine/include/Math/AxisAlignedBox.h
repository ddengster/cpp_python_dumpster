
#ifndef _AXISALIGNEDBOX_H
#define _AXISALIGNEDBOX_H

#include "Prerequisites.h"
#include "Math/Vector3.h"
#include "Math/Matrix4.h"
#include "Math/GenericMath.h"

DECLARE_ENGINE_NAMESPACE

class Plane;
/** \ingroup Maths
A 3D box aligned with the x/y/z axes.
@remarks
This class represents a simple box which is aligned with the
axes. Internally it only stores 2 points as the extremeties of
the box, one which is the minima of all 3 axes, and the other
which is the maxima of all 3 axes. This class is typically used
for an axis-aligned bounding box (AABB) for collision and
visibility determination.
*/
class AxisAlignedBox
{
protected:
	enum Extent
	{
		EXTENT_NULL,
		EXTENT_FINITE,
		EXTENT_INFINITE
	};

	Vector3 mMinimum;
	Vector3 mMaximum;
	Extent mExtent;
	mutable Vector3* mpCorners;

public:
	/*
	   1-----2
	  /|    /|
	 / |   / |
	5-----4  |
	|  0--|--3
	| /   | /
	|/    |/
	6-----7
	*/
	typedef enum {
		FAR_LEFT_BOTTOM = 0,
		FAR_LEFT_TOP = 1,
		FAR_RIGHT_TOP = 2,
		FAR_RIGHT_BOTTOM = 3,
		NEAR_RIGHT_BOTTOM = 7,
		NEAR_LEFT_BOTTOM = 6,
		NEAR_LEFT_TOP = 5,
		NEAR_RIGHT_TOP = 4
	} CornerEnum;

	inline AxisAlignedBox() : mpCorners(0)
	{
		// Default to a null box 
		SetMinimum( -0.5, -0.5, -0.5 );
		SetMaximum( 0.5, 0.5, 0.5 );
		mExtent = EXTENT_NULL;
	}

	inline AxisAlignedBox(const AxisAlignedBox & rkBox) : mpCorners(0)
	{
		if (rkBox.IsNull())
			SetNull();
		else if (rkBox.IsInfinite())
			SetInfinite();
		else
			SetExtents( rkBox.mMinimum, rkBox.mMaximum );
	}

	inline AxisAlignedBox(const Vector3& min, const Vector3& max) : mpCorners(0)
	{
		SetExtents( min, max );
	}

	inline AxisAlignedBox(	Real mx, Real my, Real mz,
							Real Mx, Real My, Real Mz )
		: mpCorners(0)
	{
		SetExtents( mx, my, mz, Mx, My, Mz );
	}

	AxisAlignedBox& operator=(const AxisAlignedBox& rhs)
	{
		// Specifically override to avoid copying mpCorners
		if (rhs.IsNull())
			SetNull();
		else if (rhs.IsInfinite())
			SetInfinite();
		else
			SetExtents(rhs.mMinimum, rhs.mMaximum);

		return *this;
	}

	~AxisAlignedBox()
	{
		if (mpCorners)
			delete [] mpCorners;
	}

	/** Gets the minimum corner of the box.
	*/
	inline const Vector3& GetMinimum(void) const
	{ 
		return mMinimum; 
	}

	/** Gets a modifiable version of the minimum
	corner of the box.
	*/
	inline Vector3& GetMinimum(void)
	{ 
		return mMinimum; 
	}

	/** Gets the maximum corner of the box.
	*/
	inline const Vector3& GetMaximum(void) const
	{ 
		return mMaximum;
	}

	/** Gets a modifiable version of the maximum
	corner of the box.
	*/
	inline Vector3& GetMaximum(void)
	{ 
		return mMaximum;
	}

	/** Sets the minimum corner of the box.
	*/
	inline void SetMinimum(const Vector3& vec)
	{
		mExtent = EXTENT_FINITE;
		mMinimum = vec;
	}

	inline void SetMinimum(Real x, Real y, Real z)
	{
		mExtent = EXTENT_FINITE;
		mMinimum.x = x;
		mMinimum.y = y;
		mMinimum.z = z;
	}

	/** Changes one of the components of the minimum corner of the box
	used to resize only one dimension of the box
	*/
	inline void SetMinimumX(Real x)
	{
		mMinimum.x = x;
	}

	inline void SetMinimumY(Real y)
	{
		mMinimum.y = y;
	}

	inline void SetMinimumZ(Real z)
	{
		mMinimum.z = z;
	}

	/** Sets the maximum corner of the box.
	*/
	inline void SetMaximum(const Vector3& vec)
	{
		mExtent = EXTENT_FINITE;
		mMaximum = vec;
	}

	inline void SetMaximum(Real x, Real y, Real z)
	{
		mExtent = EXTENT_FINITE;
		mMaximum.x = x;
		mMaximum.y = y;
		mMaximum.z = z;
	}

	/** Changes one of the components of the maximum corner of the box
	used to resize only one dimension of the box
	*/
	inline void SetMaximumX(Real x)
	{
		mMaximum.x = x;
	}

	inline void SetMaximumY(Real y)
	{
		mMaximum.y = y;
	}

	inline void SetMaximumZ(Real z)
	{
		mMaximum.z = z;
	}

	/** Sets both minimum and maximum extents at once.
	*/
	inline void SetExtents(const Vector3& min, const Vector3& max)
	{
        assert( (min.x <= max.x && min.y <= max.y && min.z <= max.z) &&
            "The minimum corner of the box must be less than or equal to maximum corner" );

		mExtent = EXTENT_FINITE;
		mMinimum = min;
		mMaximum = max;
	}

	inline void SetExtents(	Real mx, Real my, Real mz,
							Real Mx, Real My, Real Mz)
	{
        assert( (mx <= Mx && my <= My && mz <= Mz) &&
            "The minimum corner of the box must be less than or equal to maximum corner" );

		mExtent = EXTENT_FINITE;

		mMinimum.x = mx;
		mMinimum.y = my;
		mMinimum.z = mz;

		mMaximum.x = Mx;
		mMaximum.y = My;
		mMaximum.z = Mz;
	}

	/** Returns a pointer to an array of 8 corner points, useful for
	collision vs. non-aligned objects.
	@remarks
	If the order of these corners is important, they are as
	follows: The 4 points of the minimum Z face (note that
	because Ogre uses right-handed coordinates, the minimum Z is
	at the 'back' of the box) starting with the minimum point of
	all, then anticlockwise around this face (if you are looking
	onto the face from outside the box). Then the 4 points of the
	maximum Z face, starting with maximum point of all, then
	anticlockwise around this face (looking onto the face from
	outside the box). Like this:
	<pre>
	   1-----2
	  /|    /|
	 / |   / |
	5-----4  |
	|  0--|--3
	| /   | /
	|/    |/
	6-----7
	</pre>
	@remarks as this implementation uses a static member, make sure to use your own copy !
	*/
	inline const Vector3* GetAllCorners(void) const
	{
		assert( (mExtent == EXTENT_FINITE) && "Can't get corners of a null or infinite AAB" );

		// The order of these items is, using right-handed co-ordinates:
		// Minimum Z face, starting with Min(all), then anticlockwise
		//   around face (looking onto the face)
		// Maximum Z face, starting with Max(all), then anticlockwise
		//   around face (looking onto the face)
		// Only for optimization/compatibility.
		if (!mpCorners)
			mpCorners = new Vector3[8];

		mpCorners[0] = mMinimum;
		mpCorners[1].x = mMinimum.x; mpCorners[1].y = mMaximum.y; mpCorners[1].z = mMinimum.z;
		mpCorners[2].x = mMaximum.x; mpCorners[2].y = mMaximum.y; mpCorners[2].z = mMinimum.z;
		mpCorners[3].x = mMaximum.x; mpCorners[3].y = mMinimum.y; mpCorners[3].z = mMinimum.z;            

		mpCorners[4] = mMaximum;
		mpCorners[5].x = mMinimum.x; mpCorners[5].y = mMaximum.y; mpCorners[5].z = mMaximum.z;
		mpCorners[6].x = mMinimum.x; mpCorners[6].y = mMinimum.y; mpCorners[6].z = mMaximum.z;
		mpCorners[7].x = mMaximum.x; mpCorners[7].y = mMinimum.y; mpCorners[7].z = mMaximum.z;

		return mpCorners;
	}

	/** gets the position of one of the corners
	*/
	Vector3 GetCorner(CornerEnum cornerToGet) const
	{
		switch(cornerToGet)
		{
		case FAR_LEFT_BOTTOM:
			return mMinimum;
		case FAR_LEFT_TOP:
			return Vector3(mMinimum.x, mMaximum.y, mMinimum.z);
		case FAR_RIGHT_TOP:
			return Vector3(mMaximum.x, mMaximum.y, mMinimum.z);
		case FAR_RIGHT_BOTTOM:
			return Vector3(mMaximum.x, mMinimum.y, mMinimum.z);
		case NEAR_RIGHT_BOTTOM:
			return Vector3(mMaximum.x, mMinimum.y, mMaximum.z);
		case NEAR_LEFT_BOTTOM:
			return Vector3(mMinimum.x, mMinimum.y, mMaximum.z);
		case NEAR_LEFT_TOP:
			return Vector3(mMinimum.x, mMaximum.y, mMaximum.z);
		case NEAR_RIGHT_TOP:
			return mMaximum;
		default:
			return Vector3();
		}
	}

	friend std::ostream& operator<< (std::ostream& o, const AxisAlignedBox aab)
	{
		switch (aab.mExtent)
		{
		case EXTENT_NULL:
			o << "AxisAlignedBox(null)";
			return o;

		case EXTENT_FINITE:
			o << "AxisAlignedBox(min=" << aab.mMinimum << ", max=" << aab.mMaximum << ")";
			return o;

		case EXTENT_INFINITE:
			o << "AxisAlignedBox(infinite)";
			return o;

		default: // shut up compiler
			assert( false && "Never reached" );
			return o;
		}
	}

	/** Merges the passed in box into the current box. The result is the
	box which encompasses both.
	*/
	void Merge(const AxisAlignedBox& rhs)
	{
		// Do nothing if rhs null, or this is infinite
		if ((rhs.mExtent == EXTENT_NULL) || (mExtent == EXTENT_INFINITE))
		{
			return;
		}
		// Otherwise if rhs is infinite, make this infinite, too
		else if (rhs.mExtent == EXTENT_INFINITE)
		{
			mExtent = EXTENT_INFINITE;
		}
		// Otherwise if current null, just take rhs
		else if (mExtent == EXTENT_NULL)
		{
			SetExtents(rhs.mMinimum, rhs.mMaximum);
		}
		// Otherwise merge
		else
		{
			Vector3 min = mMinimum;
			Vector3 max = mMaximum;
			max.MakeCeil(rhs.mMaximum);
			min.MakeFloor(rhs.mMinimum);

			SetExtents(min, max);
		}

	}

	/** Extends the box to encompass the specified point (if needed).
	*/
	inline void Merge(const Vector3& point)
	{
		switch (mExtent)
		{
		case EXTENT_NULL: // if null, use this point
			SetExtents(point, point);
			return;

		case EXTENT_FINITE:
			mMaximum.MakeCeil(point);
			mMinimum.MakeFloor(point);
			return;

		case EXTENT_INFINITE: // if infinite, makes no difference
			return;
		}

		assert( false && "Never reached" );
	}

	/** Transforms the box according to the matrix supplied.
	@remarks
	By calling this method you get the axis-aligned box which
	surrounds the transformed version of this box. Therefore each
	corner of the box is transformed by the matrix, then the
	extents are mapped back onto the axes to produce another
	AABB. Useful when you have a local AABB for an object which
	is then transformed.
	*/
	inline void Transform(const Matrix4& matrix)
	{
		// Do nothing if current null or infinite
		if( mExtent != EXTENT_FINITE )
			return;

		Vector3 oldMin, oldMax, currentCorner;

		// Getting the old values so that we can use the existing merge method.
		oldMin = mMinimum;
		oldMax = mMaximum;

		// reset
		SetNull();

		// We sequentially compute the corners in the following order :
		// 0, 6, 5, 1, 2, 4 ,7 , 3
		// This sequence allows us to only change one member at a time to get at all corners.

		// For each one, we transform it using the matrix
		// Which gives the resulting point and merge the resulting point.

		// First corner 
		// min min min
		currentCorner = oldMin;
		Merge( matrix * currentCorner );

		// min,min,max
		currentCorner.z = oldMax.z;
		Merge( matrix * currentCorner );

		// min max max
		currentCorner.y = oldMax.y;
		Merge( matrix * currentCorner );

		// min max min
		currentCorner.z = oldMin.z;
		Merge( matrix * currentCorner );

		// max max min
		currentCorner.x = oldMax.x;
		Merge( matrix * currentCorner );

		// max max max
		currentCorner.z = oldMax.z;
		Merge( matrix * currentCorner );

		// max min max
		currentCorner.y = oldMin.y;
		Merge( matrix * currentCorner );

		// max min min
		currentCorner.z = oldMin.z;
		Merge( matrix * currentCorner ); 
	}

	/** Transforms the box according to the affine matrix supplied.
	@remarks
	By calling this method you get the axis-aligned box which
	surrounds the transformed version of this box. Therefore each
	corner of the box is transformed by the matrix, then the
	extents are mapped back onto the axes to produce another
	AABB. Useful when you have a local AABB for an object which
	is then transformed.
	@note
	The matrix must be an affine matrix. @see Matrix4::isAffine.
	*/
	void TransformAffine(const Matrix4& m)
	{
		assert(m.IsAffine());

		// Do nothing if current null or infinite
		if ( mExtent != EXTENT_FINITE )
			return;

		Vector3 centre = GetCenter();
		Vector3 halfSize = GetHalfSize();

		Vector3 newCentre = m.TransformAffine(centre);
		Vector3 newHalfSize(
			GenericMath::FAbs(m[0][0]) * halfSize.x + GenericMath::FAbs(m[0][1]) * halfSize.y + GenericMath::FAbs(m[0][2]) * halfSize.z, 
			GenericMath::FAbs(m[1][0]) * halfSize.x + GenericMath::FAbs(m[1][1]) * halfSize.y + GenericMath::FAbs(m[1][2]) * halfSize.z,
			GenericMath::FAbs(m[2][0]) * halfSize.x + GenericMath::FAbs(m[2][1]) * halfSize.y + GenericMath::FAbs(m[2][2]) * halfSize.z);

		SetExtents(newCentre - newHalfSize, newCentre + newHalfSize);
	}

	/** Sets the box to a 'null' value i.e. not a box.
	*/
	inline void SetNull()
	{
		mExtent = EXTENT_NULL;
	}

	/** Returns true if the box is null i.e. empty.
	*/
	inline bool IsNull(void) const
	{
		return (mExtent == EXTENT_NULL);
	}

	/** Returns true if the box is finite.
	*/
	bool IsFinite(void) const
	{
		return (mExtent == EXTENT_FINITE);
	}

	/** Sets the box to 'infinite'
	*/
	inline void SetInfinite()
	{
		mExtent = EXTENT_INFINITE;
	}

	/** Returns true if the box is infinite.
	*/
	bool IsInfinite(void) const
	{
		return (mExtent == EXTENT_INFINITE);
	}

	/** Returns whether or not this box intersects another. */
	inline bool Intersects(const AxisAlignedBox& b2) const
	{
		// Early-fail for nulls
		if (this->IsNull() || b2.IsNull())
			return false;

		// Early-success for infinites
		if (this->IsInfinite() || b2.IsInfinite())
			return true;

		// Use up to 6 separating planes
		if (mMaximum.x < b2.mMinimum.x)
			return false;
		if (mMaximum.y < b2.mMinimum.y)
			return false;
		if (mMaximum.z < b2.mMinimum.z)
			return false;

		if (mMinimum.x > b2.mMaximum.x)
			return false;
		if (mMinimum.y > b2.mMaximum.y)
			return false;
		if (mMinimum.z > b2.mMaximum.z)
			return false;

		// otherwise, must be intersecting
		return true;

	}

	/// Calculate the area of intersection of this box and another
	inline AxisAlignedBox Intersection(const AxisAlignedBox& b2) const
	{
        if (this->IsNull() || b2.IsNull())
		{
			return AxisAlignedBox();
		}
		else if (this->IsInfinite())
		{
			return b2;
		}
		else if (b2.IsInfinite())
		{
			return *this;
		}

		Vector3 intMin = mMinimum;
        Vector3 intMax = mMaximum;

        intMin.MakeCeil(b2.GetMinimum());
        intMax.MakeFloor(b2.GetMaximum());

        // Check intersection isn't null
        if (intMin.x < intMax.x &&
            intMin.y < intMax.y &&
            intMin.z < intMax.z)
        {
            return AxisAlignedBox(intMin, intMax);
        }

        return AxisAlignedBox();
	}

	/// Calculate the volume of this box
	Real volume(void) const
	{
		switch (mExtent)
		{
		case EXTENT_NULL:
			return 0.0f;

		case EXTENT_FINITE:
			{
				Vector3 diff = mMaximum - mMinimum;
				return diff.x * diff.y * diff.z;
			}

		case EXTENT_INFINITE:
			return GenericMath::POSITIVE_INFINITY;

		default: // shut up compiler
			assert( false && "Never reached" );
			return 0.0f;
		}
	}

	/** Scales the AABB by the vector given. */
	inline void Scale(const Vector3& s)
	{
		// Do nothing if current null or infinite
		if (mExtent != EXTENT_FINITE)
			return;

		// NB assumes centered on origin
		Vector3 min = mMinimum * s;
		Vector3 max = mMaximum * s;
		SetExtents(min, max);
	}

	/** Tests whether this box intersects a sphere. */
	/*
	TODO
	bool Intersects(const Sphere& s) const
	{
		return GenericMath::Intersects(s,*this); 
	}
	*/
	/** Tests whether this box intersects a plane. */
	/*
	TODO
	bool Intersects(const Plane& p) const
	{
		return GenericMath::Intersects(p, *this);
	}
	*/
	/** Tests whether the vector point is within this box. */
	bool Intersects(const Vector3& v) const
	{
		switch (mExtent)
		{
		case EXTENT_NULL:
			return false;

		case EXTENT_FINITE:
			return(v.x >= mMinimum.x  &&  v.x <= mMaximum.x  && 
				v.y >= mMinimum.y  &&  v.y <= mMaximum.y  && 
				v.z >= mMinimum.z  &&  v.z <= mMaximum.z);

		case EXTENT_INFINITE:
			return true;

		default: // shut up compiler
			assert( false && "Never reached" );
			return false;
		}
	}
	/// Gets the centre of the box
	Vector3 GetCenter(void) const
	{
		assert( (mExtent == EXTENT_FINITE) && "Can't get center of a null or infinite AAB" );

		return Vector3(
			(mMaximum.x + mMinimum.x) * 0.5,
			(mMaximum.y + mMinimum.y) * 0.5,
			(mMaximum.z + mMinimum.z) * 0.5);
	}
	/// Gets the size of the box
	Vector3 GetSize(void) const
	{
		switch (mExtent)
		{
		case EXTENT_NULL:
			return Vector3::ZERO;

		case EXTENT_FINITE:
			return mMaximum - mMinimum;

		case EXTENT_INFINITE:
			return Vector3(
				GenericMath::POSITIVE_INFINITY,
				GenericMath::POSITIVE_INFINITY,
				GenericMath::POSITIVE_INFINITY);

		default: // shut up compiler
			assert( false && "Never reached" );
			return Vector3::ZERO;
		}
	}
	/// Gets the half-size of the box
	Vector3 GetHalfSize(void) const
	{
		switch (mExtent)
		{
		case EXTENT_NULL:
			return Vector3::ZERO;

		case EXTENT_FINITE:
			return (mMaximum - mMinimum) * 0.5;

		case EXTENT_INFINITE:
			return Vector3(
				GenericMath::POSITIVE_INFINITY,
				GenericMath::POSITIVE_INFINITY,
				GenericMath::POSITIVE_INFINITY);

		default: // shut up compiler
			assert( false && "Never reached" );
			return Vector3::ZERO;
		}
	}

    /** Tests whether the given point contained by this box.
    */
    bool Contains(const Vector3& v) const
    {
        if (IsNull())
            return false;
        if (IsInfinite())
            return true;

        return mMinimum.x <= v.x && v.x <= mMaximum.x &&
               mMinimum.y <= v.y && v.y <= mMaximum.y &&
               mMinimum.z <= v.z && v.z <= mMaximum.z;
    }

    /** Tests whether another box contained by this box.
    */
    bool Contains(const AxisAlignedBox& other) const
    {
        if (other.IsNull() || this->IsInfinite())
            return true;

        if (this->IsNull() || other.IsInfinite())
            return false;

        return this->mMinimum.x <= other.mMinimum.x &&
               this->mMinimum.y <= other.mMinimum.y &&
               this->mMinimum.z <= other.mMinimum.z &&
               other.mMaximum.x <= this->mMaximum.x &&
               other.mMaximum.y <= this->mMaximum.y &&
               other.mMaximum.z <= this->mMaximum.z;
    }

    /** Tests 2 boxes for equality.
    */
    bool operator== (const AxisAlignedBox& rhs) const
    {
        if (this->mExtent != rhs.mExtent)
            return false;

        if (!this->IsFinite())
            return true;

        return this->mMinimum == rhs.mMinimum &&
               this->mMaximum == rhs.mMaximum;
    }

    /** Tests 2 boxes for inequality.
    */
    bool operator!= (const AxisAlignedBox& rhs) const
    {
        return !(*this == rhs);
    }
};

END_ENGINE_NAMESPACE

#endif