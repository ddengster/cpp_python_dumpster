
#include "Math/Vector2.h"
#include "Math/Vector3.h"
#include "Math/Vector4.h"

DECLARE_ENGINE_NAMESPACE

/** Vector2 constants **/
const Vector2 Vector2::ZERO(0, 0);
const Vector2 Vector2::UNIT_X(1, 0);
const Vector2 Vector2::UNIT_Y(0, 1);
const Vector2 Vector2::NEGATIVE_UNIT_X(-1,  0);
const Vector2 Vector2::NEGATIVE_UNIT_Y( 0, -1);
const Vector2 Vector2::UNIT_SCALE(1, 1);

/** Vector3 constants **/
const Vector3 Vector3::ZERO( 0, 0, 0 );
const Vector3 Vector3::UNIT_X( 1, 0, 0 );
const Vector3 Vector3::UNIT_Y( 0, 1, 0 );
const Vector3 Vector3::UNIT_Z( 0, 0, 1 );
const Vector3 Vector3::NEGATIVE_UNIT_X( -1,  0,  0 );
const Vector3 Vector3::NEGATIVE_UNIT_Y(  0, -1,  0 );
const Vector3 Vector3::NEGATIVE_UNIT_Z(  0,  0, -1 );
const Vector3 Vector3::UNIT_SCALE(1, 1, 1);

/** Vector4 constants **/
const Vector4 Vector4::ZERO( 0, 0, 0, 0 );

END_ENGINE_NAMESPACE
