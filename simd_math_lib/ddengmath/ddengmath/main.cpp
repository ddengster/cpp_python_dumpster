
#include <iostream>
#include "test_vector2.h"
#include "test_vector3.h"
#include "test_vector4.h"
#include "test_matrix3.h"
#include "test_matrix4.h"
#include "test_quaternion.h"
#include "test_aabb2d.h"
#include "test_aabb3d.h"
#include "test_ray3d.h"
#include "test_sphere.h"
#include "test_plane.h"
#include "test_frustum.h"

#include <pmmintrin.h>

using namespace std;

int main()
{
  /**Vector2 test**/
  //Vector2AllTests();
  /**Vector3 test**/
  //Vector3AllTests();
  /**Vector4 test**/
  //Vector4AllTests();

  /**Matrix3 test**/
  //Matrix3AllTests();
  /**Matrix4 test**/
  //Matrix4AllTests();
  /**Quarternion test**/
  //QuaternionAllTests();

  /**AABB2d test**/
  //AABB2DAllTests();

  /**AABB3d test**/
  //AABB3DAllTests();

  /** Ray3D test **/
  //Ray3DAllTests();

  /** Sphere test **/
  //SphereAllTests();

  /** Plane test **/
  //PlaneAllTests();

  /** Frustum test **/
  //FrustumAllTests();

  /**Simd**/
#if 0
  __declspec(align(16)) float val[4] = { 1.0, 2.0, 3.0, 4.0f };
  __m128 a = _mm_load_ps(val); // a contains val
  __declspec(align(16)) float val2[4] = { 0.1f };
  __m128 b = _mm_load_ps(val2); //b contains 0.1f for each float val

  cout << sizeof(__m128) << endl;
  __declspec(align(16))float retval[4] = { 0.0f };
  __m128* pVal = (__m128*)retval;
  __m128 ret;
  *pVal = _mm_add_ps(a, b);

  cout << val[0] << " " << val[1] << " " << val[2] << " " << val[3] << endl;
  cout << val2[0] << " " << val2[1] << " " << val2[2] << " " << val2[3] << endl;
  cout << retval[0] << " " << retval[1] << " " << retval[2] << " " << retval[3] << endl;
#endif

  LogProfileData();
  return 0;
}
