
#include "Math/MathHeaders.h"

using namespace ddengine_RenderEngine;
using namespace std;

int main()
{
	/******** Quaternion and Vector *********/
	std::cout << std::endl << std::endl << "/******** Quaternion and Vector Test*********/" << std::endl << std::endl;

	Vector3 v1(1,0,0);

	Quaternion q1 = v1.GetRotationTo(Vector3(0,1,1));

	std::cout << "the x axis angle to (0,1,1) from (1,0,0) is  " << q1.x << std::endl;
	std::cout << "the y axis angle to (0,1,1) from (1,0,0) is  " << q1.y << std::endl;
	std::cout << "the z axis angle to (0,1,1) from (1,0,0) is  " << q1.z << std::endl;
	std::cout << "the w to (0,1,1) is  " << q1.w << std::endl;


	system("PAUSE");

	/******** Matrices *********/
	std::cout << std::endl << std::endl << "/******** Matrices Test*********/" << std::endl << std::endl;

	Matrix4 m1(12,32,43,54,23,81,29,49,5,3,26,36,86,69,15,37);

	std::cout << "matrix m1 contains ... " << std::endl << m1 << std::endl;

	m1 = m1.Transpose();

	std::cout << "transposed m1 contains ... " << std::endl << m1 << std::endl;

	Real* array1 = new Real[16];

	m1.ToArray16(array1);

	std::cout << "first line of m1 is ... " << array1[0] << " " << array1[1] << " " 
											<< array1[2] << " " << array1[3] << std::endl;


	system("PAUSE");

	/******** Plane and Ray **********/
	std::cout << std::endl << std::endl << "/******** Plane and Ray Test**********/" << std::endl << std::endl;

	Plane plane1 (	Vector3(1,0,0),
					Vector3(0,0,0),
					Vector3(0,0,1));
	Ray ray1(Vector3(0,2,0), Vector3(0,-1,0));

	if ((ray1.Intersects(plane1)).first)
		std::cout << "Plane1(1,0,0),(0,0,0),(0,0,1) \n and Ray1 (Origin(0,2,0),direction(0,-1,0)) intersects!!!" << std::endl;

	system("PAUSE");

	/******** Sphere **********/
	std::cout << std::endl << std::endl << "/******** Sphere and AABB Test**********/" << std::endl << std::endl;

	Sphere sphere1(Vector3(1,0,1), 2);
	AxisAlignedBox aabb1(Vector3(0,0,0),Vector3(1,1,1));

	if ((ray1.Intersects(sphere1)).first)
		std::cout << "Sphere1 at(1,0,1) with radius 2 \n and Ray1 (Origin(0,2,0),direction(0,-1,0)) intersects!!!" << std::endl;
	if ((ray1.Intersects(aabb1)).first)
		std::cout << "AABB with (0,0,0) (1,1,1) \n and Ray1 (Origin(0,2,0),direction(0,-1,0)) intersects!!!" << std::endl;

	return 0;
}