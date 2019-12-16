
#include <iostream>
#include "Trick1.h"
#include "Trick2.h"
#include "Trick3and4_assert.h"
#include "Trick5.h"
#include "Trick6.h"
#include "Trick7.h"
#include "Trick9.h"

int main()
{
	STRPRINT(YO);
	Trick2();
	//Trick3();
	//Trick4();

	int myarray[5];
	int size = NumElements(myarray);
	std::cout << size << std::endl;

	Trick6();
	//Trick7();


	return 0;
}
