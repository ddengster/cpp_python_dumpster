
#ifndef _TRICK1_H
#define _TRICK1_H

#include <string>
/***************
Turn enums to strings.
***************/

#define STRPRINT(str) stringprint(#str) //therefore STRPRINT(MYMSG) will become stringprint("MYMSG")

void stringprint(std::string str)
{
	std::cout << str << std::endl;
}




#endif