
#ifndef _TRICK3_H
#define _TRICK3_H

/*****************
Custom assert with msg
*****************/
#include <assert.h>
#include <cassert>
#define assertmsg(a,b) assert(a && b)

void Trick3()
{
	assertmsg(0, "mymsg");
}

/*********************
Compile time assert with msg
*********************/

#define cassert(expn) typedef char __C_ASSERT__[(expn)?1:-1] //if expression is false, array size will be -1, thus
															//giving an error while compiling

void Trick4()
{
	//cassert(0);
}


#endif