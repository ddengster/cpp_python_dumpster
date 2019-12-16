
#ifndef _TRICK7_H
#define _TRICK7_H

/************
Infinite loop protection
************/
#include <assert.h>

static bool while_assert(bool a)
{
	assert(a && "while_limit: exceeded iteration limit");
	return (a);
}

//Unique var such that it will change when while_limit is called, so no compile-time errors
//#define UNIQUE_VAR(x) safety_limit ## x 
/* Why compiler error?
#define _while_limit(a,b,c) \ 
	assert(b>0 && "while_limit: limit is 0 or negative"); \
   int UNIQUE_VAR(c) = b; \
while(a && while_assert(--UNIQUE_VAR(c)>=0))
*/
//#define while_limit(a,b) _while_limit(a,b,__COUNTER__)
#define _UNIQUE_VAR(x) safety_limit ## x 
#define UNIQUE_VAR(x) _UNIQUE_VAR(x)

#define while_limit(a,b) \
	assert(b>0 && "while_limit: limit is 0 or negative"); \
    int UNIQUE_VAR(__LINE__) = b; \
	while(a && while_assert(--UNIQUE_VAR(__LINE__)>=0))

//__COUNTER__ : the number of times this line is called. Not standard ANSI C macro


void Trick7()
{
	while_limit(1, 10)
	{
	}
}
#endif
